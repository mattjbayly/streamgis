#' Extract Path Between Two Points on Stream Network
#'
#' @description Snaps two spatial points to the nearest streamline, finds the shortest path
#' between them on the stream network using `sfnetworks`, clips the streamline layer at the
#' point intersections, and extracts the line segments between the two points.
#'
#' @details
#' This function performs the following steps:
#'   1. Snaps `point_1` and `point_2` to the nearest streamlines
#'   2. Splits streamlines at the snapped point locations
#'   3. Creates a network from the split streamlines using `sfnetworks`
#'   4. Finds the shortest path between the snapped points on the network
#'   5. Extracts and returns the line segments forming the shortest path
#'
#' The function preserves original attributes from the `streamlines` attribute table
#' for all segments in the returned path.
#'
#' @param streamlines An `sf` LINESTRING/MULTILINESTRING object representing the stream network.
#' @param point_1 An `sf` POINT object (single feature).
#' @param point_2 An `sf` POINT object (single feature).
#'
#' @returns An `sf` LINESTRING object containing the stream segments forming the shortest path
#' between the two snapped points. Attributes from the original `streamlines` are preserved.
#'
#' @examples
#' \dontrun{
#' fname <- system.file("extdata", "/clip_points/clip_points.gpkg", package = "streamgis")
#' clip_points <- sf::st_read(fname)
#' fname <- system.file("extdata", "/clip_points/clip_lines.gpkg", package = "streamgis")
#' clip_lines <- sf::st_read(fname)
#'
#' clip_pts_target <- clip_points[clip_points$NAME == "PEP8", ]
#' point_1 <- clip_pts_target[1, ]
#' point_2 <- clip_pts_target[2, ]
#'
#' path <- extract_stream_path(clip_lines, point_1, point_2)
#' plot(sf::st_geometry(clip_lines))
#' plot(sf::st_geometry(path), col = "red", lwd = 2, add = TRUE)
#' }
#'
#' @export
#' @importFrom sf st_nearest_feature st_distance st_snap st_cast st_length
#' @importFrom sf st_geometry st_crs st_set_crs st_coordinates st_linestring
#' @importFrom sf st_sfc st_as_sf st_difference st_intersection st_union
#' @importFrom sfnetworks as_sfnetwork st_network_paths activate
#' @importFrom dplyr filter tibble

extract_stream_path <- function(streamlines, point_1, point_2) {

  # Input validation
  if (!inherits(streamlines, "sf")) {
    stop("`streamlines` must be an sf object (LINESTRING/MULTILINESTRING).")
  }
  if (!inherits(point_1, "sf") || nrow(point_1) != 1) {
    stop("`point_1` must be an sf object with exactly 1 feature.")
  }
  if (!inherits(point_2, "sf") || nrow(point_2) != 1) {
    stop("`point_2` must be an sf object with exactly 1 feature.")
  }
  if (nrow(streamlines) < 1) {
    stop("`streamlines` is empty.")
  }

  # Ensure same CRS
  if (!identical(sf::st_crs(streamlines), sf::st_crs(point_1))) {
    point_1 <- sf::st_transform(point_1, sf::st_crs(streamlines))
  }
  if (!identical(sf::st_crs(streamlines), sf::st_crs(point_2))) {
    point_2 <- sf::st_transform(point_2, sf::st_crs(streamlines))
  }

  # Cast to LINESTRING (handling MULTILINESTRING)
  streamlines_cast <- suppressWarnings(sf::st_cast(streamlines, "LINESTRING"))

  streamlines_cast$m___id <- 1:nrow(streamlines_cast)

  # Step 1: Snap points to nearest streamlines
  snapped_pt1 <- snap_point_to_streamline(streamlines_cast, point_1)
  snapped_pt2 <- snap_point_to_streamline(streamlines_cast, point_2)

  # Add length metadata for shortest path weighting
  streamlines_cast$length_m <- as.numeric(sf::st_length(streamlines_cast))

  # Add explicit edge ID to enable row tracking in network
  streamlines_cast$edge_id <- seq_len(nrow(streamlines_cast))

  # Step 3: Create network from split streamlines
  network <- create_stream_network(streamlines_cast)

  # Step 4: Find shortest path on network
  path_result <- find_shortest_path_network(network, streamlines_cast, snapped_pt1, snapped_pt2)

  # Get the connected segments - global
  ids_path <- streamlines_cast$m___id[path_result$edge_path]
  id_start <- snapped_pt1$original_line$m___id
  id_end <- snapped_pt2$original_line$m___id
  uids <- unique(c(id_start, ids_path, id_end))

  if(snapped_pt1$line_idx == snapped_pt2$line_idx) {
    uids <- snapped_pt2$original_line$m___id
  }

  # Add on the focal segments
  streamlines_cast <- streamlines_cast[streamlines_cast$m___id %in% uids, ]

  streamlines_cast$length_m <- NULL
  streamlines_cast$edge_id <- NULL
  streamlines_cast$m___id <- NULL

  # Path with no trimming
  if(FALSE) {
    plot(sf::st_geometry(streamlines), col = 'black')
    plot(sf::st_geometry(point_1), col = 'red', pch = 16, cex = 1, add = TRUE)
    plot(sf::st_geometry(snapped_pt1$original_line), col = 'red', add = TRUE)
    plot(sf::st_geometry(point_2), col = 'red', pch = 16, cex = 1, add = TRUE)
    plot(sf::st_geometry(snapped_pt2$original_line), col = 'red', add = TRUE)
    plot(sf::st_geometry(streamlines_cast), col = 'blue', lwd = 2, add = TRUE)
  }


  # =========================================================
  # Part2 - trim the path
  # =========================================================

  # Step 1: Re-Snap points to nearest streamlines
  snapped_pt1 <- snap_point_to_streamline(streamlines_cast, point_1)
  snapped_pt2 <- snap_point_to_streamline(streamlines_cast, point_2)

  # Step 2: Split streamlines at snapped point locations
  # This critical step ensures network nodes align with snap point locations
  streamlines_split <- split_streamlines_at_points(
    streamlines_cast,
    snapped_pt1,
    snapped_pt2
  )

  # If points are on one line then return single line
  if(length(snapped_pt1$line_idx) == 1 && length(snapped_pt2$line_idx) == 1) {
    if(snapped_pt1$line_idx == snapped_pt2$line_idx) {
      streamlines_split$length_m <- as.numeric(sf::st_length(streamlines_split))
      # Add explicit edge ID to enable row tracking in network
      streamlines_split$edge_id <- seq_len(nrow(streamlines_split))
      # move geometry column to the end
      streamlines_split <- streamlines_split[, c(setdiff(names(streamlines_split), "geometry"), "geometry")]
      # Return middle segment only
      return(streamlines_split)
    }
  }



  # plot(sf::st_geometry(streamlines), col = 'black')
  # plot(sf::st_geometry(point_1), col = 'red', pch = 16, cex = 1, add = TRUE)
  # plot(sf::st_geometry(point_2), col = 'red', pch = 16, cex = 1, add = TRUE)
  # plot(sf::st_geometry(streamlines_split), col = 'blue', lwd = 2, add = TRUE)

  # Add length metadata for shortest path weighting
  streamlines_split$length_m <- as.numeric(sf::st_length(streamlines_split))

  # Add explicit edge ID to enable row tracking in network
  streamlines_split$edge_id <- seq_len(nrow(streamlines_split))

  # test <- streamlines_split[streamlines_split$rid == snapped_pt2$original_line$rid, ]
  # plot(st_geometry(test), col = as.factor(test$edge_id))
  # plot(st_geometry(point_2), col = 'red', add = TRUE)
  # plot(st_geometry(snapped_pt2$point), col = 'purple', add = TRUE)

  # Step 3: Create network from split streamlines
  network <- create_stream_network(streamlines_split)

  # Step 4: Find shortest path on network
  # Returns edge indices that correspond to row numbers in streamlines_split
  path_result <- find_shortest_path_network(network, streamlines_split, snapped_pt1, snapped_pt2)

  # Step 5: Extract and clip path segments from split streamlines
  result <- extract_path_segments(
    streamlines_split,
    snapped_pt1,
    snapped_pt2,
    path_result
  )

  if(FALSE) {
    plot(sf::st_geometry(streamlines), col = 'black')
    plot(sf::st_geometry(streamlines_cast), col = 'purple', add = TRUE)
    plot(sf::st_geometry(streamlines_split), col = 'orange', add = TRUE)
    plot(sf::st_geometry(point_1), col = 'red', pch = 16, cex = 1, add = TRUE)
    plot(sf::st_geometry(point_2), col = 'red', pch = 16, cex = 1, add = TRUE)
    plot(sf::st_geometry(streamlines_split[path_result$edge_path, ]), col = 'pink', add = TRUE)
    plot(sf::st_geometry(result), col = 'blue', lwd = 2, add = TRUE)
  }


  return(result)
}


#' Helper: Snap a single point to nearest streamline
#'
#' @keywords internal
snap_point_to_streamline <- function(streamlines, point) {

  # streamlines <- streamlines_cast
  # point <- point_1

  if (!inherits(streamlines, "sf")) {
    stop("`streamlines` must be an sf object.")
  }
  if (!inherits(point, "sf") || nrow(point) != 1) {
    stop("`point` must be an sf object with exactly 1 feature.")
  }

  # 1) Find nearest streamline row index
  nearest_idx  <- sf::st_nearest_feature(point, streamlines)
  nearest_line <- streamlines[nearest_idx, ]

  # 2) Compute the shortest segment from point to line
  #    This returns a LINESTRING from the original point to the closest
  #    point on the line.
  seg <- sf::st_nearest_points(point, nearest_line)
  # seg is an sfc_LINESTRING of length 1

  # 3) Extract the coordinates of that segment
  seg_coords <- sf::st_coordinates(seg)
  # First row: original point
  # Second row: closest point on the line
  snapped_coords <- seg_coords[2, 1:2]

  # 4) Build a snapped POINT geometry at that closest location
  snapped_geom <- sf::st_sfc(
    sf::st_point(snapped_coords),
    crs = sf::st_crs(point)
  )

  # 5) Wrap as sf, preserving any attributes on the input point
  snapped <- sf::st_sf(
    sf::st_drop_geometry(point),
    geometry = snapped_geom
  )

  # 5) Extract coordinates
  snapped_coords <- sf::st_coordinates(snapped)[1,1:2]

  # plot(st_geometry(nearest_line), col = "lightblue")
  # plot(st_geometry(point), col = "red", add = TRUE)
  # plot(st_geometry(snapped), col = "purple", add = TRUE)


  # Return snapped point info with coordinates for clipping operations
  list(
    point = snapped,
    line_idx = nearest_idx,
    original_line = nearest_line,
    coords = snapped_coords
  )
}


#' Helper: Split streamlines at snapped point locations
#'
#' @keywords internal
split_streamlines_at_points <- function(streamlines_cast, snapped_pt1, snapped_pt2) {

  # Start with original streamlines
  result <- streamlines_cast

  # Get the line indices that need splitting
  idx1 <- snapped_pt1$line_idx
  idx2 <- snapped_pt2$line_idx

  # Split line 1 at snapped point 1
  line1 <- streamlines_cast[idx1, ]
  split_result1 <- split_line_at_point(line1, snapped_pt1$coords)

  # If split produced 2 segments, add the second one
  if (length(split_result1) > 1) {

    line_bg <- result[-idx1, ]

    if(nrow(line_bg) > 0) {
      # dealing with more than one line
      dist1 <- min(as.numeric(sf::st_distance(split_result1[[1]], line_bg)))
      dist2 <- min(as.numeric(sf::st_distance(split_result1[[2]], line_bg)))

      if(dist2 < dist1) {
        result[idx1, ] <- split_result1[[2]]
      } else {
        result[idx1, ] <- split_result1[[1]]
      }
    } else {
      # dealing with only one line
      dist1 <- min(as.numeric(sf::st_distance(split_result1[[1]], snapped_pt2$point)))
      dist2 <- min(as.numeric(sf::st_distance(split_result1[[2]], snapped_pt2$point)))

      if(dist2 < dist1) {
        result[idx1, ] <- split_result1[[2]]
      } else {
        result[idx1, ] <- split_result1[[1]]
      }

    }

  }

  # plot(st_geometry(streamlines_cast), col = "lightgrey")
  # plot(st_geometry(result[idx1, ]), add = TRUE)
  # plot(st_geometry(result), add = TRUE, col = "green")
  # plot(st_geometry(snapped_pt1$point), col = "red", add = TRUE)
  # plot(st_geometry(split_result1[[1]]), col = "orange", add = TRUE)
  # plot(st_geometry(snapped_pt2$point), col = "darkgreen", add = TRUE)

  # Ensure geometry column is named properly
  sf::st_geometry(result) <- "geometry"

  # Update idx2 if line 2 comes after line 1 and we added a row
  if (idx2 > idx1 && length(split_result1) > 1) {
    # idx2 <- idx2 + 1 # EDIT SKIP THIS...
  }

  # Handle line 2 splitting
  if (idx2 != idx1) {

    # Points are on different lines - split line 2 at snapped point 2
    line2 <- result[idx2, ]

    split_result2 <- split_line_at_point(line2, snapped_pt2$coords)

    # Determine which half to keep. Must be touching residual
    line_bg <- result[-idx2, ]

    # If split produced 2 segments, add the second one
    if (length(split_result2) > 1) {

      dist1 <- min(as.numeric(sf::st_distance(split_result2[[1]], line_bg)))
      dist2 <- min(as.numeric(sf::st_distance(split_result2[[2]], line_bg)))

      if(dist2 < dist1) {
        result[idx2, ] <- split_result2[[2]]
      } else {
        result[idx2, ] <- split_result2[[1]]
      }
    }

    # plot(st_geometry(streamlines_cast), col = "lightgrey")
    # plot(st_geometry(line2), add = TRUE)
    # plot(st_geometry(snapped_pt2$point), col = "red", add = TRUE)
    # plot(st_geometry(split_result2[[1]]), col = "orange", add = TRUE)
    # plot(st_geometry(split_result2[[2]]), col = "purple", add = TRUE)
    # plot(st_geometry(result), col = "red", add = TRUE)
    # plot(st_geometry(snapped_pt1$point), col = "red", add = TRUE)

  } else {

    # Splitting one line only
    # Points are on the same line - split at both points
    line <- result[idx1, ]

    split_result2 <- split_line_at_point(line, snapped_pt2$coords)

    # dealing with only one line
    dist1 <- min(as.numeric(sf::st_distance(split_result2[[1]], snapped_pt1$point)))
    dist2 <- min(as.numeric(sf::st_distance(split_result2[[2]], snapped_pt1$point)))

    if(dist2 < dist1) {
      result[idx1, ] <- split_result2[[2]]
    } else {
      result[idx1, ] <- split_result2[[1]]
    }

    # plot(st_geometry(streamlines_cast), col = "lightgrey")
    # plot(st_geometry(line), add = TRUE)
    # plot(st_geometry(snapped_pt2$point), col = "red", add = TRUE)
    # plot(st_geometry(split_result2[[1]]), col = "orange", add = TRUE)
    # plot(st_geometry(split_result2[[2]]), col = "purple", add = TRUE)
    # plot(st_geometry(snapped_pt1$point), col = "grey", pch = 19, add = TRUE)
    # plot(st_geometry(result), col = "yellow", pch = 19, add = TRUE)

  }

  return(result)
}


#' Helper: Split a single line at one point
#'
#' @keywords internal
split_line_at_point <- function(line, point_coords) {
  coords <- sf::st_coordinates(line)[, 1:2]

  if (nrow(coords) < 2) {
    return(list(line))
  }

  # Find closest segment to the point
  min_dist <- Inf
  min_idx <- 1
  for (i in 1:(nrow(coords) - 1)) {
    p1 <- coords[i, ]
    p2 <- coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      # Project point onto segment and calculate closest point
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist < min_dist) {
        min_dist <- dist
        min_idx <- i
      }
    }
  }

  # Create two segments: before split point and after split point
  coords1 <- coords[1:min_idx, , drop = FALSE]
  coords1 <- rbind(coords1, point_coords)

  coords2 <- rbind(point_coords, coords[(min_idx + 1):nrow(coords), , drop = FALSE])

  # Create sf objects preserving original attributes
  line1 <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(coords1), crs = sf::st_crs(line)),
    sf::st_drop_geometry(line)
  )

  line2 <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(coords2), crs = sf::st_crs(line)),
    sf::st_drop_geometry(line)
  )

  return(list(line1, line2))
}


#' Helper: Split a single line at two points
#'
#' @keywords internal
split_line_at_two_points <- function(line, pt1_coords, pt2_coords) {
  coords <- sf::st_coordinates(line)[, 1:2]

  if (nrow(coords) < 2) {
    return(list(line))
  }

  # Find positions of both points on the line
  positions <- list()
  for (pt_idx in 1:2) {
    pt_coords <- if (pt_idx == 1) pt1_coords else pt2_coords
    min_dist <- Inf
    min_seg_idx <- 1
    min_t <- 0

    for (i in 1:(nrow(coords) - 1)) {
      p1 <- coords[i, ]
      p2 <- coords[i + 1, ]
      seg_vec <- p2 - p1
      seg_len_sq <- sum(seg_vec^2)

      if (seg_len_sq > 0) {
        # Project point onto segment
        t <- max(0, min(1, sum((pt_coords - p1) * seg_vec) / seg_len_sq))
        proj_point <- p1 + t * seg_vec
        dist <- sqrt(sum((pt_coords - proj_point)^2))

        if (dist < min_dist) {
          min_dist <- dist
          min_seg_idx <- i
          min_t <- t
        }
      }
    }

    positions[[pt_idx]] <- list(seg_idx = min_seg_idx, t = min_t, coords = pt_coords)
  }

  # Ensure pt1 comes before pt2 along the line
  if (positions[[1]]$seg_idx > positions[[2]]$seg_idx ||
      (positions[[1]]$seg_idx == positions[[2]]$seg_idx &&
       positions[[1]]$t > positions[[2]]$t)) {
    positions <- list(positions[[2]], positions[[1]])
  }

  # Get original attributes to preserve through splits
  line_attrs <- sf::st_drop_geometry(line)

  segments <- list()

  # First segment (start to pt1)
  coords1 <- coords[1:positions[[1]]$seg_idx, , drop = FALSE]
  coords1 <- rbind(coords1, positions[[1]]$coords)
  segments[[1]] <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(coords1), crs = sf::st_crs(line)),
    line_attrs
  )

  # Middle segment (pt1 to pt2)
  # Include intermediate vertices between the two split points
  middle_coords <- if (positions[[1]]$seg_idx < positions[[2]]$seg_idx) {
    coords[(positions[[1]]$seg_idx + 1):positions[[2]]$seg_idx, , drop = FALSE]
  } else {
    coords[0, , drop = FALSE]  # Empty if on same segment
  }
  coords2 <- rbind(positions[[1]]$coords, middle_coords)
  coords2 <- rbind(coords2, positions[[2]]$coords)
  segments[[2]] <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(coords2), crs = sf::st_crs(line)),
    line_attrs
  )

  # Last segment (pt2 to end)
  last_coords <- if (positions[[2]]$seg_idx < nrow(coords)) {
    coords[(positions[[2]]$seg_idx + 1):nrow(coords), , drop = FALSE]
  } else {
    coords[0, , drop = FALSE]  # Empty if at end
  }
  coords3 <- rbind(positions[[2]]$coords, last_coords)
  segments[[3]] <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(coords3), crs = sf::st_crs(line)),
    line_attrs
  )

  return(segments)
}


#' Helper: Create sfnetwork from streamlines
#'
#' @keywords internal
create_stream_network <- function(streamlines) {
  # Create network
  network <- sfnetworks::as_sfnetwork(streamlines, directed = FALSE)

  return(network)
}


#' Helper: Find shortest path between two points on network
#' Returns edge indices that map to row numbers in streamlines
#'
#' @keywords internal
find_shortest_path_network <- function(network, streamlines, snapped_pt1, snapped_pt2) {

  # streamlines <- streamlines_split

  # 1) Get nodes from network
  nodes <- network %>% sfnetworks::activate("nodes") %>% sf::st_as_sf()


  # plot(st_geometry(streamlines))
  # plot(st_geometry(streamlines_split))
  # plot(st_geometry(nodes), add = TRUE, col = "red")

  # 2) Get the snapped points
  pt1 <- snapped_pt1$point
  pt2 <- snapped_pt2$point

  # 3) Find nearest nodes to snapped points
  node_idx_1 <- as.integer(sf::st_nearest_feature(pt1, nodes))
  node_idx_2 <- as.integer(sf::st_nearest_feature(pt2, nodes))

  # 4) Compute shortest path using edge length weights
  paths <- sfnetworks::st_network_paths(
    network,
    from    = node_idx_1,
    to      = node_idx_2,
    weights = "length_m"
  )

  # 5) Check if path exists
  if (nrow(paths) == 0 || length(paths$node_paths[[1]]) == 0) {
    stop("No path found between the two points on the network.")
  }

  # 6) Get edge indices from the path
  # These edge indices correspond directly to row numbers in the streamlines dataframe
  # because sfnetwork::as_sfnetwork() preserves the original row order
  edge_indices <- paths$edge_paths[[1]]

  # 7) Return list with edge path and node information
  list(
    node_path = paths$node_paths[[1]],
    edge_path = edge_indices,  # These are row indices in streamlines
    nodes = nodes
  )
}


#' Helper: Extract and combine path segments from streamlines_split
#'
#' @keywords internal
extract_path_segments <- function(streamlines_split, snapped_pt1, snapped_pt2, path_result) {

  # streamlines_split <- streamlines_cast

  # Extract edge indices from path result
  # These are row numbers in the streamlines_split dataframe
  edge_indices <- path_result$edge_path

  if (length(edge_indices) == 0) {
    stop("No segments extracted from the path.")
  }

  # Get snapped point coordinates
  pt1_coords <- snapped_pt1$coords
  pt2_coords <- snapped_pt2$coords

  # Select all segments in path order
  path_segments <- streamlines_split[edge_indices, , drop = FALSE]

  # Process each segment in the path
  all_segments <- list()

  for (i in seq_along(edge_indices)) {

    # Get the current segment
    segment <- path_segments[i, , drop = FALSE]

    # Determine segment position in path
    is_first <- (i == 1)
    is_last <- (i == length(edge_indices))

    # Apply appropriate clipping based on position
    if (is_first && is_last) {
      # Single segment path: extract portion between both points
      clipped <- clip_line_between_points(segment, pt1_coords, pt2_coords)
    } else if (is_first) {
      # First segment in multi-segment path: remove start, keep from pt1 onwards
      # clipped <- clip_line_from_start(segment, pt1_coords)
      clipped <- segment # already clipped
    } else if (is_last) {
      # Last segment in multi-segment path: remove end, keep up to pt2
      # clipped <- clip_line_to_end(segment, pt2_coords)
      clipped <- segment # already clipped
    } else {
      # Middle segments: keep entire geometry
      clipped <- segment
    }

    # Add clipped segment to list if valid
    if (!is.null(clipped) && nrow(clipped) > 0) {
      all_segments[[i]] <- clipped
    }
  }

  if (length(all_segments) == 0) {
    stop("No segments were successfully extracted from the path.")
  }

  # Combine all segments while preserving individual row attributes
  result <- do.call(rbind, all_segments)
  row.names(result) <- NULL

  return(result)
}


#' Helper: Clip line from snapped point onwards (removes start)
#'
#' @keywords internal
clip_line_from_start <- function(line, point_coords) {
  coords <- sf::st_coordinates(line)[, 1:2]

  if (nrow(coords) < 2) return(NULL)

  # Find the segment containing the snapped point
  min_dist <- Inf
  min_idx <- 1
  for (i in 1:(nrow(coords) - 1)) {
    p1 <- coords[i, ]
    p2 <- coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      # Project point onto segment
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist < min_dist) {
        min_dist <- dist
        min_idx <- i
      }
    }
  }

  # Keep from point onwards to the end of the line
  # Start with segment containing the point, then add remaining coordinates
  new_coords <- coords[min_idx:nrow(coords), , drop = FALSE]
  # Insert the snapped point as the first coordinate
  new_coords <- rbind(point_coords, new_coords[-1, , drop = FALSE])

  if (nrow(new_coords) < 2) return(NULL)

  # Create new sf object with original attributes preserved
  new_line <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(new_coords), crs = sf::st_crs(line)),
    sf::st_drop_geometry(line)
  )

  return(new_line)
}


#' Helper: Clip line up to snapped point (removes end)
#'
#' @keywords internal
clip_line_to_end <- function(line, point_coords) {

  # line <- segment
  # point_coords <- pt2_coords

  # plot(st_geometry(line), col = "blue")

  coords <- sf::st_coordinates(line)[, 1:2]

  if (nrow(coords) < 2) return(NULL)

  # Find the segment containing the snapped point
  min_dist <- Inf
  min_idx <- 1

  for (i in 1:(nrow(coords) - 1)) {
    p1 <- coords[i, ]
    p2 <- coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      # Project point onto segment
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist < min_dist) {
        min_dist <- dist
        min_idx <- i
      }
    }
  }

  # Keep from start up to and including the snapped point
  new_coords <- coords[1:min_idx, , drop = FALSE]
  # Append the snapped point as the last coordinate
  new_coords <- rbind(new_coords, point_coords)

  if (nrow(new_coords) < 2) return(NULL)

  # Create new sf object with original attributes preserved
  new_line <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(new_coords), crs = sf::st_crs(line)),
    sf::st_drop_geometry(line)
  )

   # st_length(new_line)

  return(new_line)
}


#' Helper: Clip line between two points
#'
#' @keywords internal
clip_line_between_points <- function(line, pt1_coords, pt2_coords) {
  coords <- sf::st_coordinates(line)[, 1:2]

  if (nrow(coords) < 2) return(NULL)

  # Find positions of both points on the line
  positions <- list()
  for (pt_idx in 1:2) {
    pt_coords <- if (pt_idx == 1) pt1_coords else pt2_coords
    min_dist <- Inf
    min_seg_idx <- 1
    min_t <- 0

    for (i in 1:(nrow(coords) - 1)) {
      p1 <- coords[i, ]
      p2 <- coords[i + 1, ]
      seg_vec <- p2 - p1
      seg_len_sq <- sum(seg_vec^2)

      if (seg_len_sq > 0) {
        # Project point onto segment
        t <- max(0, min(1, sum((pt_coords - p1) * seg_vec) / seg_len_sq))
        proj_point <- p1 + t * seg_vec
        dist <- sqrt(sum((pt_coords - proj_point)^2))

        if (dist < min_dist) {
          min_dist <- dist
          min_seg_idx <- i
          min_t <- t
        }
      }
    }

    positions[[pt_idx]] <- list(seg_idx = min_seg_idx, t = min_t, coords = pt_coords)
  }

  # Ensure pt1 comes before pt2 along the line
  if (positions[[1]]$seg_idx > positions[[2]]$seg_idx ||
      (positions[[1]]$seg_idx == positions[[2]]$seg_idx &&
       positions[[1]]$t > positions[[2]]$t)) {
    positions <- list(positions[[2]], positions[[1]])
  }

  # Extract coordinates between pt1 and pt2
  # Include intermediate vertices between the two points
  middle_coords <- if (positions[[1]]$seg_idx < positions[[2]]$seg_idx) {
    coords[(positions[[1]]$seg_idx + 1):positions[[2]]$seg_idx, , drop = FALSE]
  } else {
    coords[0, , drop = FALSE]  # Empty if on same segment
  }

  new_coords <- rbind(
    positions[[1]]$coords,
    middle_coords,
    positions[[2]]$coords
  )

  if (nrow(new_coords) < 2) return(NULL)

  # Create new sf object with original attributes preserved
  new_line <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_linestring(new_coords), crs = sf::st_crs(line)),
    sf::st_drop_geometry(line)
  )

  return(new_line)
}
