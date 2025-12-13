#' Split Lines at Point Locations
#'
#' @description Splits line features at locations where points intersect or snap
#' to them. Points are first snapped to the nearest line within a tolerance
#' distance, and then lines are split at those snapped locations. Points that
#' snap to line endpoints do not trigger a split.
#'
#' @details
#' This function performs the following steps:
#'   1. Casts MULTILINESTRING to LINESTRING (if needed)
#'   2. For each point, finds the nearest line within the snap tolerance
#'   3. Snaps the point to the nearest location on that line
#'   4. Checks if the snapped location is at a line endpoint (within tolerance)
#'   5. If not at an endpoint, splits the line at the snapped point location
#'   6. Returns the resulting lines with metadata tracking which were split
#'
#' If a point snaps to a location between two vertices (mid-segment), the line
#' is split at that exact location, inserting a new vertex at the split point.
#'
#' Original attributes from the input lines are preserved in all output segments.
#' Additional metadata columns are added to track the splitting operation.
#'
#' @param lines An `sf` object with LINESTRING or MULTILINESTRING geometry.
#' @param points An `sf` object with POINT or MULTIPOINT geometry representing
#'   the locations where lines should be split.
#' @param snap_tolerance Numeric. Maximum distance (in CRS units) for snapping
#'   points to lines. Points farther than this distance from any line will not
#'   cause a split. Default is 1.0.
#' @param endpoint_tolerance Numeric. Distance threshold (in CRS units) to
#'   consider a snapped point "at" a line endpoint. Points within this distance
#'   of an endpoint will not trigger a split. Default is 0.1.
#'
#' @returns A named list with two elements:
#'   \describe{
#'     \item{lines}{An `sf` LINESTRING object containing all line segments after
#'       splitting. Includes all original attributes plus:
#'       \itemize{
#'         \item \code{original_fid}: Integer linking back to the original feature row
#'         \item \code{was_split}: Logical flag indicating if this segment resulted
#'           from a split operation
#'       }}
#'     \item{points}{An `sf` POINT object containing the snapped point locations with:
#'       \itemize{
#'         \item All original point attributes
#'         \item \code{snapped}: Logical flag indicating if the point was snapped to a line
#'         \item \code{split_performed}: Logical flag indicating if a split occurred
#'         \item \code{at_endpoint}: Logical flag indicating if point snapped to an endpoint
#'         \item \code{snap_distance}: Numeric distance from original to snapped location
#'         \item \code{line_fid}: Integer ID of the line the point snapped to
#'       }}
#'   }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create example lines
#' line1 <- st_linestring(matrix(c(0,0, 10,0, 20,0), ncol=2, byrow=TRUE))
#' line2 <- st_linestring(matrix(c(10,0, 10,10), ncol=2, byrow=TRUE))
#' lines <- st_sf(id = 1:2, geometry = st_sfc(line1, line2, crs = 32610))
#'
#' # Create split points
#' pt1 <- st_point(c(5, 0.5))   # Near middle of line1
#' pt2 <- st_point(c(10, 5))    # Near middle of line2
#' pt3 <- st_point(c(0, 0))     # At endpoint of line1 (no split)
#' points <- st_sf(name = c("A", "B", "C"),
#'                 geometry = st_sfc(pt1, pt2, pt3, crs = 32610))
#'
#' # Split lines at points
#' result <- split_lines_at_points(lines, points, snap_tolerance = 1)
#'
#' # View results
#' plot(st_geometry(result$lines), col = rainbow(nrow(result$lines)))
#' plot(st_geometry(result$points), pch = 19, add = TRUE)
#' }
#'
#' @export
#' @importFrom sf st_cast st_crs st_geometry_type st_coordinates
#' @importFrom sf st_sfc st_sf st_point st_linestring st_drop_geometry st_length
#' @importFrom sf st_is_longlat st_nearest_points st_nearest_feature st_distance
#' @importFrom lwgeom st_startpoint st_endpoint

split_lines_at_points <- function(lines, points, snap_tolerance = 1.0, endpoint_tolerance = 0.1) {

  # ===== Input Validation =====
  if (!inherits(lines, "sf")) {
    stop("`lines` must be an sf object.")
  }

  if (!inherits(points, "sf")) {
    stop("`points` must be an sf object.")
  }

  line_geom_type <- as.character(sf::st_geometry_type(lines, by_geometry = FALSE))
  if (!line_geom_type %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRY")) {
    stop("`lines` must have LINESTRING or MULTILINESTRING geometry.")
  }

  point_geom_type <- as.character(sf::st_geometry_type(points, by_geometry = FALSE))
  if (!point_geom_type %in% c("POINT", "MULTIPOINT", "GEOMETRY")) {
    stop("`points` must have POINT or MULTIPOINT geometry.")
  }

  if (nrow(lines) < 1) {
    stop("`lines` is empty.")
  }

  if (nrow(points) < 1) {
    stop("`points` is empty.")
  }

  # Warn if using geographic coordinates
  if (sf::st_is_longlat(lines)) {
    warning("Input has geographic (lat/lon) coordinates. ",
            "Tolerances are in degrees, not meters. ",
            "Consider reprojecting to a projected CRS for accurate results.")
  }

  # Ensure same CRS
  if (!identical(sf::st_crs(lines), sf::st_crs(points))) {
    points <- sf::st_transform(points, sf::st_crs(lines))
    message("Transformed points to match lines CRS.")
  }

  # ===== Step 1: Preprocessing =====
  # Store original row numbers
  lines$original_fid <- seq_len(nrow(lines))

  # Cast to LINESTRING if needed
  if (line_geom_type == "MULTILINESTRING" || line_geom_type == "GEOMETRY") {
    lines <- suppressWarnings(sf::st_cast(lines, "LINESTRING"))
    message("Cast MULTILINESTRING to LINESTRING: ",
            nrow(lines), " features after casting.")
  }

  # Cast points to POINT if needed
  if (point_geom_type == "MULTIPOINT" || point_geom_type == "GEOMETRY") {
    points <- suppressWarnings(sf::st_cast(points, "POINT"))
    message("Cast MULTIPOINT to POINT: ",
            nrow(points), " features after casting.")
  }

  # Initialize tracking columns
  lines$was_split <- FALSE

  # Initialize point tracking
  points$snapped <- FALSE
  points$split_performed <- FALSE
  points$at_endpoint <- FALSE
  points$snap_distance <- NA_real_
  points$line_fid <- NA_integer_

  # Store original point geometries for comparison
  original_point_geoms <- sf::st_geometry(points)

  # ===== Step 2: Snap points and identify split locations =====
  line_crs <- sf::st_crs(lines)
  line_geoms <- sf::st_geometry(lines)
  n_lines <- nrow(lines)

  # Extract all startpoints and endpoints for endpoint checking
  startpoints <- lwgeom::st_startpoint(line_geoms)
  endpoints_geom <- lwgeom::st_endpoint(line_geoms)

  # Build list of split operations to perform
  # Structure: list of lists, one per line, containing split point coordinates

  split_operations <- vector("list", n_lines)
  for (i in seq_len(n_lines)) {
    split_operations[[i]] <- list()
  }

  # Process each point
  for (p_idx in seq_len(nrow(points))) {
    pt <- points[p_idx, ]
    pt_geom <- sf::st_geometry(pt)

    # Find nearest line
    nearest_idx <- sf::st_nearest_feature(pt, lines)

    # Calculate distance to nearest line
    dist_to_line <- as.numeric(sf::st_distance(pt, lines[nearest_idx, ]))

    # Check if within snap tolerance
    if (dist_to_line > snap_tolerance) {
      # Point too far from any line - skip
      next
    }

    # Snap point to line
    nearest_line <- lines[nearest_idx, ]
    seg <- sf::st_nearest_points(pt, nearest_line)
    seg_coords <- sf::st_coordinates(seg)
    snapped_coords <- seg_coords[2, 1:2]

    # Update point with snapped location
    snapped_geom <- sf::st_sfc(sf::st_point(snapped_coords), crs = line_crs)
    sf::st_geometry(points)[p_idx] <- snapped_geom

    # Record snap info
    points$snapped[p_idx] <- TRUE
    points$snap_distance[p_idx] <- dist_to_line
    points$line_fid[p_idx] <- lines$original_fid[nearest_idx]

    # Check if snapped point is at an endpoint
    start_coord <- sf::st_coordinates(startpoints[nearest_idx])[1, 1:2]
    end_coord <- sf::st_coordinates(endpoints_geom[nearest_idx])[1, 1:2]

    dist_to_start <- sqrt(sum((snapped_coords - start_coord)^2))
    dist_to_end <- sqrt(sum((snapped_coords - end_coord)^2))

    if (dist_to_start <= endpoint_tolerance || dist_to_end <= endpoint_tolerance) {
      # Point is at endpoint - no split needed
      points$at_endpoint[p_idx] <- TRUE
      next
    }

    # Point is interior - add to split operations for this line
    split_operations[[nearest_idx]] <- c(
      split_operations[[nearest_idx]],
      list(list(coords = snapped_coords, point_idx = p_idx))
    )
  }

  # ===== Step 3: Perform splits =====
  # Process lines and build result
  result_list <- vector("list", n_lines * 10)  # Estimate max size
  result_idx <- 0

  for (i in seq_len(n_lines)) {
    line <- lines[i, , drop = FALSE]
    ops <- split_operations[[i]]

    if (length(ops) == 0) {
      # No splits for this line - keep original
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- line
      next
    }

    # Extract all split point coordinates for this line
    split_coords_list <- lapply(ops, function(x) x$coords)
    point_indices <- sapply(ops, function(x) x$point_idx)

    # Sort points by position along line for consistent splitting
    line_coords <- sf::st_coordinates(line)[, 1:2]
    positions <- vapply(split_coords_list, function(pt_coords) {
      find_position_on_line(line_coords, pt_coords)
    }, numeric(1))

    sort_order <- order(positions)
    split_coords_list <- split_coords_list[sort_order]
    point_indices <- point_indices[sort_order]

    # Perform sequential splits
    current_segments <- list(line)
    line_attrs <- sf::st_drop_geometry(line)

    for (j in seq_along(split_coords_list)) {
      pt_coords <- split_coords_list[[j]]

      # Find which segment contains this point
      new_segments <- list()
      split_done <- FALSE

      for (seg in current_segments) {
        if (!split_done && point_on_line(sf::st_coordinates(seg)[, 1:2], pt_coords, endpoint_tolerance)) {
          # Split this segment
          split_result <- split_single_line_at_point(seg, pt_coords)

          if (length(split_result) > 1) {
            # Mark split success
            points$split_performed[point_indices[j]] <- TRUE

            # Add both split segments
            for (split_seg in split_result) {
              split_seg$was_split <- TRUE
              new_segments <- c(new_segments, list(split_seg))
            }
            split_done <- TRUE
          } else {
            new_segments <- c(new_segments, list(seg))
          }
        } else {
          new_segments <- c(new_segments, list(seg))
        }
      }

      current_segments <- new_segments
    }

    # Add all resulting segments to result list
    for (seg in current_segments) {
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- seg
    }
  }

  # ===== Step 4: Assemble Output =====
  # Remove NULL entries and combine
  result_list <- result_list[seq_len(result_idx)]
  result_lines <- do.call(rbind, result_list)
  row.names(result_lines) <- NULL

  # Ensure geometry column is named properly
  sf::st_geometry(result_lines) <- "geometry"

  # Reorder columns
  orig_cols <- setdiff(names(lines), c("original_fid", "was_split", "geometry"))
  col_order <- c(orig_cols, "original_fid", "was_split", "geometry")
  col_order <- col_order[col_order %in% names(result_lines)]
  result_lines <- result_lines[, col_order]

  n_split <- sum(result_lines$was_split)
  n_points_split <- sum(points$split_performed)

  message("Output: ", nrow(result_lines), " line segments (",
          n_split, " created by splitting at ", n_points_split, " points).")

  return(list(
    lines = result_lines,
    points = points
  ))
}


#' Helper: Find position along line (0-1 scale)
#'
#' @keywords internal
find_position_on_line <- function(line_coords, point_coords) {
  if (nrow(line_coords) < 2) return(0)

  # Calculate cumulative distances along line
  segment_lengths <- numeric(nrow(line_coords) - 1)
  for (i in 1:(nrow(line_coords) - 1)) {
    segment_lengths[i] <- sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
  }
  total_length <- sum(segment_lengths)
  if (total_length == 0) return(0)

  cumulative_dist <- c(0, cumsum(segment_lengths))

  # Find closest segment and position on it
  min_dist <- Inf
  position <- 0

  for (i in 1:(nrow(line_coords) - 1)) {
    p1 <- line_coords[i, ]
    p2 <- line_coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist < min_dist) {
        min_dist <- dist
        position <- (cumulative_dist[i] + t * segment_lengths[i]) / total_length
      }
    }
  }

  return(position)
}


#' Helper: Check if point is on line (not at endpoints)
#'
#' @keywords internal
point_on_line <- function(line_coords, point_coords, tolerance) {
  if (nrow(line_coords) < 2) return(FALSE)

  # Check if point is close to any segment of the line
  for (i in 1:(nrow(line_coords) - 1)) {
    p1 <- line_coords[i, ]
    p2 <- line_coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist <= tolerance) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}


#' Helper: Split a single line at one point
#'
#' @description Splits a line at a given point coordinate. If the point falls
#' between two vertices (mid-segment), the line is split at that exact location.
#'
#' @keywords internal
split_single_line_at_point <- function(line, point_coords) {
  coords <- sf::st_coordinates(line)[, 1:2]

  if (nrow(coords) < 2) {
    return(list(line))
  }

  # Find closest segment to the point
  min_dist <- Inf
  min_idx <- 1
  min_t <- 0

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
        min_t <- t
      }
    }
  }

  # Create two segments: before split point and after split point
  # First segment: from start to split point
  coords1 <- coords[1:min_idx, , drop = FALSE]
  coords1 <- rbind(coords1, point_coords)

  # Second segment: from split point to end
  coords2 <- rbind(point_coords, coords[(min_idx + 1):nrow(coords), , drop = FALSE])

  # Validate segments have at least 2 points
  if (nrow(coords1) < 2 || nrow(coords2) < 2) {
    return(list(line))
  }

  # Create sf objects preserving original attributes
  line_attrs <- sf::st_drop_geometry(line)
  line_crs <- sf::st_crs(line)

  line1 <- sf::st_sf(
    line_attrs,
    geometry = sf::st_sfc(sf::st_linestring(coords1), crs = line_crs)
  )

  line2 <- sf::st_sf(
    line_attrs,
    geometry = sf::st_sfc(sf::st_linestring(coords2), crs = line_crs)
  )

  return(list(line1, line2))
}
