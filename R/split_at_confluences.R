#' Split Stream Lines at Confluence Points
#'
#' @description Splits stream lines at their confluence points (where tributaries
#' join main stems), ensuring all line features terminate or start at confluence
#' nodes rather than passing through them.
#'
#' @details
#' This function performs the following steps:
#'   1. Casts MULTILINESTRING to LINESTRING (if needed)
#'   2. Optionally snaps line endpoints to nearby lines (if snap_tolerance > 0)
#'   3. Identifies confluence points where lines intersect
#'   4. Determines which lines pass through confluences (vs terminate at them)
#'   5. Splits those lines at the confluence points
#'   6. Returns both the split lines and confluence point locations
#'
#' The optional snapping step is useful for stream networks where tributary
#' endpoints don't exactly touch the mainstem due to digitization errors
#' (overshoots or undershoots). When `snap_tolerance` is set, endpoints that
#' are within this distance of another line will be moved to the nearest point
#' on that line.
#'
#' Original attributes from the input streamlines are preserved in all output
#' segments. Additional metadata columns are added to track the splitting operation.
#'
#' @param streamlines An `sf` object with LINESTRING or MULTILINESTRING geometry
#'   representing the stream network.
#' @param tolerance Numeric. Distance threshold (in CRS units) to consider a point
#'   "at" an endpoint versus "through" a line. Default is 0.1. For projected
#'   coordinates in meters, this means points within 0.1m of an endpoint are
#'   considered to be at the endpoint.
#' @param snap_tolerance Numeric or NULL. If provided and > 0, line endpoints
#'   that are within this distance of another line (but don't touch it) will be
#'   snapped to the nearest point on that line. This fixes common digitization
#'   errors where tributaries overshoot or undershoot the mainstem. Default is
#'   NULL (no snapping). Units are in CRS units (typically meters for projected
#'   coordinates).
#'
#' @returns A named list with two elements:
#'   \describe{
#'     \item{lines}{An `sf` LINESTRING object containing all stream segments after
#'       splitting. Includes all original attributes plus:
#'       \itemize{
#'         \item \code{original_fid}: Integer linking back to the original feature row
#'         \item \code{was_split}: Logical flag indicating if this segment resulted
#'           from a split operation
#'         \item \code{was_snapped}: Logical flag indicating if an endpoint was
#'           snapped (only present if snap_tolerance was used)
#'       }}
#'     \item{confluences}{An `sf` POINT object containing confluence locations with:
#'       \itemize{
#'         \item \code{confluence_id}: Unique integer identifier for each confluence
#'         \item \code{degree}: Number of line segments meeting at this confluence
#'       }}
#'   }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Load example stream network
#' fname <- system.file("extdata", "ifc_coho.gpkg", package = "streamgis")
#' streams <- st_read(fname)
#'
#' # Split at confluences with endpoint snapping (5m tolerance)
#' result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 5)
#' names(result)
#'
#' # View results
#' plot(st_geometry(result$lines), col = "blue")
#' plot(st_geometry(result$confluences), col = "red", pch = 19, add = TRUE)
#'
#' # Check which segments were snapped
#' if ("was_snapped" %in% names(result$lines)) {
#'   snapped_segs <- result$lines[result$lines$was_snapped, ]
#'   cat("Snapped", nrow(snapped_segs), "line endpoints\n")
#' }
#' }
#'
#' @export
#' @importFrom sf st_cast st_crs st_geometry_type st_intersection st_coordinates
#' @importFrom sf st_sfc st_sf st_point st_linestring st_drop_geometry st_length
#' @importFrom sf st_is_longlat st_nearest_points st_buffer st_intersects

split_at_confluences <- function(streamlines, tolerance = 0.1, snap_tolerance = NULL) {


  # ===== Input Validation =====
  if (!inherits(streamlines, "sf")) {
    stop("`streamlines` must be an sf object.")
  }

  geom_type <- as.character(sf::st_geometry_type(streamlines, by_geometry = FALSE))
  if (!geom_type %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRY")) {
    stop("`streamlines` must have LINESTRING or MULTILINESTRING geometry.")
  }

  if (nrow(streamlines) < 1) {
    stop("`streamlines` is empty.")
  }

  # Warn if using geographic coordinates
  if (sf::st_is_longlat(streamlines)) {
    warning("Input has geographic (lat/lon) coordinates. ",
            "Tolerance is in degrees, not meters. ",
            "Consider reprojecting to a projected CRS for accurate results.")
  }

  # ===== Step 1: Preprocessing =====
  # Store original row numbers before any casting
  streamlines$original_fid <- seq_len(nrow(streamlines))

  # Cast to LINESTRING if needed
  if (geom_type == "MULTILINESTRING" || geom_type == "GEOMETRY") {
    streamlines <- suppressWarnings(sf::st_cast(streamlines, "LINESTRING"))
    message("Cast MULTILINESTRING to LINESTRING: ",
            nrow(streamlines), " features after casting.")
  }

  # Add internal line ID for tracking through operations
  streamlines$.__line_id__ <- seq_len(nrow(streamlines))

  # Initialize was_split flag
  streamlines$was_split <- FALSE

  # ===== Step 1b: Optional Endpoint Snapping =====
  if (!is.null(snap_tolerance) && snap_tolerance > 0) {
    snap_result <- snap_endpoints_to_lines(streamlines, snap_tolerance)
    streamlines <- snap_result$lines
    n_snapped <- snap_result$n_snapped
    if (n_snapped > 0) {
      message("Snapped ", n_snapped, " line endpoints to nearby lines.")
    }
  }

  # ===== Step 2: Find Confluence Points =====
  confluences <- find_confluence_points(streamlines, tolerance)

  if (nrow(confluences) == 0) {
    message("No confluence points found. Returning original streamlines.")
    # Clean up internal column
    streamlines$.__line_id__ <- NULL
    return(list(
      lines = streamlines,
      confluences = sf::st_sf(
        confluence_id = integer(0),
        degree = integer(0),
        geometry = sf::st_sfc(crs = sf::st_crs(streamlines))
      )
    ))
  }

  message("Found ", nrow(confluences), " confluence points.")

  # ===== Step 3: Identify Lines Needing Splits =====
  lines_to_split <- identify_lines_needing_splits(streamlines, confluences, tolerance)

  if (length(lines_to_split) == 0) {
    message("No lines need splitting. All lines already terminate at confluences.")
    streamlines$.__line_id__ <- NULL
    return(list(
      lines = streamlines,
      confluences = confluences
    ))
  }

  message("Identified ", length(lines_to_split), " lines that need splitting.")

  # ===== Step 4: Split Lines at Confluence Points =====
  result_lines <- split_lines_at_confluences(streamlines, confluences, lines_to_split, tolerance)

  # ===== Step 5: Assemble Output =====
  # Clean up internal columns

  result_lines$.__line_id__ <- NULL

  # Ensure geometry column is at the end and named properly
  sf::st_geometry(result_lines) <- "geometry"

  # Reorder columns: original attributes, then tracking columns, then geometry
  orig_cols <- setdiff(names(streamlines), c(".__line_id__", "original_fid", "was_split", "geometry"))
  col_order <- c(orig_cols, "original_fid", "was_split", "geometry")
  col_order <- col_order[col_order %in% names(result_lines)]
  result_lines <- result_lines[, col_order]

  message("Output: ", nrow(result_lines), " line segments, ",
          sum(result_lines$was_split), " created by splitting.")

  return(list(
    lines = result_lines,
    confluences = confluences
  ))
}


#' Find Confluence Points Between Stream Lines
#'
#' @description Identifies points where stream lines intersect each other
#' (excluding self-intersections).
#'
#' @param streamlines An sf LINESTRING object with .__line_id__ column.
#' @param tolerance Distance tolerance for merging nearby confluence points.
#'
#' @returns An sf POINT object with confluence_id and degree columns.
#'
#' @keywords internal
find_confluence_points <- function(streamlines, tolerance) {

  # Vectorized approach: find all pairwise intersections at once
  geoms <- sf::st_geometry(streamlines)

  # Use st_intersects to build a sparse matrix of which lines touch
  ints_sparse <- sf::st_intersects(geoms, geoms)

  # Build unique pairs (i < j) that intersect - pre-allocate
  pair_list <- vector("list", length(ints_sparse))
  for (i in seq_along(ints_sparse)) {
    j_vals <- ints_sparse[[i]]
    j_vals <- j_vals[j_vals > i]
    if (length(j_vals) > 0) {
      pair_list[[i]] <- cbind(i = i, j = j_vals)
    }
  }
  pairs <- do.call(rbind, pair_list)

  if (is.null(pairs) || nrow(pairs) == 0) {
    return(sf::st_sf(
      confluence_id = integer(0),
      degree = integer(0),
      geometry = sf::st_sfc(crs = sf::st_crs(streamlines))
    ))
  }

  # Vectorized intersection for all pairs at once
  int_geoms <- sf::st_intersection(geoms[pairs[, "i"]], geoms[pairs[, "j"]])

  # Filter to only POINT and MULTIPOINT geometries
  geom_types <- sf::st_geometry_type(int_geoms)
  point_mask <- geom_types %in% c("POINT", "MULTIPOINT")

  if (!any(point_mask)) {
    return(sf::st_sf(
      confluence_id = integer(0),
      degree = integer(0),
      geometry = sf::st_sfc(crs = sf::st_crs(streamlines))
    ))
  }

  point_geoms <- int_geoms[point_mask]

  # Cast everything to POINT (handles MULTIPOINT by exploding)
  all_points_sfc <- suppressWarnings(
    sf::st_cast(sf::st_sfc(point_geoms, crs = sf::st_crs(streamlines)), "POINT")
  )

  if (length(all_points_sfc) == 0) {
    return(sf::st_sf(
      confluence_id = integer(0),
      degree = integer(0),
      geometry = sf::st_sfc(crs = sf::st_crs(streamlines))
    ))
  }

  # Remove duplicate/very close points (within tolerance) - vectorized version
  unique_points <- unique_points_within_tolerance(all_points_sfc, tolerance)

  # Calculate degree (number of lines meeting at each confluence) - vectorized
  degrees <- calculate_confluence_degrees(streamlines, unique_points, tolerance)

  # Create output sf object
  result <- sf::st_sf(
    confluence_id = seq_along(unique_points),
    degree = degrees,
    geometry = unique_points
  )

  return(result)
}


#' Remove Duplicate Points Within Tolerance
#'
#' @description Uses spatial clustering to efficiently remove duplicate points.
#' Points within tolerance distance are merged by taking the centroid of each cluster.
#'
#' @keywords internal
unique_points_within_tolerance <- function(points_sfc, tolerance) {
  if (length(points_sfc) == 0) return(points_sfc)
  if (length(points_sfc) == 1) return(points_sfc)

  # Use st_is_within_distance to find clusters (vectorized spatial index)
  within_dist <- sf::st_is_within_distance(points_sfc, points_sfc, dist = tolerance)

  # Build clusters using union-find approach
  n <- length(points_sfc)
  cluster_id <- seq_len(n)

  for (i in seq_len(n)) {
    neighbors <- within_dist[[i]]
    if (length(neighbors) > 1) {
      # Assign all neighbors to the minimum cluster id
      min_cluster <- min(cluster_id[neighbors])
      cluster_id[neighbors] <- min_cluster
    }
  }

  # Normalize cluster IDs (second pass to ensure transitivity)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_len(n)) {
      neighbors <- within_dist[[i]]
      min_cluster <- min(cluster_id[neighbors])
      if (any(cluster_id[neighbors] != min_cluster)) {
        cluster_id[neighbors] <- min_cluster
        changed <- TRUE
      }
    }
  }

  # Get unique clusters and compute centroids
  unique_clusters <- unique(cluster_id)
  coords_mat <- sf::st_coordinates(points_sfc)

  unique_points <- sf::st_sfc(
    lapply(unique_clusters, function(cid) {
      cluster_coords <- coords_mat[cluster_id == cid, 1:2, drop = FALSE]
      # Take centroid of cluster
      centroid <- colMeans(cluster_coords)
      sf::st_point(centroid)
    }),
    crs = sf::st_crs(points_sfc)
  )

  return(unique_points)
}


#' Calculate Confluence Degrees
#'
#' @description Counts how many line segments meet at each confluence point.
#' Uses buffered points and st_intersects for efficient spatial indexing.
#'
#' @keywords internal
calculate_confluence_degrees <- function(streamlines, confluence_points, tolerance) {
  # Buffer confluence points by tolerance and use st_intersects
  # This uses spatial indexing and is O(n log n) instead of O(n * m * s)
  buffered_points <- sf::st_buffer(confluence_points, dist = tolerance)

  # Find which lines intersect each buffered point
  intersects_sparse <- sf::st_intersects(buffered_points, sf::st_geometry(streamlines))

  # Degree is simply the count of intersecting lines for each confluence

  degrees <- lengths(intersects_sparse)

  return(degrees)
}


#' Identify Lines That Need Splitting at Confluences
#'
#' @description Finds lines where confluence points lie in the interior
#' (not at endpoints). Uses vectorized spatial operations.
#'
#' @keywords internal
identify_lines_needing_splits <- function(streamlines, confluences, tolerance) {
  if (nrow(confluences) == 0) return(integer(0))

  line_geoms <- sf::st_geometry(streamlines)
  conf_geoms <- sf::st_geometry(confluences)

  # Buffer confluences and find which lines they intersect
  buffered_conf <- sf::st_buffer(conf_geoms, dist = tolerance)
  line_conf_intersects <- sf::st_intersects(line_geoms, buffered_conf)

  # Extract all startpoints and endpoints at once
  startpoints <- lwgeom::st_startpoint(line_geoms)
  endpoints <- lwgeom::st_endpoint(line_geoms)

  # Pre-compute confluence coordinates matrix
  conf_coords <- sf::st_coordinates(conf_geoms)[, 1:2, drop = FALSE]

  # For each line, check if any touching confluence is NOT at an endpoint
 lines_to_split <- integer(0)

  for (i in seq_len(nrow(streamlines))) {
    touching_confs <- line_conf_intersects[[i]]
    if (length(touching_confs) == 0) next

    # Get this line's start and end coordinates
    start_coord <- sf::st_coordinates(startpoints[i])[1, 1:2]
    end_coord <- sf::st_coordinates(endpoints[i])[1, 1:2]

    # Check each touching confluence
    for (j in touching_confs) {
      pt <- conf_coords[j, ]

      # Calculate distance to endpoints
      dist_start <- sqrt(sum((pt - start_coord)^2))
      dist_end <- sqrt(sum((pt - end_coord)^2))

      if (dist_start > tolerance && dist_end > tolerance) {
        # This confluence is in the interior - line needs splitting
        lines_to_split <- c(lines_to_split, i)
        break
      }
    }
  }

  return(unique(lines_to_split))
}


#' Split Lines at Confluence Points
#'
#' @description Performs the actual splitting of lines at confluence points.
#' Uses lwgeom::st_split for efficient line splitting.
#'
#' @keywords internal
split_lines_at_confluences <- function(streamlines, confluences, lines_to_split, tolerance) {

  conf_geoms <- sf::st_geometry(confluences)
  line_geoms <- sf::st_geometry(streamlines)
  line_crs <- sf::st_crs(streamlines)

  # Pre-allocate result list
  n_lines <- nrow(streamlines)
  result_list <- vector("list", n_lines * 2)  # Estimate max size
  result_idx <- 0

  # Get column order from original (excluding geometry)
  geom_col <- attr(streamlines, "sf_column")
  attr_names <- setdiff(names(streamlines), geom_col)

  # Process lines that DON'T need splitting first (fast path)
  no_split_idx <- setdiff(seq_len(n_lines), lines_to_split)
  for (i in no_split_idx) {
    result_idx <- result_idx + 1
    result_list[[result_idx]] <- streamlines[i, , drop = FALSE]
  }

  # Process lines that need splitting
  for (i in lines_to_split) {
    line <- streamlines[i, , drop = FALSE]
    line_geom <- line_geoms[i]
    line_attrs <- sf::st_drop_geometry(line)

    # Find confluence points that are interior to this line
    interior_confs <- find_interior_confluences(line_geom, conf_geoms, tolerance)

    if (length(interior_confs) == 0) {
      # No interior points - keep original
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- line
      next
    }

    # Create blade geometry from interior confluence points
    conf_coords <- sf::st_coordinates(interior_confs)[, 1:2, drop = FALSE]
    blade <- sf::st_sfc(sf::st_multipoint(conf_coords), crs = line_crs)

    # Use lwgeom::st_split for efficient splitting
    split_result <- tryCatch({
      lwgeom::st_split(line_geom, blade)
    }, error = function(e) {
      # Fallback: return original if split fails
      NULL
    })

    if (is.null(split_result) || length(split_result) == 0) {
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- line
      next
    }

    # Extract individual linestrings from the split result
    split_geoms <- sf::st_collection_extract(split_result, "LINESTRING")

    if (length(split_geoms) <= 1) {
      # No actual split occurred
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- line
      next
    }

    # Create sf objects for each split segment with matching structure
    for (j in seq_along(split_geoms)) {
      seg_geom <- sf::st_sfc(split_geoms[[j]], crs = line_crs)
      # Create a copy of attributes and set was_split
      seg_attrs <- line_attrs
      seg_attrs$was_split <- TRUE
      # Build sf object - use same geometry column name as original
      seg_attrs[[geom_col]] <- seg_geom
      seg <- sf::st_as_sf(seg_attrs)
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- seg
    }
  }

  # Remove NULL entries and combine
  result_list <- result_list[seq_len(result_idx)]

  # Use dplyr::bind_rows for more robust binding that handles column differences
  result <- do.call(rbind, result_list)
  row.names(result) <- NULL

  return(result)
}


#' Find Interior Confluence Points for a Line
#'
#' @description Finds confluence points that lie in the interior of a line
#' (not at endpoints). Uses vectorized spatial operations.
#'
#' @keywords internal
find_interior_confluences <- function(line_geom, conf_geoms, tolerance) {
  # Buffer line slightly and find which confluences touch it
  line_buffer <- sf::st_buffer(line_geom, dist = tolerance)
  touches <- sf::st_intersects(line_buffer, conf_geoms)[[1]]

  if (length(touches) == 0) return(sf::st_sfc(crs = sf::st_crs(line_geom)))

  # Get line endpoints
  start_pt <- lwgeom::st_startpoint(line_geom)
  end_pt <- lwgeom::st_endpoint(line_geom)

  # Filter to only interior points
  interior_idx <- vapply(touches, function(j) {
    conf_pt <- conf_geoms[j]
    dist_start <- as.numeric(sf::st_distance(conf_pt, start_pt))
    dist_end <- as.numeric(sf::st_distance(conf_pt, end_pt))
    dist_start > tolerance && dist_end > tolerance
  }, logical(1))

  interior_confs <- conf_geoms[touches[interior_idx]]

  return(interior_confs)
}


#' Snap Line Endpoints to Nearby Lines
#'
#' @description Snaps line endpoints that are close to (but don't touch) other
#' lines to the nearest point on those lines. This fixes common digitization
#' errors where tributaries overshoot or undershoot the mainstem.
#'
#' @param streamlines An sf LINESTRING object.
#' @param snap_tolerance Maximum distance for snapping (in CRS units).
#'
#' @returns A list with:
#'   \describe{
#'     \item{lines}{The streamlines with snapped endpoints}
#'     \item{n_snapped}{Number of endpoints that were snapped}
#'   }
#'
#' @keywords internal
snap_endpoints_to_lines <- function(streamlines, snap_tolerance) {

  n_lines <- nrow(streamlines)
  line_geoms <- sf::st_geometry(streamlines)
  line_crs <- sf::st_crs(streamlines)

  # Extract all startpoints and endpoints
  startpoints <- lwgeom::st_startpoint(line_geoms)
  endpoints <- lwgeom::st_endpoint(line_geoms)

  # Initialize tracking
  streamlines$was_snapped <- FALSE
  n_snapped <- 0

  # Buffer all lines for proximity detection
  line_buffers <- sf::st_buffer(line_geoms, dist = snap_tolerance)

  # For each line, check if its endpoints need snapping
  for (i in seq_len(n_lines)) {
    line_coords <- sf::st_coordinates(line_geoms[[i]])[, 1:2, drop = FALSE]
    start_pt <- startpoints[i]
    end_pt <- endpoints[i]
    modified <- FALSE

    # Find lines whose buffers contain this line's startpoint (excluding self)
    start_in_buffer <- sf::st_intersects(start_pt, line_buffers)[[1]]
    start_in_buffer <- start_in_buffer[start_in_buffer != i]

    # Check if startpoint actually touches any line (if so, no snapping needed)
    if (length(start_in_buffer) > 0) {
      start_touches <- sf::st_intersects(start_pt, line_geoms[start_in_buffer])[[1]]

      if (length(start_touches) == 0) {
        # Startpoint is near but doesn't touch - find nearest point to snap to
        candidate_lines <- line_geoms[start_in_buffer]
        nearest <- sf::st_nearest_points(start_pt, sf::st_combine(candidate_lines))
        # st_nearest_points returns a line from start_pt to nearest point
        nearest_coords <- sf::st_coordinates(nearest)[, 1:2, drop = FALSE]
        snap_point <- nearest_coords[2, ]  # Second point is on the target line

        # Verify distance is within tolerance
        dist <- sqrt(sum((sf::st_coordinates(start_pt)[1, 1:2] - snap_point)^2))
        if (dist <= snap_tolerance && dist > 0) {
          # Update the first coordinate
          line_coords[1, ] <- snap_point
          modified <- TRUE
          n_snapped <- n_snapped + 1
        }
      }
    }

    # Find lines whose buffers contain this line's endpoint (excluding self)
    end_in_buffer <- sf::st_intersects(end_pt, line_buffers)[[1]]
    end_in_buffer <- end_in_buffer[end_in_buffer != i]

    # Check if endpoint actually touches any line
    if (length(end_in_buffer) > 0) {
      end_touches <- sf::st_intersects(end_pt, line_geoms[end_in_buffer])[[1]]

      if (length(end_touches) == 0) {
        # Endpoint is near but doesn't touch - find nearest point to snap to
        candidate_lines <- line_geoms[end_in_buffer]
        nearest <- sf::st_nearest_points(end_pt, sf::st_combine(candidate_lines))
        nearest_coords <- sf::st_coordinates(nearest)[, 1:2, drop = FALSE]
        snap_point <- nearest_coords[2, ]

        # Verify distance is within tolerance
        dist <- sqrt(sum((sf::st_coordinates(end_pt)[1, 1:2] - snap_point)^2))
        if (dist <= snap_tolerance && dist > 0) {
          # Update the last coordinate
          line_coords[nrow(line_coords), ] <- snap_point
          modified <- TRUE
          n_snapped <- n_snapped + 1
        }
      }
    }

    # If modified, update the geometry
    if (modified) {
      new_geom <- sf::st_linestring(line_coords)
      line_geoms[[i]] <- new_geom
      streamlines$was_snapped[i] <- TRUE
    }
  }

  # Update geometries in streamlines
  sf::st_geometry(streamlines) <- sf::st_sfc(line_geoms, crs = line_crs)

  return(list(lines = streamlines, n_snapped = n_snapped))
}
