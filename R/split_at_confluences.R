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
#' **Performance mode (`vertices_only`):**
#' For large datasets where lines only intersect at existing vertices (not mid-segment),
#' set `vertices_only = TRUE` for significantly faster processing. This mode:
#' - Only detects confluences at line endpoints and existing vertices
#' - Skips expensive segment-to-segment intersection detection
#' - Skips self-intersection and near-return detection
#' - Is appropriate when lines are already properly noded (vertices exist at all intersections)
#'
#' Use `vertices_only = FALSE` (default) when lines may cross mid-segment without
#' a vertex at the crossing point, or when lines may self-intersect.
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
#' @param vertices_only Logical. If TRUE, only detect confluences at existing
#'   line vertices (endpoints and interior vertices). This is much faster for
#'   large datasets but will miss intersections where lines cross mid-segment
#'   without a vertex. Default is FALSE (full segment intersection detection).
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
#' # For large datasets with properly noded lines (faster processing)
#' result_fast <- split_at_confluences(streams, tolerance = 0.1, vertices_only = TRUE)
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

split_at_confluences <- function(streamlines, tolerance = 0.1, snap_tolerance = NULL,
                                  vertices_only = FALSE) {


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
  confluences <- find_confluence_points(streamlines, tolerance, vertices_only)

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
#' @description Identifies points where stream lines intersect each other,
#' including self-intersections within individual lines and near-touching
#' endpoints that may be missed due to floating-point precision issues.
#'
#' @param streamlines An sf LINESTRING object with .__line_id__ column.
#' @param tolerance Distance tolerance for merging nearby confluence points.
#' @param vertices_only Logical. If TRUE, only detect confluences at existing
#'   vertices (fast mode). If FALSE, also detect mid-segment intersections.
#'
#' @returns An sf POINT object with confluence_id and degree columns.
#'
#' @keywords internal
find_confluence_points <- function(streamlines, tolerance, vertices_only = FALSE) {

  geoms <- sf::st_geometry(streamlines)
  line_crs <- sf::st_crs(streamlines)
  all_point_geoms <- list()

  if (vertices_only) {
    # ===== FAST MODE: Only detect confluences at existing vertices =====
    # This is much faster for large datasets where lines are already properly noded
    vertex_points <- find_vertex_confluences(geoms, tolerance, line_crs)
    if (length(vertex_points) > 0) {
      all_point_geoms <- c(all_point_geoms, list(vertex_points))
    }
  } else {
    # ===== FULL MODE: Detect all intersections including mid-segment =====

    # ===== Part 1: Find pairwise intersections between different lines =====
    # Use st_intersects to build a sparse matrix of which lines touch
    ints_sparse <- sf::st_intersects(geoms, geoms)

    # Build unique pairs (i < j) that intersect
    pair_list <- vector("list", length(ints_sparse))
    for (i in seq_along(ints_sparse)) {
      j_vals <- ints_sparse[[i]]
      j_vals <- j_vals[j_vals > i]
      if (length(j_vals) > 0) {
        pair_list[[i]] <- cbind(i = i, j = j_vals)
      }
    }
    pairs <- do.call(rbind, pair_list)

    if (!is.null(pairs) && nrow(pairs) > 0) {
      # Vectorized intersection for all pairs at once
      int_geoms <- sf::st_intersection(geoms[pairs[, "i"]], geoms[pairs[, "j"]])

      # Filter to only POINT and MULTIPOINT geometries
      geom_types <- sf::st_geometry_type(int_geoms)
      point_mask <- geom_types %in% c("POINT", "MULTIPOINT")

      if (any(point_mask)) {
        point_geoms <- int_geoms[point_mask]
        # Cast to individual points
        pairwise_points <- suppressWarnings(
          sf::st_cast(sf::st_sfc(point_geoms, crs = line_crs), "POINT")
        )
        if (length(pairwise_points) > 0) {
          all_point_geoms <- c(all_point_geoms, list(pairwise_points))
        }
      }
    }

    # ===== Part 2: Find self-intersection points within each line =====
    self_int_points <- find_self_intersections(geoms, line_crs)
    if (length(self_int_points) > 0) {
      all_point_geoms <- c(all_point_geoms, list(self_int_points))
    }

    # ===== Part 3: Find near-touching endpoints (precision issues) =====
    near_touch_points <- find_near_touching_endpoints(geoms, tolerance, line_crs)
    if (length(near_touch_points) > 0) {
      all_point_geoms <- c(all_point_geoms, list(near_touch_points))
    }
  }

  # ===== Combine all points =====
  if (length(all_point_geoms) == 0) {
    return(sf::st_sf(
      confluence_id = integer(0),
      degree = integer(0),
      geometry = sf::st_sfc(crs = line_crs)
    ))
  }

  all_points_sfc <- do.call(c, all_point_geoms)

  if (length(all_points_sfc) == 0) {
    return(sf::st_sf(
      confluence_id = integer(0),
      degree = integer(0),
      geometry = sf::st_sfc(crs = line_crs)
    ))
  }

  # Remove duplicate/very close points
  # Use a smaller merging tolerance (10% of user tolerance or 0.1m, whichever is smaller)
  # This only merges true duplicates from multiple detection methods, not distinct nearby points
  merge_tolerance <- min(tolerance * 0.1, 0.1)
  unique_points <- unique_points_within_tolerance(all_points_sfc, merge_tolerance)

  # Keep all confluence points including:
  # - Points where 2+ lines meet (endpoint-to-endpoint or crossing)
  # - Self-intersection points (where a single line crosses itself)
  # - Near-return points (where a line passes close to its own start/end)
  # Lines that pass through confluences will be split; lines that terminate at
  # confluences will not be split but the confluence is still recorded

  if (length(unique_points) == 0) {
    return(sf::st_sf(
      confluence_id = integer(0),
      degree = integer(0),
      geometry = sf::st_sfc(crs = line_crs)
    ))
  }

  # Calculate degree - for true confluences (2+ lines meeting) this is the line count
  # For self-intersections, we count the same line twice (it crosses itself)
  degrees <- calculate_confluence_degrees_with_self_int(streamlines, unique_points, tolerance)

  # Create output sf object
  result <- sf::st_sf(
    confluence_id = seq_along(unique_points),
    degree = degrees,
    geometry = unique_points
  )

  return(result)
}


#' Find Self-Intersection Points Within Lines
#'
#' @description Identifies points where a line crosses itself or passes very
#' close to its own start/end points (near-returns).
#'
#' @param geoms An sfc_LINESTRING object.
#' @param crs The CRS of the geometries.
#' @param near_return_tolerance Distance tolerance for detecting near-returns
#'   (where line passes close to its own endpoint). Default is 1.0.
#'
#' @returns An sfc_POINT object with self-intersection points.
#'
#' @keywords internal
find_self_intersections <- function(geoms, crs, near_return_tolerance = 1.0) {
  self_int_list <- list()
  n_geoms <- length(geoms)

  # Quick check: identify which lines are non-simple (self-intersecting)
  # This is O(n) and filters out most lines quickly
  is_simple <- vapply(seq_along(geoms), function(i) {
    sf::st_is_simple(geoms[i])
  }, logical(1))

  non_simple_idx <- which(!is_simple)

  # Part 1: Process self-intersecting lines
  for (i in non_simple_idx) {
    line_geom <- geoms[i]
    coords <- sf::st_coordinates(line_geom)[, 1:2, drop = FALSE]
    n_coords <- nrow(coords)

    if (n_coords < 4) next

    # Check segment pairs for intersections
    # Optimization: only check non-adjacent segments
    for (si in 1:(n_coords - 3)) {
      seg_i <- sf::st_sfc(
        sf::st_linestring(coords[si:(si + 1), , drop = FALSE]),
        crs = crs
      )

      for (sj in (si + 2):(n_coords - 1)) {
        seg_j <- sf::st_sfc(
          sf::st_linestring(coords[sj:(sj + 1), , drop = FALSE]),
          crs = crs
        )

        int_result <- sf::st_intersection(seg_i, seg_j)

        if (length(int_result) > 0 && !sf::st_is_empty(int_result)) {
          int_type <- sf::st_geometry_type(int_result)
          if (int_type == "POINT") {
            self_int_list <- c(self_int_list, list(int_result))
          } else if (int_type == "MULTIPOINT") {
            pts <- suppressWarnings(sf::st_cast(int_result, "POINT"))
            self_int_list <- c(self_int_list, list(pts))
          }
        }
      }
    }
  }

  # Part 2: Check for near-returns (segments passing close to endpoints)
  # Only check lines with enough vertices
  for (i in seq_along(geoms)) {
    coords <- sf::st_coordinates(geoms[i])[, 1:2, drop = FALSE]
    n_coords <- nrow(coords)

    if (n_coords < 4) next  # Need at least 4 vertices for interior segments

    start_pt <- coords[1, ]
    end_pt <- coords[n_coords, ]
    near_return_tol_sq <- near_return_tolerance^2

    # Check each non-endpoint segment for proximity to start/end points
    for (si in 2:(n_coords - 2)) {
      p1 <- coords[si, ]
      p2 <- coords[si + 1, ]
      seg_vec <- p2 - p1
      seg_len_sq <- sum(seg_vec^2)

      if (seg_len_sq > 0) {
        # Distance from start point to this segment (using squared distance)
        t_start <- max(0, min(1, sum((start_pt - p1) * seg_vec) / seg_len_sq))
        proj_start <- p1 + t_start * seg_vec
        dist_sq_to_start <- sum((start_pt - proj_start)^2)

        if (dist_sq_to_start < near_return_tol_sq && dist_sq_to_start > 0) {
          near_pt <- sf::st_sfc(sf::st_point(start_pt), crs = crs)
          self_int_list <- c(self_int_list, list(near_pt))
          break  # Found near-return to start, no need to check more segments
        }

        # Distance from end point to this segment
        t_end <- max(0, min(1, sum((end_pt - p1) * seg_vec) / seg_len_sq))
        proj_end <- p1 + t_end * seg_vec
        dist_sq_to_end <- sum((end_pt - proj_end)^2)

        if (dist_sq_to_end < near_return_tol_sq && dist_sq_to_end > 0) {
          near_pt <- sf::st_sfc(sf::st_point(end_pt), crs = crs)
          self_int_list <- c(self_int_list, list(near_pt))
          break  # Found near-return to end, no need to check more segments
        }
      }
    }
  }

  if (length(self_int_list) == 0) {
    return(sf::st_sfc(crs = crs))
  }

  # Combine all self-intersection points
  result <- do.call(c, self_int_list)
  return(result)
}


#' Find Near-Touching Endpoints
#'
#' @description Identifies points where line endpoints are very close to
#' another line but don't exactly touch due to floating-point precision.
#' Uses a very small precision tolerance (0.01 units) to only catch true
#' precision issues, not general near-misses.
#'
#' @param geoms An sfc_LINESTRING object.
#' @param tolerance Distance tolerance (used for cluster merging, not for
#'   finding near-touches).
#' @param crs The CRS of the geometries.
#'
#' @returns An sfc_POINT object with near-touching endpoint locations.
#'
#' @keywords internal
find_near_touching_endpoints <- function(geoms, tolerance, crs) {
  near_touch_list <- list()
  n_lines <- length(geoms)

  # Use a very small precision tolerance for floating-point issues only
  # This should catch true precision issues (1e-6 to 0.01 m), not real gaps
  precision_tolerance <- min(tolerance, 0.01)

  # Extract all endpoints
  startpoints <- lwgeom::st_startpoint(geoms)
  endpoints <- lwgeom::st_endpoint(geoms)

  for (i in seq_len(n_lines)) {
    start_pt <- startpoints[i]
    end_pt <- endpoints[i]

    for (j in seq_len(n_lines)) {
      if (i == j) next

      line_j <- geoms[j]

      # Check if startpoint is near line j but doesn't exactly intersect
      dist_start <- as.numeric(sf::st_distance(start_pt, line_j))
      if (dist_start <= precision_tolerance && dist_start > 0) {
        # It's close but not touching - find the nearest point on line j
        nearest_seg <- sf::st_nearest_points(start_pt, line_j)
        nearest_coords <- sf::st_coordinates(nearest_seg)
        if (nrow(nearest_coords) >= 2) {
          snap_pt <- sf::st_sfc(sf::st_point(nearest_coords[2, 1:2]), crs = crs)
          near_touch_list <- c(near_touch_list, list(snap_pt))
        }
      }

      # Check if endpoint is near line j but doesn't exactly intersect
      dist_end <- as.numeric(sf::st_distance(end_pt, line_j))
      if (dist_end <= precision_tolerance && dist_end > 0) {
        nearest_seg <- sf::st_nearest_points(end_pt, line_j)
        nearest_coords <- sf::st_coordinates(nearest_seg)
        if (nrow(nearest_coords) >= 2) {
          snap_pt <- sf::st_sfc(sf::st_point(nearest_coords[2, 1:2]), crs = crs)
          near_touch_list <- c(near_touch_list, list(snap_pt))
        }
      }
    }
  }

  if (length(near_touch_list) == 0) {
    return(sf::st_sfc(crs = crs))
  }

  result <- do.call(c, near_touch_list)
  return(result)
}


#' Find Vertex-Based Confluences (Fast Mode)
#'
#' @description Identifies confluence points by only examining existing vertices
#' (endpoints and interior vertices) of all lines. This is much faster than
#' full segment intersection detection but will miss intersections where lines
#' cross mid-segment without a vertex at the crossing point.
#'
#' This mode is appropriate for datasets where lines are already properly noded
#' (vertices exist at all intersection points).
#'
#' @param geoms An sfc_LINESTRING object.
#' @param tolerance Distance tolerance for considering vertices as coincident.
#' @param crs The CRS of the geometries.
#'
#' @returns An sfc_POINT object with confluence points.
#'
#' @keywords internal
find_vertex_confluences <- function(geoms, tolerance, crs) {
  n_lines <- length(geoms)
  if (n_lines == 0) return(sf::st_sfc(crs = crs))

  # Extract all vertices from all lines with their line IDs
  all_coords <- list()
  line_ids <- list()

  for (i in seq_len(n_lines)) {
    coords <- sf::st_coordinates(geoms[i])[, 1:2, drop = FALSE]
    all_coords[[i]] <- coords
    line_ids[[i]] <- rep(i, nrow(coords))
  }

  # Combine all coordinates
  coords_mat <- do.call(rbind, all_coords)
  line_id_vec <- unlist(line_ids)

  if (nrow(coords_mat) == 0) return(sf::st_sfc(crs = crs))

  # Convert to points for spatial indexing
  all_points <- sf::st_sfc(
    lapply(seq_len(nrow(coords_mat)), function(i) {
      sf::st_point(coords_mat[i, ])
    }),
    crs = crs
  )

  # Find points that are within tolerance of each other
  within_dist <- sf::st_is_within_distance(all_points, all_points, dist = tolerance)

  # Find vertices that are shared between different lines
  # A confluence is where 2+ different lines have vertices within tolerance
  confluence_points <- list()
  processed <- logical(length(all_points))

  for (i in seq_along(within_dist)) {
    if (processed[i]) next

    neighbors <- within_dist[[i]]
    neighbor_lines <- unique(line_id_vec[neighbors])

    if (length(neighbor_lines) >= 2) {
      # This is a confluence - multiple lines share this vertex location
      # Use centroid of all nearby vertices as the confluence point
      neighbor_coords <- coords_mat[neighbors, , drop = FALSE]
      centroid <- colMeans(neighbor_coords)
      confluence_points <- c(confluence_points, list(sf::st_point(centroid)))
      processed[neighbors] <- TRUE
    }
  }

  if (length(confluence_points) == 0) {
    return(sf::st_sfc(crs = crs))
  }

  result <- sf::st_sfc(confluence_points, crs = crs)
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


#' Check Which Confluences Need Splitting
#'
#' @description Determines which confluence points require line splitting.
#' A confluence needs splitting if at least one line passes through it
#' (i.e., the point is interior to at least one line, not just at endpoints).
#' Confluences where all touching lines have the point at their endpoints
#' are filtered out - these are just endpoint connections, not intersections.
#'
#' Special case: for self-intersecting or near-return lines, a point at an
#' endpoint may still need splitting if another segment of the same line
#' passes close to that point.
#'
#' @param streamlines An sf LINESTRING object.
#' @param confluence_points An sfc_POINT object with candidate confluences.
#' @param tolerance Distance tolerance for point-to-endpoint matching.
#'
#' @returns Logical vector indicating which confluences need splitting.
#'
#' @keywords internal
check_confluences_need_split <- function(streamlines, confluence_points, tolerance) {
  if (length(confluence_points) == 0) return(logical(0))

  line_geoms <- sf::st_geometry(streamlines)
  line_crs <- sf::st_crs(streamlines)

  # Extract all startpoints and endpoints
  startpoints <- lwgeom::st_startpoint(line_geoms)
  endpoints <- lwgeom::st_endpoint(line_geoms)

  # Buffer confluence points to find which lines they touch
  buffered_conf <- sf::st_buffer(confluence_points, dist = tolerance)
  conf_line_intersects <- sf::st_intersects(buffered_conf, line_geoms)

  # Get confluence coordinates
  conf_coords <- sf::st_coordinates(confluence_points)[, 1:2, drop = FALSE]

  needs_split <- logical(length(confluence_points))

  for (ci in seq_along(confluence_points)) {
    pt <- conf_coords[ci, ]
    touching_lines <- conf_line_intersects[[ci]]

    if (length(touching_lines) == 0) {
      needs_split[ci] <- FALSE
      next
    }

    # Check if this point is interior to ANY touching line
    is_interior_to_any <- FALSE

    for (li in touching_lines) {
      start_coord <- sf::st_coordinates(startpoints[li])[1, 1:2]
      end_coord <- sf::st_coordinates(endpoints[li])[1, 1:2]

      dist_to_start <- sqrt(sum((pt - start_coord)^2))
      dist_to_end <- sqrt(sum((pt - end_coord)^2))

      # If point is NOT at either endpoint, it's interior to this line
      if (dist_to_start > tolerance && dist_to_end > tolerance) {
        is_interior_to_any <- TRUE
        break
      }

      # Special case: for self-intersecting lines, a point at an endpoint
      # may still be a near-return that needs splitting
      if (dist_to_start <= tolerance || dist_to_end <= tolerance) {
        # Point is at an endpoint of this line - check if line is self-intersecting
        # and if so, check if another segment passes near this point
        if (!sf::st_is_simple(line_geoms[li])) {
          # Line is self-intersecting or has near-return
          # Check if point is close to an interior segment (not first/last)
          if (is_near_interior_segment(line_geoms[li], pt, tolerance)) {
            is_interior_to_any <- TRUE
            break
          }
        }
      }
    }

    needs_split[ci] <- is_interior_to_any
  }

  return(needs_split)
}


#' Check if Point is Near Interior Segment
#'
#' @description For a self-intersecting line, checks if a point at an endpoint
#' is also close to an interior segment (not the first or last segment).
#' This identifies near-return patterns where the line loops back near its start/end.
#'
#' @keywords internal
is_near_interior_segment <- function(line_geom, point_coords, tolerance) {
  coords <- sf::st_coordinates(line_geom)[, 1:2, drop = FALSE]
  n_coords <- nrow(coords)

  if (n_coords < 4) return(FALSE)  # Need at least 4 points for interior segments

  # Check segments 2 through (n-2) - the "interior" segments
  # (Skip first segment [1-2] and last segment [(n-1)-n])
  for (i in 2:(n_coords - 2)) {
    p1 <- coords[i, ]
    p2 <- coords[i + 1, ]

    # Project point onto segment
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


#' Calculate Confluence Degrees with Self-Intersection Support
#'
#' @description Calculates the degree (number of line segments meeting) at each
#' confluence point. For regular confluences, this is the count of different lines
#' touching the point. For self-intersection points, a line that crosses itself
#' contributes degree 2 (both "arms" of the crossing).
#'
#' @keywords internal
calculate_confluence_degrees_with_self_int <- function(streamlines, confluence_points, tolerance) {
  if (length(confluence_points) == 0) return(integer(0))

  line_geoms <- sf::st_geometry(streamlines)
  buffered_points <- sf::st_buffer(confluence_points, dist = tolerance)

  # Find which lines intersect each buffered point
  intersects_sparse <- sf::st_intersects(buffered_points, line_geoms)

  # Start with basic degree count
  degrees <- lengths(intersects_sparse)

  # For each confluence, check if any touching line has a self-intersection there
  # If so, add 1 to the degree (the line crosses itself = contributes 2 arms)
  conf_coords <- sf::st_coordinates(confluence_points)[, 1:2, drop = FALSE]

  for (ci in seq_along(confluence_points)) {
    touching_lines <- intersects_sparse[[ci]]
    if (length(touching_lines) == 0) next

    pt <- conf_coords[ci, ]

    for (li in touching_lines) {
      line_geom <- line_geoms[li]

      # Check if this line has a self-intersection at this point
      if (!sf::st_is_simple(line_geom)) {
        # Count how many times this line passes through the point
        passes <- count_line_passes_through_point(line_geom, pt, tolerance)
        if (passes > 1) {
          # Add (passes - 1) to degree since we already counted the line once
          degrees[ci] <- degrees[ci] + (passes - 1)
        }
      }
    }
  }

  return(degrees)
}


#' Count How Many Times a Line Passes Through a Point
#'
#' @description For self-intersecting lines, counts how many times the line
#' passes through (or very close to) a given point.
#'
#' @keywords internal
count_line_passes_through_point <- function(line_geom, point_coords, tolerance) {
  coords <- sf::st_coordinates(line_geom)[, 1:2, drop = FALSE]
  n_coords <- nrow(coords)
  if (n_coords < 2) return(0)

  passes <- 0

  for (i in 1:(n_coords - 1)) {
    p1 <- coords[i, ]
    p2 <- coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      # Project point onto segment
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist <= tolerance) {
        passes <- passes + 1
      }
    }
  }

  return(passes)
}


#' Identify Lines That Need Splitting at Confluences
#'
#' @description Finds lines where confluence points lie in the interior
#' (not at endpoints). Uses vectorized spatial operations with optimizations
#' for large datasets.
#'
#' @details
#' Optimization strategy:
#' 1. Pre-compute all endpoint coordinates once (avoid repeated extraction)
#' 2. Use pre-allocated logical vector instead of growing integer vector
#' 3. Early exit when interior confluence is found for a line
#' 4. Skip lines with no touching confluences (common in sparse networks)
#'
#' @keywords internal
identify_lines_needing_splits <- function(streamlines, confluences, tolerance) {
  if (nrow(confluences) == 0) return(integer(0))

  n_lines <- nrow(streamlines)
  line_geoms <- sf::st_geometry(streamlines)
  conf_geoms <- sf::st_geometry(confluences)

  # Buffer confluences and find which lines they intersect
  # This uses spatial indexing internally (R-tree) for O(n log n) performance

  buffered_conf <- sf::st_buffer(conf_geoms, dist = tolerance)
  line_conf_intersects <- sf::st_intersects(line_geoms, buffered_conf)

  # Quick check: if no line touches any confluence, return early
  has_any_touching <- vapply(line_conf_intersects, function(x) length(x) > 0, logical(1))
  if (!any(has_any_touching)) return(integer(0))

  # Extract all startpoints and endpoints at once (vectorized)
  startpoints <- lwgeom::st_startpoint(line_geoms)
  endpoints_geom <- lwgeom::st_endpoint(line_geoms)

  # Pre-compute ALL endpoint coordinates in one call (major optimization)
  start_coords <- sf::st_coordinates(startpoints)[, 1:2, drop = FALSE]
  end_coords <- sf::st_coordinates(endpoints_geom)[, 1:2, drop = FALSE]
  conf_coords <- sf::st_coordinates(conf_geoms)[, 1:2, drop = FALSE]

  # Pre-allocate result vector
  needs_split <- logical(n_lines)

  # Only iterate over lines that touch at least one confluence
  lines_with_confs <- which(has_any_touching)

  for (i in lines_with_confs) {
    touching_confs <- line_conf_intersects[[i]]

    # Get this line's endpoint coordinates (already pre-computed)
    start_coord <- start_coords[i, ]
    end_coord <- end_coords[i, ]

    # Check each touching confluence
    for (j in touching_confs) {
      pt <- conf_coords[j, ]

      # Calculate squared distances (avoid sqrt for comparison)
      dist_sq_start <- sum((pt - start_coord)^2)
      dist_sq_end <- sum((pt - end_coord)^2)
      tol_sq <- tolerance^2

      if (dist_sq_start > tol_sq && dist_sq_end > tol_sq) {
        # This confluence is in the interior - line needs splitting
        needs_split[i] <- TRUE
        break
      }
    }
  }

  return(which(needs_split))
}


#' Split Lines at Confluence Points
#'
#' @description Performs the actual splitting of lines at confluence points.
#' Uses manual splitting approach for reliability.
#'
#' @keywords internal
split_lines_at_confluences <- function(streamlines, confluences, lines_to_split, tolerance) {

  conf_geoms <- sf::st_geometry(confluences)
  line_geoms <- sf::st_geometry(streamlines)
  line_crs <- sf::st_crs(streamlines)

  # Pre-allocate result list
  n_lines <- nrow(streamlines)
  result_list <- vector("list", n_lines * 10)  # Estimate max size
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

    # Get line coordinates and snap confluence points to line
    line_coords <- sf::st_coordinates(line_geom)[, 1:2, drop = FALSE]
    conf_coords <- sf::st_coordinates(interior_confs)[, 1:2, drop = FALSE]

    # For each confluence point, find ALL positions where it touches the line
    # This is important for self-intersecting lines where a confluence may touch
    # the line at multiple positions
    split_positions <- numeric(0)
    snapped_points <- matrix(nrow = 0, ncol = 2)

    for (ci in seq_len(nrow(conf_coords))) {
      pt <- conf_coords[ci, ]
      # Find ALL positions where this confluence touches the line
      pos_result <- find_all_positions_on_line(line_coords, pt, tolerance)
      if (length(pos_result$positions) > 0) {
        split_positions <- c(split_positions, pos_result$positions)
        snapped_points <- rbind(snapped_points, pos_result$snapped_points)
      }
    }

    # Sort split positions and remove duplicates
    sort_idx <- order(split_positions)
    split_positions <- split_positions[sort_idx]
    snapped_points <- snapped_points[sort_idx, , drop = FALSE]

    # Remove positions too close to each other or to endpoints
    keep <- rep(TRUE, length(split_positions))
    total_length <- calculate_line_length(line_coords)

    for (ci in seq_along(split_positions)) {
      pos <- split_positions[ci]
      # Skip if too close to start or end
      if (pos * total_length < tolerance || (1 - pos) * total_length < tolerance) {
        keep[ci] <- FALSE
        next
      }
      # Skip if too close to previous kept position
      if (ci > 1 && keep[ci - 1]) {
        prev_pos <- split_positions[ci - 1]
        if ((pos - prev_pos) * total_length < tolerance) {
          keep[ci] <- FALSE
        }
      }
    }

    split_positions <- split_positions[keep]
    snapped_points <- snapped_points[keep, , drop = FALSE]

    if (length(split_positions) == 0) {
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- line
      next
    }

    # Split the line at each position
    split_geoms <- split_line_at_positions(line_coords, snapped_points, line_crs)

    if (length(split_geoms) <= 1) {
      result_idx <- result_idx + 1
      result_list[[result_idx]] <- line
      next
    }

    # Create sf objects for each split segment
    for (j in seq_along(split_geoms)) {
      seg_geom <- sf::st_sfc(split_geoms[[j]], crs = line_crs)
      seg_attrs <- line_attrs
      seg_attrs$was_split <- TRUE
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


#' Find Position and Snap Point on Line
#'
#' @description Finds the normalized position (0-1) along a line where a point
#' projects, and returns the snapped location on the line.
#'
#' @param line_coords Matrix of line coordinates (n x 2).
#' @param point_coords Vector of point coordinates (length 2).
#'
#' @returns A list with position (0-1) and snapped_point coordinates.
#'
#' @keywords internal
find_position_and_snap_on_line <- function(line_coords, point_coords) {
  n_coords <- nrow(line_coords)
  if (n_coords < 2) {
    return(list(position = 0, snapped_point = line_coords[1, ]))
  }

  # Calculate segment lengths and cumulative distances
  segment_lengths <- numeric(n_coords - 1)
  for (i in 1:(n_coords - 1)) {
    segment_lengths[i] <- sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
  }
  total_length <- sum(segment_lengths)
  if (total_length == 0) {
    return(list(position = 0, snapped_point = line_coords[1, ]))
  }
  cumulative_dist <- c(0, cumsum(segment_lengths))

  # Find closest segment and position on it
  min_dist <- Inf
  best_position <- 0
  best_snapped <- line_coords[1, ]

  for (i in 1:(n_coords - 1)) {
    p1 <- line_coords[i, ]
    p2 <- line_coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      # Project point onto segment
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist < min_dist) {
        min_dist <- dist
        best_snapped <- proj_point
        best_position <- (cumulative_dist[i] + t * segment_lengths[i]) / total_length
      }
    }
  }

  return(list(position = best_position, snapped_point = best_snapped))
}


#' Find All Positions Where Point Touches Line
#'
#' @description For self-intersecting lines, finds ALL positions where a point
#' is within tolerance of the line, not just the closest. This is essential for
#' correctly splitting lines that cross the same point multiple times.
#'
#' @param line_coords Matrix of line coordinates (n x 2).
#' @param point_coords Vector of point coordinates (length 2).
#' @param tolerance Distance tolerance for considering a point "on" the line.
#'
#' @returns A list with positions (vector of 0-1) and snapped_points (matrix).
#'
#' @keywords internal
find_all_positions_on_line <- function(line_coords, point_coords, tolerance) {
  n_coords <- nrow(line_coords)
  if (n_coords < 2) {
    return(list(positions = numeric(0), snapped_points = matrix(nrow = 0, ncol = 2)))
  }

  # Calculate segment lengths and cumulative distances
  segment_lengths <- numeric(n_coords - 1)
  for (i in 1:(n_coords - 1)) {
    segment_lengths[i] <- sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
  }
  total_length <- sum(segment_lengths)
  if (total_length == 0) {
    return(list(positions = numeric(0), snapped_points = matrix(nrow = 0, ncol = 2)))
  }
  cumulative_dist <- c(0, cumsum(segment_lengths))

  # Find ALL segments where point is within tolerance
  positions <- numeric(0)
  snapped_points <- matrix(nrow = 0, ncol = 2)

  for (i in 1:(n_coords - 1)) {
    p1 <- line_coords[i, ]
    p2 <- line_coords[i + 1, ]
    seg_vec <- p2 - p1
    seg_len_sq <- sum(seg_vec^2)

    if (seg_len_sq > 0) {
      # Project point onto segment
      t <- max(0, min(1, sum((point_coords - p1) * seg_vec) / seg_len_sq))
      proj_point <- p1 + t * seg_vec
      dist <- sqrt(sum((point_coords - proj_point)^2))

      if (dist <= tolerance) {
        pos <- (cumulative_dist[i] + t * segment_lengths[i]) / total_length
        positions <- c(positions, pos)
        snapped_points <- rbind(snapped_points, proj_point)
      }
    }
  }

  return(list(positions = positions, snapped_points = snapped_points))
}


#' Calculate Line Length
#'
#' @description Calculates the total length of a line from its coordinates.
#'
#' @param line_coords Matrix of line coordinates (n x 2).
#'
#' @returns Total length in CRS units.
#'
#' @keywords internal
calculate_line_length <- function(line_coords) {
  if (nrow(line_coords) < 2) return(0)

  total <- 0
  for (i in 1:(nrow(line_coords) - 1)) {
    total <- total + sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
  }
  return(total)
}


#' Split Line at Multiple Positions
#'
#' @description Splits a line at specified snapped point locations.
#' Properly handles multiple split points including those on the same segment
#' or on non-adjacent segments.
#'
#' @param line_coords Matrix of line coordinates (n x 2).
#' @param snapped_points Matrix of snapped split point coordinates (m x 2).
#' @param crs The CRS for output geometries.
#'
#' @returns List of sf LINESTRING geometries.
#'
#' @keywords internal
split_line_at_positions <- function(line_coords, snapped_points, crs) {
  if (nrow(snapped_points) == 0) {
    return(list(sf::st_linestring(line_coords)))
  }

  n_coords <- nrow(line_coords)
  n_splits <- nrow(snapped_points)

  # Calculate cumulative distances for each vertex
  segment_lengths <- numeric(n_coords - 1)
  for (i in 1:(n_coords - 1)) {
    segment_lengths[i] <- sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
  }
  cumulative_dist <- c(0, cumsum(segment_lengths))
  total_length <- cumulative_dist[n_coords]

  if (total_length == 0) {
    return(list(sf::st_linestring(line_coords)))
  }

  # For each split point, find its exact position along the line (as distance)
  split_distances <- numeric(n_splits)
  for (si in seq_len(n_splits)) {
    pt <- snapped_points[si, ]
    min_dist <- Inf
    best_position <- 0

    for (i in 1:(n_coords - 1)) {
      p1 <- line_coords[i, ]
      p2 <- line_coords[i + 1, ]
      seg_vec <- p2 - p1
      seg_len_sq <- sum(seg_vec^2)

      if (seg_len_sq > 0) {
        t <- max(0, min(1, sum((pt - p1) * seg_vec) / seg_len_sq))
        proj_point <- p1 + t * seg_vec
        dist <- sqrt(sum((pt - proj_point)^2))

        if (dist < min_dist) {
          min_dist <- dist
          best_position <- cumulative_dist[i] + t * segment_lengths[i]
        }
      }
    }

    split_distances[si] <- best_position
  }

  # Sort split points by distance along line
  sort_order <- order(split_distances)
  split_distances <- split_distances[sort_order]
  snapped_points <- snapped_points[sort_order, , drop = FALSE]

  # Build output segments by walking along the line and splitting at each point
  result_geoms <- list()
  current_segment_coords <- matrix(nrow = 0, ncol = 2)
  current_dist <- 0
  split_idx <- 1
  vertex_idx <- 1

  while (vertex_idx <= n_coords || split_idx <= n_splits) {
    # Determine next event: vertex or split point
    next_vertex_dist <- if (vertex_idx <= n_coords) cumulative_dist[vertex_idx] else Inf
    next_split_dist <- if (split_idx <= n_splits) split_distances[split_idx] else Inf

    if (next_vertex_dist <= next_split_dist && vertex_idx <= n_coords) {
      # Add this vertex to current segment
      current_segment_coords <- rbind(current_segment_coords, line_coords[vertex_idx, ])
      current_dist <- next_vertex_dist
      vertex_idx <- vertex_idx + 1
    } else if (split_idx <= n_splits) {
      # Add split point and finish current segment
      split_pt <- snapped_points[split_idx, ]

      # Add the split point to current segment
      current_segment_coords <- rbind(current_segment_coords, split_pt)

      # Save current segment if it has at least 2 points
      if (nrow(current_segment_coords) >= 2) {
        result_geoms <- c(result_geoms, list(sf::st_linestring(current_segment_coords)))
      }

      # Start new segment from split point
      current_segment_coords <- matrix(split_pt, nrow = 1, ncol = 2)
      current_dist <- split_distances[split_idx]
      split_idx <- split_idx + 1
    } else {
      break
    }
  }

  # Add final segment if it has at least 2 points
  if (nrow(current_segment_coords) >= 2) {
    result_geoms <- c(result_geoms, list(sf::st_linestring(current_segment_coords)))
  }

  if (length(result_geoms) == 0) {
    return(list(sf::st_linestring(line_coords)))
  }

  return(result_geoms)
}


#' Find Interior Confluence Points for a Line
#'
#' @description Finds confluence points that lie in the interior of a line
#' (not at endpoints). Uses vectorized spatial operations.
#'
#' For self-intersecting or near-return lines, a confluence at an endpoint may
#' also touch the line at an interior position. This function includes such
#' confluences.
#'
#' @keywords internal
find_interior_confluences <- function(line_geom, conf_geoms, tolerance) {
  # Buffer line slightly and find which confluences touch it
  line_buffer <- sf::st_buffer(line_geom, dist = tolerance)
  touches <- sf::st_intersects(line_buffer, conf_geoms)[[1]]

  if (length(touches) == 0) return(sf::st_sfc(crs = sf::st_crs(line_geom)))

  # Get line coordinates to check for interior positions
  line_coords <- sf::st_coordinates(line_geom)[, 1:2, drop = FALSE]
  n_coords <- nrow(line_coords)

  # Get line endpoints
  start_pt <- lwgeom::st_startpoint(line_geom)
  end_pt <- lwgeom::st_endpoint(line_geom)

  # Calculate total line length for position checking
  total_length <- 0
  for (i in 1:(n_coords - 1)) {
    total_length <- total_length + sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
  }

  # Filter to confluences that have at least one interior position
  # (not just at endpoints)
  interior_idx <- vapply(touches, function(j) {
    conf_pt <- conf_geoms[j]
    conf_coords <- sf::st_coordinates(conf_pt)[1:2]

    # Check if confluence is ONLY at endpoints (not interior)
    dist_start <- as.numeric(sf::st_distance(conf_pt, start_pt))
    dist_end <- as.numeric(sf::st_distance(conf_pt, end_pt))

    # If clearly interior (not at either endpoint), include it
    if (dist_start > tolerance && dist_end > tolerance) {
      return(TRUE)
    }

    # If at endpoint, check if it ALSO touches an interior segment
    # This handles near-return lines where a point at the start also
    # appears in the middle of the line
    has_interior_position <- FALSE
    cumulative_dist <- 0

    for (i in 1:(n_coords - 1)) {
      p1 <- line_coords[i, ]
      p2 <- line_coords[i + 1, ]
      seg_vec <- p2 - p1
      seg_len <- sqrt(sum(seg_vec^2))
      seg_len_sq <- sum(seg_vec^2)

      if (seg_len_sq > 0) {
        # Project point onto segment
        t <- max(0, min(1, sum((conf_coords - p1) * seg_vec) / seg_len_sq))
        proj_point <- p1 + t * seg_vec
        dist <- sqrt(sum((conf_coords - proj_point)^2))

        if (dist <= tolerance) {
          # Found a touch point - check if it's interior (not near start/end)
          pos_dist <- cumulative_dist + t * seg_len
          dist_from_start <- pos_dist
          dist_from_end <- total_length - pos_dist

          if (dist_from_start > tolerance && dist_from_end > tolerance) {
            has_interior_position <- TRUE
            break
          }
        }
      }
      cumulative_dist <- cumulative_dist + seg_len
    }

    return(has_interior_position)
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
