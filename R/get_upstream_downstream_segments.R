#' Get Upstream or Downstream Stream Segments
#'
#' @description Selects stream segments that are upstream or downstream of a
#' target segment using network traversal. Directionality is determined by the
#' underlying line geometry direction (start vertex to end vertex).
#'
#' @details
#' This function performs the following steps:
#'   1. Creates a directed network from streamlines using `sfnetworks`
#'   2. Identifies the target segment (by index or spatial query)
#'   3. Uses graph traversal to find connected upstream or downstream segments
#'   4. Returns the subset of streamlines matching the traversal
#'
#' The direction of flow is inferred from line geometry:
#' - By default, assumes lines are digitized in the direction of flow
#'   (upstream vertex to downstream vertex, which is common for hydrological data)
#' - For downstream: traverses from the target's end node following edge direction
#' - For upstream: traverses to the target's start node against edge direction
#'
#' If your data is digitized downstream-to-upstream, set `reverse_direction = TRUE`.
#'
#' @param streamlines An `sf` object with LINESTRING or MULTILINESTRING geometry
#'   representing the stream network.
#' @param target Either:
#'   - An integer specifying the row index of the target segment in `streamlines`
#'   - An `sf` POINT object that will be snapped to the nearest segment
#' @param direction Character. Either `"upstream"` or `"downstream"` to specify
#'   which connected segments to return. Default is `"downstream"`.
#' @param reverse_direction Logical. If `TRUE`, reverses the assumed flow direction.
#'   Use this if your lines are digitized downstream-to-upstream (opposite of the
#'   typical convention). Default is `FALSE`.
#'
#' @returns An `sf` LINESTRING object containing the stream segments that are
#' upstream or downstream of the target segment. The target segment itself is
#' NOT included in the output. Returns an empty sf object (0 rows) if no
#' connected segments are found in the specified direction.
#'
#' All original attributes from `streamlines` are preserved.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Load stream network
#' fname <- system.file("extdata", "bcfwa2.gpkg", package="streamgis")
#' bcfwa <- sf::st_read(fname)
#' target <- which(bcfwa$LINEAR_FEATURE_ID == 701771373)
#'
#' # Get downstream segments from target segment
#' # Remember to set reverse_direction=TRUE if lines are digitized downstream-to-upstream (BCFWA)
#'
#' downstream <- get_upstream_downstream_segments(bcfwa,
#' target = target,
#' direction = "downstream",
#' reverse_direction = TRUE)
#'
#' plot(st_geometry(bcfwa), col = "grey")
#' plot(st_geometry(downstream), col = "blue", lwd = 2, add = TRUE)
#'
#' upstream <- get_upstream_downstream_segments(bcfwa,
#' target = target,
#' direction = "upstream",
#' reverse_direction = TRUE)
#'
#' plot(st_geometry(upstream), col = "red", lwd = 2, add = TRUE)
#' plot(st_geometry(bcfwa[target,]), col = "yellow", lwd = 5, add = TRUE)
#'
#' }
#'
#' @export
#' @importFrom sf st_cast st_crs st_geometry_type st_nearest_feature
#' @importFrom sf st_sfc st_sf st_drop_geometry st_transform
#' @importFrom sfnetworks as_sfnetwork activate
#' @importFrom igraph E V subcomponent incident

get_upstream_downstream_segments <- function(streamlines,
                                             target,
                                             direction = c("downstream", "upstream"),
                                             reverse_direction = FALSE) {
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

  direction <- match.arg(direction)

  # ===== Step 1: Preprocessing =====
  # Cast to LINESTRING if needed
  if (geom_type == "MULTILINESTRING" || geom_type == "GEOMETRY") {
    streamlines <- suppressWarnings(sf::st_cast(streamlines, "LINESTRING"))
  }

  # Add internal edge ID for tracking
  streamlines$.__edge_id__ <- seq_len(nrow(streamlines))

  # ===== Step 2: Identify Target Segment =====
  if (inherits(target, "sf")) {
    # Target is a point - find nearest segment
    if (nrow(target) != 1) {
      stop("`target` point must have exactly 1 feature.")
    }

    # Ensure same CRS
    if (!identical(sf::st_crs(streamlines), sf::st_crs(target))) {
      target <- sf::st_transform(target, sf::st_crs(streamlines))
    }

    target_idx <- sf::st_nearest_feature(target, streamlines)
  } else if (is.numeric(target) && length(target) == 1) {
    # Target is a row index
    target_idx <- as.integer(target)

    if (target_idx < 1 || target_idx > nrow(streamlines)) {
      stop("`target` index is out of bounds. Must be between 1 and ",
           nrow(streamlines),
           ".")
    }
  } else {
    stop("`target` must be either an integer row index or an sf POINT object.")
  }

  # ===== Step 3: Create Directed Network =====
  # Create network - direction follows line geometry (start -> end)
  network <- sfnetworks::as_sfnetwork(streamlines, directed = TRUE)

  # Get the underlying igraph object
  graph <- igraph::as.igraph(network)

  # ===== Step 4: Find Target Edge's Nodes =====
  target_edge_idx <- target_idx

  # Get the from and to nodes of the target edge (based on original geometry direction)
  edge_endpoints <- igraph::ends(graph, igraph::E(graph)[target_edge_idx])
  from_node <- edge_endpoints[1, 1]  # Start of line (digitization start)
  to_node <- edge_endpoints[1, 2]    # End of line (digitization end)

  # ===== Step 5: Traverse Network =====
  # Determine traversal parameters based on direction and reverse_direction flag
  #
  # Default assumption: lines digitized upstream → downstream (with flow)
  #   - downstream: follow edge direction (mode="out" from to_node)
  #   - upstream: against edge direction (mode="in" from from_node)
  #
  # If reverse_direction=TRUE: lines digitized downstream → upstream (against flow)
  #   - downstream: against edge direction (mode="in" from from_node)
  #   - upstream: follow edge direction (mode="out" from to_node)

  if (direction == "downstream") {
    if (reverse_direction) {
      # Physical downstream is against digitization direction
      start_node <- from_node
      traverse_mode <- "in"
      edge_filter_col <- 2  # Filter by "to" column
    } else {
      # Physical downstream follows digitization direction
      start_node <- to_node
      traverse_mode <- "out"
      edge_filter_col <- 1  # Filter by "from" column
    }
  } else {
    # direction == "upstream"
    if (reverse_direction) {
      # Physical upstream follows digitization direction
      start_node <- to_node
      traverse_mode <- "out"
      edge_filter_col <- 1  # Filter by "from" column
    } else {
      # Physical upstream is against digitization direction
      start_node <- from_node
      traverse_mode <- "in"
      edge_filter_col <- 2  # Filter by "to" column
    }
  }

  # Find all reachable nodes from the start node
  reachable_nodes <- igraph::subcomponent(graph, start_node, mode = traverse_mode)

  # Get all edges and their endpoints
  all_edges <- igraph::E(graph)
  edge_endpoints_all <- igraph::ends(graph, all_edges)

  # Find edges connected to reachable nodes
  selected_edges <- which(edge_endpoints_all[, edge_filter_col] %in% as.integer(reachable_nodes))

  # Remove the target edge from results
  selected_edge_ids <- setdiff(selected_edges, target_edge_idx)

  # ===== Step 6: Extract Selected Segments =====
  if (length(selected_edge_ids) == 0) {
    # No connected segments found - return empty sf with same structure
    result <- streamlines[0, ]
    result$.__edge_id__ <- NULL
    message("No ", direction, " segments found from the target segment.")
    return(result)
  }

  # Get the original edge IDs (row numbers in streamlines)
  # Since network preserves edge order, edge index = row index
  result <- streamlines[selected_edge_ids, ]

  # Clean up internal column
  result$.__edge_id__ <- NULL

  # Reset row names
  row.names(result) <- NULL

  message("Found ", nrow(result), " ", direction, " segment(s).")

  return(result)
}
