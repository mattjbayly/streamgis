#' Generate Points on Line
#'
#' @description Sample points at a fixed spacing along stream centerlines.
#'
#' @details
#' For each LINESTRING in `center_line`, this function generates:
#'   - a start point,
#'   - regularly spaced sample points at `point_spacing` (m) from the start, and
#'   - an end point.
#'
#' It returns an `sf` point layer with metadata:
#' `l_id` (line id), `p_id` (point index within line), `group` (`start`/`sample`/`end`),
#' and `distance_m` (distance along the line from its origin).
#'
#' @param center_line An `sf` LINESTRING/MULTILINESTRING with a column `id` identifying each line.
#'   Must be projected (meters). If unprojected/unknown, the function will assign the provided `epsg`.
#' @param point_spacing Numeric. Point spacing in meters.
#' @param epsg Numeric EPSG code for the projected CRS in meters.
#' @param reverse_coordinates Logical; if `TRUE`, reverse each line before sampling.
#'
#' @returns An `sf` POINT layer with columns:
#'   `l_id`, `p_id`, `group` (`start`/`sample`/`end`), `distance_m`.
#'
#' @examples
#' \dontrun{
#' fname <- system.file("extdata", "center_line.gpkg", package = "streamgis")
#' center_line <- sf::st_read(fname, quiet = TRUE)
#' plot(sf::st_geometry(center_line))
#'
#' pts <- suppressWarnings(points_on_line(center_line, point_spacing = 100, epsg = 26910))
#'
#' plot(sf::st_geometry(center_line[center_line$id == 1, ]))
#' plot(sf::st_geometry(pts[pts$l_id == 1, ]), add = TRUE, col = "red")
#' }
#'
#' @export
points_on_line <- function(center_line = NA,
                           point_spacing = 50,
                           epsg = 26910,
                           reverse_coordinates = FALSE) {

  if (is.na(center_line)[1] || !inherits(center_line, "sf")) {
    stop("`center_line` must be an sf object (LINESTRING/MULTILINESTRING) with an `id` column.")
  }
  if (!"id" %in% names(center_line)) {
    stop("`center_line` must have an `id` column.")
  }
  if (nrow(center_line) < 1) stop("Center line is empty.")
  if (!is.numeric(point_spacing) || point_spacing <= 0) {
    stop("`point_spacing` must be a positive number (meters).")
  }

  if (epsg == 26910) {
    message("Assigning EPSG:26910 (UTM zone 10N) for input data.")
  }

  # set (not transform) CRS if missing/unknown
  if (is.na(sf::st_crs(center_line))) {
    center_line <- sf::st_set_crs(center_line, epsg)
  }

  all_points <- vector("list", nrow(center_line))

  for (j in seq_len(nrow(center_line))) {

    line <- center_line[j, ]
    line <- suppressWarnings(sf::st_cast(line, "LINESTRING"))

    if (reverse_coordinates) {
      line <- sf::st_reverse(line)
    }

    segment_length <- as.numeric(sf::st_length(line))

    if(length(segment_length) != 1) {
      print("Cast to linstring")
      stop("Error: MULTILINE STRING with parts not handled")
    }


    if (point_spacing > segment_length) {
      # single segment: just start & end
      crds  <- sf::st_coordinates(line)
      crds  <- as.data.frame(crds)
      mdist <- segment_length
      x_set <- crds$X[c(1, nrow(crds))]
      y_set <- crds$Y[c(1, nrow(crds))]

      add_pt <- data.frame(
        l_id       = line$id,
        p_id       = c(1, 2),
        group      = c("start", "end"),
        distance_m = c(0, mdist),
        X          = x_set,
        Y          = y_set
      )

      all_points[[j]] <- add_pt
      warning("Point spacing is longer than line segment length; created only start/end points.")
      next
    }

    # regular interior samples
    d_int    <- segment_length / point_spacing
    max_int  <- floor(d_int)
    max_dist <- max_int * point_spacing
    p_dists  <- if (max_dist >= point_spacing) seq(point_spacing, max_dist, by = point_spacing) else numeric(0)

    # positions along [0, 1]
    frac <- if (length(p_dists)) p_dists / segment_length else numeric(0)
    frac <- if (length(frac)) pmax(0, pmin(1, frac)) else numeric(0)

    p_on_line <- if (length(frac)) sf::st_line_sample(line, sample = frac) else sf::st_sfc(crs = sf::st_crs(line))
    p_on_line <- if (length(frac)) sf::st_cast(p_on_line, "POINT") else p_on_line

    # start/end coords
    coords <- sf::st_coordinates(line)
    coords <- as.data.frame(coords)

    check_1 <- unique(coords$L1)
    check_2 <- unique(coords$L2)
    if (!identical(check_1, 1)) stop("Unhandled MULTILINESTRING structure (L1 != 1).")
    if (length(check_2) > 0 && !identical(check_2, 1)) stop("Unhandled MULTILINESTRING structure (L2 != 1).")

    coords_start <- stats::setNames(as.data.frame(coords[1, 1:2]), c("X", "Y"))
    coords_end   <- stats::setNames(as.data.frame(coords[nrow(coords), 1:2]), c("X", "Y"))
    coords_start$group <- "start"
    coords_end$group   <- "end"

    coord_samp <- if (length(frac)) {
      cs <- as.data.frame(sf::st_coordinates(p_on_line))[, 1:2]
      names(cs) <- c("X", "Y"); cs$group <- "sample"; cs
    } else {
      stats::setNames(data.frame(X = numeric(0), Y = numeric(0), group = character(0)),
               c("X", "Y", "group"))
    }

    coord_all <- rbind(coords_start, coord_samp, coords_end)

    # helper: project each point onto line to measure distance from origin
    gproject_planar <- function(line_ls, pts) {
      M      <- sf::st_coordinates(line_ls)[, 1:2, drop = FALSE]
      segvec <- M[-1, , drop = FALSE] - M[-nrow(M), , drop = FALSE]
      seglen <- sqrt(rowSums(segvec^2))
      cum0   <- c(0, cumsum(seglen))

      vapply(sf::st_geometry(pts), function(pt) {
        p <- sf::st_coordinates(pt)
        num <- rowSums((matrix(p, nrow(segvec), 2, byrow = TRUE) - M[-nrow(M), ]) * segvec)
        den <- rowSums(segvec^2)
        t   <- pmax(0, pmin(1, num / den))
        proj <- M[-nrow(M), ] + segvec * t
        d2   <- rowSums((proj - matrix(p, nrow(proj), 2, byrow = TRUE))^2)
        k    <- which.min(d2)
        cum0[k] + t[k] * seglen[k]
      }, numeric(1))
    }

    ca_sf <- sf::st_as_sf(coord_all, coords = c("X", "Y"), crs = sf::st_crs(line))
    m_distances <- gproject_planar(line_ls = line, pts = ca_sf)
    m_distances <- round(m_distances, 2)
    coord_all$distance_m <- m_distances

    # light sanity check if enough points
    if (nrow(coord_all) > 5 && any(coord_all$group == "sample")) {
      samp_idx <- which(coord_all$group == "sample")
      if (length(samp_idx) >= 2) {
        dist_check <- coord_all$distance_m[samp_idx[2]] - coord_all$distance_m[samp_idx[1]]
        diff <- abs(1 - (point_spacing / dist_check))
        if (is.finite(diff) && diff > 0.1) {
          stop("Large spacing error (>10%). Check CRS/units or geometry validity.")
        }
      }
    }

    # ids & order
    coord_all$p_id <- seq_len(nrow(coord_all))
    coord_all$l_id <- line$id
    coord_all <- coord_all[, c("l_id", "p_id", "group", "distance_m", "X", "Y")]

    all_points[[j]] <- coord_all
  }

  output <- do.call(rbind, all_points)
  output <- sf::st_as_sf(output, coords = c("X", "Y"), crs = sf::st_crs(center_line))
  output
}
