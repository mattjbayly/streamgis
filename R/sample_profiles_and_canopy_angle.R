#' Sample Profiles and Compute Canopy Open Angles
#'
#' @description
#' Sample points at regular spacing along each csl line feature, extract raster values
#' (e.g., tree height / elevation) at those points, and compute a simple
#' canopy-open angle per unique \code{(l_id, p_id)} profile based on the first
#' left/right rise above the center point.
#'
#' @details
#' For each csl line, points are generated every \code{step_m} meters from the start
#' of the geometry to its end (the end is included even when not a multiple of
#' \code{step_m}). Raster values are extracted at those points. For each profile
#' (\code{l_id}, \code{p_id}), the center point is chosen as the sample closest
#' to \code{center_distance}. The first point to the left and right with a value
#' strictly greater than the center value defines the left/right rays. Angles are
#' computed as \eqn{\mathrm{atan2}(\Delta y, \Delta x)} in degrees and summed to a
#' \emph{canopy open angle}.
#'
#' Inputs must be projected in meters (e.g., UTM). If the raster CRS differs from
#' the lines CRS, the raster is projected to match the lines.
#'
#' @param csl An \code{sf} data frame of \code{LINESTRING}/\code{MULTILINESTRING}
#'   geometry with columns \code{l_id} and \code{p_id}. Cross sectional lines.
#'   This object should be generated from \code{streamgis::cross_section_lines}
#' @param raster_path_tree_height Character path to a raster readable by \pkg{terra}
#'   (tree height or DSM elevation - fine scale digital surface model).
#' @param raster_path_terrain Character path to a raster readable by \pkg{terra}
#'   (terrain or DTM elevation - fine scale digital terrain model).
#' @param step_m Numeric spacing (meters) between sampled points along each line.
#'   Must be > 0. Default \code{1} for 1m.
#' @param center_distance Numeric distance (meters) along each profile at which to
#'   anchor the center point for the angle calculation. Default \code{50} 50m.
#'
#' @returns
#' A list with:
#' \itemize{
#'   \item \code{points}: an \code{sf} POINT layer with columns \code{l_id}, \code{p_id},
#'     \code{uid}, \code{dist_m}, and one column per raster band (e.g., \code{tree_height}).
#'   \item \code{canopy}: a data frame with one row per profile (\code{l_id}, \code{p_id}, \code{uid})
#'     and columns \code{center_dist_m}, \code{center_elev}, \code{left_angle_deg},
#'     \code{right_angle_deg}, and \code{canopy_open_angle_deg}.
#' }
#'
#' @section Angle definition:
#' Angles are measured from the horizontal using \code{atan2(Δy, Δx)} (degrees).
#' The left ray goes toward decreasing \code{dist_m}; the right ray toward increasing
#' \code{dist_m}. If no qualifying point exists on a side, that side's angle is 0.
#' The canopy open angle measurement is the sum of the left ray and the right ray.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(streamgis)
#' # Load Stream Lines
#' fname <-
#'   system.file("extdata", "./ndss/lidar_strms.gpkg", package = "streamgis")
#'
#' center_line <- sf::st_read(fname)
#' # Drop duplicate geometries
#' center_line <- center_line[!duplicated(sf::st_as_text(sf::st_geometry(center_line))), ]
#' # Drop emty geometires
#' center_line <- center_line[!sf::st_is_empty(center_line), ]
#' center_line$length_m <- as.numeric(sf::st_length(center_line))
#'
#' summary(center_line$length_m)
#' # plot(sf::st_geometry(center_line))
#' center_line <- suppressWarnings({
#'   sf::st_cast(center_line, "LINESTRING")
#' })
#'
#' # Make sure there is an ID field
#' center_line$id <- 1:nrow(center_line)
#'
#' # =============================================
#' # Sample Points Along the Stream
#' # =============================================
#'
#' # Sample points along line
#' pol <- suppressWarnings({
#'   points_on_line(center_line, point_spacing = 50)
#' })
#'
#'
#' dev.off()
#' plot(sf::st_geometry(center_line[center_line$id == 3, ]))
#' plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")
#'
#'
#' # =============================================
#' # Create Cross-Sectional Lines
#' # =============================================
#'
#' csl <- cross_section_lines(
#'   center_line = center_line,
#'   points = pol,
#'   cross_profile_length = 151,
#'   epsg = 26910
#' )
#'
#'
#' plot(sf::st_geometry(center_line[center_line$id == 12, ]))
#' plot(sf::st_geometry(pol[pol$l_id == 12, ]), add = TRUE, col = "red")
#' plot(sf::st_geometry(csl[csl$l_id == 12, ]), add = TRUE, col = "blue")
#'
#'
#'
#' # =============================================
#' # Calculate Open Angle
#' # =============================================
#'
#' tree_raster_path <- system.file("extdata", "./ndss/gedi_tree_32610.tif", package = "streamgis")
#'
#' spcoa <- sample_profiles_and_canopy_angle(
#'   csl = csl,
#'   raster_path_tree_height = tree_raster_path,
#'   step_m = 1,
#'   center_distance = (151 / 2),
#'   set_vertical_offset = 1.5
#' )
#'
#' # Generate sample QA plots
#' spcoa$points  # sf POINTs: l_id, p_id, uid, dist_m, elevation/tree height
#' head(spcoa$canopy)  # one row per (l_id, p_id) with canopy_open_angle_deg
#'
#'
#' # =============================================
#' # Create Detailed Diagnostic Plots
#' # =============================================
#'
#' # See vignette
#'
#' }
#' @importFrom sf st_crs st_geometry st_length st_line_sample st_cast st_drop_geometry
#' @importFrom dplyr bind_cols mutate group_by group_map arrange
#' @importFrom terra rast project same.crs extract vect nlyr
#' @export
sample_profiles_and_canopy_angle <- function(csl,
                                             raster_path_tree_height = NULL,
                                             raster_path_terrain = NULL,
                                             step_m = 1,
                                             center_distance = 50,
                                             set_vertical_offset = NULL) {
  # Relabel objects
  lines_sf <- csl

  if (!all(c("l_id", "p_id") %in% names(lines_sf))) {
    stop("lines_sf must contain columns 'l_id' and 'p_id'.")
  }

  if (!inherits(sf::st_geometry(lines_sf),
                c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
    stop("Geometry must be LINESTRING/MULTILINESTRING.")
  }

  if (step_m <= 0) {
    stop("'step_m' must be > 0")
  }

  # load & align raster - for dsm or tree height
  r <- terra::rast(raster_path_tree_height)
  crs_lines <- sf::st_crs(lines_sf)

  if (is.na(crs_lines))
    stop("lines_sf has no CRS.")

  if (!terra::same.crs(r, crs_lines$wkt)) {
    r <- terra::project(r, crs_lines$wkt)
  }

  # ---- per-feature sampling (fractional) ----
  sample_one <- function(row_sf) {
    p_id <- row_sf$p_id[[1]]
    l_id <- row_sf$l_id[[1]]
    geom <- sf::st_geometry(row_sf)[[1]]

    Lm <- as.numeric(sf::st_length(geom))
    if (Lm == 0)
      return(NULL)

    pos_m <- seq(0, Lm, by = step_m)
    if (tail(pos_m, 1) < Lm)
      pos_m <- c(pos_m, Lm)  # include end

    eps  <- .Machine$double.eps
    frac <- pmin(pos_m / Lm, 1 - eps)

    s   <- sf::st_line_sample(geom, sample = frac)
    pts <- sf::st_cast(s, "POINT")

    if (length(pts) != length(pos_m))
      pos_m <- pos_m[seq_len(length(pts))]

    sf::st_sf(
      l_id = l_id,
      p_id = p_id,
      dist_m = pos_m,
      geometry = pts,
      crs = sf::st_crs(row_sf)
    )
  }

  pts_list <- lapply(seq_len(nrow(lines_sf)), function(i)
    sample_one(lines_sf[i, ]))
  pts_sf   <- dplyr::bind_rows(pts_list)

  # extract raster values
  vals <- terra::extract(r, terra::vect(pts_sf))
  vals <- vals[, -1, drop = FALSE]
  if (ncol(vals) == 0)
    stop("No values extracted from raster at sampled points.")

  nm <- names(r)
  if (is.null(nm) ||
      all(nm == ""))
    nm <- paste0("band_", seq_len(terra::nlyr(r)))
  names(vals) <- nm
  pts_sf <- dplyr::bind_cols(pts_sf, vals)

  # uid per profile
  pts_sf <- dplyr::mutate(pts_sf, uid = paste0(l_id, "__", p_id))


  # -----------------------------------------
  # Also load the DTM (if provided)
  if(length(raster_path_terrain) > 0) {
    # load & align raster - for dsm or tree height
    r2 <- terra::rast(raster_path_terrain)
    crs_lines <- sf::st_crs(lines_sf)

    if (is.na(crs_lines))
      stop("lines_sf has no CRS.")

    if (!terra::same.crs(r2, crs_lines$wkt)) {
      r2 <- terra::project(r2, crs_lines$wkt)
    }

    # extract raster values
    vals <- terra::extract(r2, terra::vect(pts_sf))
    vals <- vals[, -1, drop = FALSE]
    if (ncol(vals) == 0)
      stop("No values extracted from raster at sampled points.")

    nm <- names(r2)
    if (is.null(nm) ||
        all(nm == ""))
      nm <- paste0("band_", seq_len(terra::nlyr(r)))
    names(vals) <- nm
    pts_sf <- dplyr::bind_cols(pts_sf, vals)

    # uid per profile
    pts_sf <- dplyr::mutate(pts_sf, uid = paste0(l_id, "__", p_id))

  }



  # Take a sneak peak at a profile view.

  # plot(pts_sf$dist_m[pts_sf$uid == "1__2"], pts_sf$dsm_26910[pts_sf$uid == "1__2"], type = 'l', col = "darkgreen")
  # points(pts_sf$dist_m[pts_sf$uid == "1__2"], pts_sf$dtm_26910[pts_sf$uid == "1__2"], type = 'l', col = "brown")

  # ---- canopy angle per (l_id, p_id) ----
  # Will apply function to each transect
  compute_one_angle <- function(df_group,
                                center_distance,
                                default_missing_angle = 90) {

    # For manual debugging
    if(FALSE) {
      df_group <- pts_sf[pts_sf$uid == "15__11", ]
    }

    # Sort by distance on x-axis to make sure points are ordered
    df_group <- dplyr::arrange(df_group, dist_m)

    # first data/value column
    # Find the elevation column
    val_col <- setdiff(names(df_group), c("l_id", "p_id", "uid", "dist_m"))[1]
    val_col_dtm <- setdiff(names(df_group), c("l_id", "p_id", "uid", "dist_m"))[2]

    # center sample (nearest to center_distance)
    i0 <- which.min(abs(df_group$dist_m - center_distance))
    cx <- df_group$dist_m[i0]
    cy <- df_group[[val_col]][i0]

    # optional vertical override
    if (exists("set_vertical_offset", inherits = TRUE) &&
        length(set_vertical_offset) > 0) {
      cy <- as.numeric(set_vertical_offset)
    }

    # If DTM is provided then use it
    if(length(raster_path_terrain) > 0) {
      cy <- df_group[[val_col_dtm]][i0]
    }

    # If DTM is provided and vertical offset is set
    # then add it to the DTM
    if(length(set_vertical_offset) > 0 & length(raster_path_terrain) > 0) {
      cy <- df_group[[val_col_dtm]][i0] + as.numeric(set_vertical_offset)
    }

    # bail if center is NA
    if (is.na(cy)) {
      return(
        data.frame(
          l_id = df_group$l_id[[1]],
          p_id = df_group$p_id[[1]],
          uid  = df_group$uid[[1]],
          center_dist_m = cx,
          center_elev   = cy,
          left_angle_deg  = NA_real_,
          right_angle_deg = NA_real_,
          canopy_open_angle_deg = NA_real_
        )
      )
    }

    # helpers ---------------------------------------------------------------
    # angle from vertical in degrees for a single (x,y)
    angle_from_vertical_pt <- function(x, y, cx, cy) {
      dx <- x - cx
      dy <- y - cy
      if (is.na(dx) ||
          is.na(dy) || dy <= 0)
        return(NA_real_)  # must be above center
      # from vertical: theta = atan(|dx| / dy)
      atan2(abs(dx), dy) * 180 / pi
    }

    # side-min: smallest angle among candidates on one side
    side_min_angle <- function(xs, ys, side = c("left", "right")) {
      side <- match.arg(side)
      # keep points on the requested side and above the center
      keep <- if (side == "left")
        xs < cx
      else
        xs > cx
      keep <- keep & (ys > cy) & !is.na(xs) & !is.na(ys)
      if (!any(keep))
        return(default_missing_angle)

      ang <- mapply(
        angle_from_vertical_pt,
        x = xs[keep],
        y = ys[keep],
        MoreArgs = list(cx = cx, cy = cy)
      )
      # if all invalid (NA), use default; else min valid
      if (all(is.na(ang)))
        default_missing_angle
      else
        min(ang, na.rm = TRUE)
    }

    # compute per-side angles ----------------------------------------------
    xs <- df_group$dist_m
    ys <- df_group[[val_col]]

    left_angle  <- side_min_angle(xs, ys, "left")
    right_angle <- side_min_angle(xs, ys, "right")

    if (is.na(left_angle) ||
        is.na(right_angle)) {
      canopy_angle <- NA_real_

    } else {
      canopy_angle <- left_angle + right_angle
    }

    # optional: force "closed" if center suggests overhanging veg
    has_offset <- exists("set_vertical_offset", inherits = TRUE) &&
      length(get("set_vertical_offset", inherits = TRUE)) > 0

    offset_val <- if (has_offset) {
      as.numeric(get("set_vertical_offset", inherits = TRUE))
    } else {
      0
    }

    # If DTM is provided and vertical offset is set
    # then add it to the DTM
    if(length(set_vertical_offset) > 0 & length(raster_path_terrain) > 0) {
      offset_val <- df_group[[val_col_dtm]][i0] + as.numeric(set_vertical_offset)
    }

    cent_point_elev <- df_group[[val_col]][i0]

    # Overhanging vegetation
    if(has_offset) {
      if (!is.na(cent_point_elev) && cent_point_elev > offset_val) {
        left_angle <- 0
        right_angle <- 0
        canopy_angle <- 0
      }
    }

    data.frame(
      l_id = df_group$l_id[[1]],
      p_id = df_group$p_id[[1]],
      uid  = df_group$uid[[1]],
      center_dist_m = cx,
      center_elev   = cy,
      left_angle_deg  = left_angle,
      right_angle_deg = right_angle,
      canopy_open_angle_deg = canopy_angle
    )
  }


  # Run the canopy angle function across profiles
  canopy_df_list <- pts_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(l_id, p_id, uid) %>%
    dplyr::group_map( ~ compute_one_angle(.x, center_distance), .keep = TRUE)

  canopy_df <- do.call("rbind", canopy_df_list)

  # summary(canopy_df$canopy_open_angle_deg)

  # If center point elevation is NA then
  # left_angle_deg right_angle_deg and canopy_open_angle_deg are NA
  canopy_df$left_angle_deg <- ifelse(is.na(canopy_df$center_dist_m),
                                     NA,
                                     canopy_df$left_angle_deg)
  canopy_df$right_angle_deg <- ifelse(is.na(canopy_df$center_dist_m),
                                      NA,
                                      canopy_df$right_angle_deg)
  canopy_df$canopy_open_angle_deg <- ifelse(is.na(canopy_df$center_dist_m),
                                            NA,
                                            canopy_df$canopy_open_angle_deg)


  #canopy_df$left_angle_deg <- 90 - canopy_df$left_angle_deg
  #canopy_df$right_angle_deg <- 90 - canopy_df$right_angle_deg
  canopy_df$canopy_open_angle_deg <- canopy_df$left_angle_deg + canopy_df$right_angle_deg

  list(points = pts_sf, canopy = canopy_df)
}

