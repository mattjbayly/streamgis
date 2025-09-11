#' Perpendicular Cross-Section Lines
#'
#' @description Generate perpendicular cross-sectional lines to a stream centerline.
#'
#' @details
#' Takes a stream centerline object and output from `points_on_line()` (or user-supplied
#' sample points) and generates perpendicular lines at those points.
#'
#' @param center_line An `sf` data frame of LINESTRING or MULTILINESTRING geometry.
#'   Must include a column `id` (line identifier) and be projected in meters (local UTM).
#' @param points An `sf` data frame of sample points (e.g., from `points_on_line()`)
#'   at which to create perpendicular profiles.
#' @param cross_profile_length Numeric. Total length of each cross-section (m). The
#'   function uses half of this value on each side of the centerline.
#' @param epsg Numeric. EPSG code for the projected CRS (meters).
#'
#' @returns An `sf` data frame of perpendicular cross-section profile lines relative to
#'   the stream centerline, with columns `l_id`, `p_id`, `group`, `distance_m`,
#'   `bearing_next`, `bearing1`, and `bearing2`.
#'
#' @examples
#' \dontrun{
#' library(streamgis)
#' # Example centerline
#' fname <- system.file("extdata", "center_line.gpkg", package = "streamgis")
#' center_line <- sf::st_read(fname)
#' plot(sf::st_geometry(center_line))
#'
#' # Sample points along the line
#' pol <- suppressWarnings(points_on_line(center_line, point_spacing = 100, epsg = 26910))
#'
#' # Build cross sections
#' csl <- cross_section_lines(
#'   center_line = center_line,
#'   points      = pol,
#'   cross_profile_length = 250,
#'   epsg        = 26910
#' )
#'
#' plot(sf::st_geometry(center_line[center_line$id == 1, ]))
#' plot(sf::st_geometry(pol[pol$l_id == 1, ]), add = TRUE, col = "red")
#' plot(sf::st_geometry(csl[csl$l_id == 1, ]), add = TRUE, col = "blue")
#' }
#'
#' @importFrom magrittr %>%
#' @export
cross_section_lines <- function(center_line = NA,
                                points = NA,
                                cross_profile_length = 250,
                                epsg = 26910) {

  mgroup <- NULL

  # Cut with cross sectional profiles
  if (nrow(center_line) < 1) {
    stop("Center line is empty")
  }

  if (epsg == 26910) {
    message("Assigning EPSG:26910 UTM UTM zone 10N for input data")
  }

  # Assign projection if not already set
  sf::st_crs(center_line) <- epsg


  all_lines <- list()

  # Loop through multiline string
  for (j in 1:nrow(center_line)) {
    # j = 1 # debugging
    line <- center_line[j, ]
    line <- suppressWarnings({
      sf::st_cast(line, "LINESTRING")
    })

    # Get the points linked to current line segment
    line_id <- unique(line$id)[1]

    these_pts <- points[which(points$l_id == line_id), ]

    coord_all <- sf::st_coordinates(these_pts)
    coord_all <- as.data.frame(coord_all)

    # Convert to lat long for bearing
    ca_sp <- sf::st_as_sf(coord_all, coords = c("X", "Y"), crs = epsg)
    ca_sp_4326 <- sf::st_transform(ca_sp, 4326)

    cb <- sf::st_coordinates(ca_sp_4326)
    cb <- as.data.frame(cb)

    p1 <- cb[-nrow(cb), ]  # origins
    p2 <- cb[-1, ]         # destinations

    # Fast vectorized bearings (0–360°) between consecutive rows of X (lon), Y (lat)
    # Spherical Earth formula (no loops, no sp). Works directly on a data.frame.
    bearing_seq <- function(df, x = "X", y = "Y") {
      n <- nrow(df)
      out <- rep(NA_real_, n)
      if (n < 2)
        return(out)

      i   <- seq_len(n - 1L)
      lon1 <- df[[x]][i]
      lat1 <- df[[y]][i]
      lon2 <- df[[x]][i + 1L]
      lat2 <- df[[y]][i + 1L]

      ok <- is.finite(lon1) &
        is.finite(lat1) & is.finite(lon2) & is.finite(lat2)
      same <- ok & (lon1 == lon2 & lat1 == lat2)

      rad <- pi / 180
      phi1 <- lat1[ok] * rad
      phi2 <- lat2[ok] * rad
      dlam <- (lon2[ok] - lon1[ok]) * rad

      y <- sin(dlam) * cos(phi2)
      x <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dlam)
      theta <- atan2(y, x)

      out[i[ok]] <- (theta * 180 / pi + 360) %% 360
      out[i[same]] <- NA_real_
      out
    }

    # Example: cb is a data.frame with columns X (lon) and Y (lat)
    cb <- as.data.frame(sf::st_coordinates(ca_sp_4326))
    cb$bearing <- bearing_seq(cb, "X", "Y")

    cb$bearing <- ifelse(cb$bearing < 0, 360 - abs(cb$bearing), cb$bearing)

    coord_all$bearing_next <- cb$bearing

    bearings <- cb$bearing

    # left bearing
    bl <- bearings - 90
    bl <- ifelse(bl < 0, 360 - abs(bl), bl)
    bl <- ifelse(bl > 360, abs(bl) - 360, bl)

    br <- bearings + 90
    br <- ifelse(br < 0, 360 - abs(br), br)
    br <- ifelse(br > 360, abs(br) - 360, br)

    coord_all$bearing_left <- bl
    coord_all$bearing_right <- br

    cb$downstream_bearing <- bearings
    cb$bl <- bl
    cb$br <- br
    cb$id <- 1:nrow(cb)

    # Generate perpendicular line based on bearings
    perp_lines <- function(x, cross_profile_length) {

      #  print(x)

      xcrd <- data.frame(X = x[1], Y = x[2])

      xcrd_sf <- sf::st_as_sf(xcrd, coords = c("X", "Y"), crs = 4326)

      if(is.na(x["bl"])) {
        return(NULL)
      }

      point_left <- dest_point_sf(pts = xcrd_sf,
                                  bearing_deg = x["bl"],
                                  distance_m = cross_profile_length)
      point_right <- dest_point_sf(pts = xcrd_sf,
                                   bearing_deg = x["br"],
                                   distance_m = cross_profile_length)

      point_left  <- as.data.frame(sf::st_coordinates(point_left))
      point_right <- as.data.frame(sf::st_coordinates(point_right))

      mini_line <- rbind(point_left, point_right)
      mini_line$mgroup <- 1

      lsf <- mini_line %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
        dplyr::group_by(mgroup) %>%
        dplyr::summarize(do_union = FALSE) %>%  # do_union=FALSE doesn't work as well
        sf::st_cast("LINESTRING")

      lsf <- sf::st_as_sf(lsf)

      lsf$id <- as.numeric(x["id"])
      lsf$downstream_bearing <- as.numeric(x["downstream_bearing"])
      lsf$bearing1 <- as.numeric(x["bl"])
      lsf$bearing2 <- as.numeric(x["br"])

      return(lsf)

    }

    # Apply to all points
    plines <-
      apply(cb, 1, perp_lines, cross_profile_length = cross_profile_length /
              2)

    plines_merge <- do.call("rbind", plines)

    # plot(sf::st_geometry(plines_merge))

    sf::st_crs(plines_merge) <- 4326

    plines_merge <- sf::st_transform(plines_merge, epsg)

    # plot(sf::st_geometry(plines_merge))

    # Add additional attribute data from other objects

    plines_merge$l_id <-  line_id

    pt_data <- these_pts
    sf::st_geometry(pt_data) <- NULL

    pt_data$l_id <- NULL
    plines_merge2 <- merge(plines_merge, pt_data,
                           by.x = "id", by.y = "p_id", all.x = TRUE)

    plines_merge2$bearing_next <- plines_merge2$downstream_bearing
    plines_merge2$p_id <- plines_merge2$id
    plines_merge2$id <- NULL

    add_lines <- plines_merge2[, c("l_id",
                                   "p_id",
                                   "group",
                                   "distance_m",
                                   "bearing_next",
                                   "bearing1",
                                   "bearing2")]

    all_lines[[j]] <- add_lines


  }

  output <- do.call("rbind", all_lines)

  # Assign projection
  sf::st_crs(output) <- epsg

  return(output)
}
