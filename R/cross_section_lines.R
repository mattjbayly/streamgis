#' Perpendicular Cross-Section Lines
#'
#' @description Generate perpendicular cross-sectional lines to stream
#'
#' @details Function takes a stream center line object and output from
#' `points_on_line` (or user supplied points) to generate perpendicular
#' lines at sample points.
#'
#' @param center_line sf dataframe. Stream center line spatial data. Must be
#' imported as an sf datamfrace object of LINESTRING or MULTILINESTRING. The
#' object must have a column named `id` to represent the id field. The object
#' must also be projected to a local UTM projection with units of meters.
#' @param points sf dataframe. Stream sample points returned from
#' from `points_on_line` or user-supplied points at which to create
#' perpendicular profiles.
#' @param  cross_profile_length Numeric. Length (in meters) of cross-sectional
#' profile lines. Total length of profile. Divide by two for center to edge
#' @param epsg Numeric. EPSG code for local UTM projection system (see:
#' https://spatialreference.org/ref/epsg/ for details).
#'
#' @returns An sf dataframe object of perpendicular cross-sectional profile
#' lines relative to stream centerline.
#'
#' @examples
#' \dontrun{
#'
#' library(streamgis)
#' # Import a simple stream center line
#' # center_line <- st_read("./path/to/my/file.gpkg", layer = "layer name")
#' # or use default provided for tutorial
#' fname <- system.file("extdata", "center_line.gpkg", package="streamgis")
#' center_line <- sf::st_read(fname)
#' plot(sf::st_geometry(center_line))
#'
#' # Sample points along line
#' pol <- suppressWarnings({ points_on_line(center_line,
#'   point_spacing = 100, epsg = 26910) })
#'
#' csl <- cross_section_lines(center_line = center_line,
#'   points = pol,
#'   cross_profile_length = 250,
#'   epsg = 26910)
#'
#' # Plot to visualize
#' plot(sf::st_geometry(center_line[center_line$id == 1, ]))
#' plot(sf::st_geometry(pol[pol$l_id == 1, ]), add = TRUE, col = "red")
#' plot(sf::st_geometry(csl[csl$l_id == 1, ]), add = TRUE, col = "blue")
#'
#' }
#'
#' @export
cross_section_lines <- function(center_line = NA,
                                points = NA,
                                cross_profile_length = 250,
                                epsg = 26910) {
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

    # Need class sp for some functions
    linesp <- methods::as(line, "Spatial")

    # Get the points linked to current line segment
    line_id <- unique(line$id)[1]

    these_pts <- points[which(points$l_id == line_id), ]

    coord_all <- sf::st_coordinates(these_pts)
    coord_all <- as.data.frame(coord_all)

    # Convert to lat long for bearing
    ca_sp <- coord_all
    sp::coordinates(ca_sp) <- ~ X + Y
    p4s <- suppressWarnings({
      sp::proj4string(linesp)
    })
    sp::proj4string(ca_sp) <- p4s
    ca_sp_4326 <-
      sp::spTransform(ca_sp, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    cb <- sp::coordinates(ca_sp_4326)
    cb <- as.data.frame(cb)


    bearings <-
      geosphere::bearing(cb[1:(nrow(cb) - 1), ], cb[2:(nrow(cb)), ])
    bearings <- c(bearings, bearings[length(bearings)])

    bearings <- ifelse(bearings < 0, 360 - abs(bearings), bearings)

    coord_all$bearing_next <- bearings

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
      xcrd <- data.frame(X = x["X"],
                         Y = x["Y"])

      point_left <- geosphere::destPoint(xcrd, b = x["bl"],
                                         d = cross_profile_length)

      point_right <- geosphere::destPoint(xcrd, b = x["br"],
                                          d = cross_profile_length)

      point_left <- as.data.frame(point_left)

      point_right <- as.data.frame(point_right)

      mini_line <- rbind(point_left, point_right)

      line_obj <- sp::Line(mini_line)

      lines_obj <- sp::Lines(list(line_obj), ID = 1)

      l_obj <- sp::SpatialLines(list(lines_obj))

      sp::proj4string(l_obj) <-
        "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

      lsf <- sf::st_as_sf(l_obj)

      lsf$id <- x["id"]
      lsf$downstream_bearing <- x["downstream_bearing"]
      lsf$bearing1 <- x["bl"]
      lsf$bearing2 <- x["br"]

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

    plines_merge$p_id <- pt_data$p_id
    plines_merge$group <- pt_data$group
    plines_merge$distance_m <- pt_data$distance_m

    plines_merge$bearing_next <- plines_merge$downstream_bearing

    plines_merge$id <- NULL

    add_lines <- plines_merge[, c("l_id",
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
