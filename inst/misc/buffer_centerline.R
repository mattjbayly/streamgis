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
buffer_centerline <- function(center_line = NA,
                              points = NA,
                              lines = NA,
                              buffer_dist = 350,
                              epsg = 26910) {

  #===================================
  #===================================
  #===================================
  #===================================
  #===================================
  points <- suppressWarnings({ points_on_line(center_line,
                                              point_spacing = 150,
                                              epsg = 26910) })
  head(points, 3)

  csl <- cross_section_lines(
    center_line = center_line,
    points = points,
    cross_profile_length = 400,
    epsg = 26910)

  head(csl, 3)

  plot(sf::st_geometry(center_line[center_line$id == 1, ]))
  plot(sf::st_geometry(points[points$l_id == 1, ]), add = TRUE, col = "red")
  plot(sf::st_geometry(csl[csl$l_id == 1, ]), add = TRUE, col = "blue")


  buffer_dist <- 50
  buff <- sf::st_buffer(center_line, buffer_dist)
  epsg = 26910
  #===================================
  #===================================
  #===================================
  #===================================
  #===================================


  # Dissolve buffer
  buff$diss <- 1

  b2 <- dplyr::group_by(buff)
  buff_diss <- dplyr::summarise(b2)

  nrow(lines)

  buff_diss_raw <- buff_diss


  # Store object here.
  build_slits <- list()
  coutner <- 1

  for(i in 1:nrow(lines)) {
  #for(i in 1:) {

    this_line <- lines[i, ]

    if(i > 1) {

      # Remove sections already split
      prev_line <- lines[i - 1, ]

      # trim new line by previous line
      fix <- lwgeom::st_split(this_line, prev_line)
      fix <- sf::st_collection_extract(fix, "LINESTRING")

      if(nrow(fix) > 1) {
        # gather segment linked to centerline
        int_ll <- sf::st_intersects(fix, center_line)
        int_ll <- lapply(int_ll, length)
        int_ll <- unlist(int_ll)
        this_line <- fix[which(int_ll == 1), ]
      }

      prev_line <- st_buffer(prev_line, 0.5)
      int1 <- lapply(sf::st_intersects(buff_diss, prev_line), length)
      int2 <- lapply(sf::st_intersects(buff_diss, this_line), length)
      int1 <- unlist(int1); int2 <- unlist(int2)

      if(any(int2 == 2)) { next }

      previous_int <- which(int2 == 0 & int1 == 1)
      continue_int <- which(int2 == 1 & int1 == 1)

      buff_diss_store <- buff_diss[previous_int, ]

      # Add and build object
      build_slits[[coutner]] <- buff_diss_store
      coutner <- coutner + 1

      if(nrow(buff_diss[continue_int, ]) == 0) {
        next
      }

      # update object
      buff_diss <- buff_diss[continue_int, ]

    }

    buff_diss <- lwgeom::st_split(buff_diss, this_line)
    buff_diss <- sf::st_collection_extract(buff_diss, "POLYGON")


    plot(sf::st_geometry(this_line))
    plot(sf::st_geometry(buff_diss_raw), add = TRUE, col = 'lightgrey')
    plot(sf::st_geometry(buff_diss), add = TRUE, col = 'darkgrey')
    plot(sf::st_geometry(this_line), add = TRUE, col = 'red')

    Sys.sleep(0.1)






  }



  # Fix intersection cross lines
  # lines = csl
  int <- sf::st_intersection(lines)

  lines_fix <- lines

  if(nrow(int) > 0) {
    int_sub <- int[sf::st_geometry_type(int) == "POINT", ]
    if(nrow(int_sub) > 0) {
      for(f in 1:nrow(int_sub)) {

        this_int <- int_sub[f, ]
        bad_lines <- this_int$origins[[1]]

        if(length(bad_lines) != 2) {
          stop("three-way line intersection not handled")
        }

        line_1 <- lines_fix[bad_lines[1], ]
        line_2 <- lines_fix[bad_lines[2], ]

        p <- this_int
        p$origins <- NULL

        q <- lwgeom::st_split(line_2, p)
        q <- st_collection_extract(q, "LINESTRING")
        plot(st_geometry(q), axes = T, col = 1:2, lwd = 3)
        plot(st_geometry(line_1), axes = T, col = 1:2, lwd = 3, add= TRUE)
        plot(st_geometry(line_2), axes = T, col = 1:2, lwd = 3, add= TRUE)


      }
    }

  }





  int_sub <- int[sf::st_geometry_type(int) == "POINT", ]
  # sf::st_write(int_sub, dsn = "int.gpkg", delete_dsn = TRUE)

  mapview(int)



}
