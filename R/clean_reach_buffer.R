#' Clean Reach Buffer
#'
#' @description Variable buffer cut to stream reaches
#'
#' @details Function takes a stream center line object, a section of cross-
#' sectional lines either from `cross_section_lines` or supplied by user and
#' creates a clean buffer around the stream center line, clipped neatly to
#' reach end points.
#'
#' @param center_line sf dataframe. Stream center line spatial data. Must be
#' imported as an sf dataframe object of LINESTRING or MULTILINESTRING. The
#' object must also be projected to a local UTM projection with units of meters.
#' @param buffer_width Numeric (fixed) or column name (variable). If numeric
#' value is supplied, the buffer width will be fixed and continuously
#' applied across the stream line. If a column name is provided (linked to the
#' `center_line` object), the buffer width will be fixed according values
#' provided in the `center_line`dataframe.
#' @param  cross_section_lines sf dataframe. Object returned from
#' `cross_section_lines` or user-supplied perpendicular lines to cut buffer.
#' @param  us_distance_colname Character string. Optional. Column name within
#' the `cross_section_lines` object specifying the upstream distance of the
#' cross section. Attribute IDs will be joined to polygons by upstream line segment.
#' If `us_distance_colname` is missing, linkages will be random.
#' @param epsg Numeric. EPSG code for local UTM projection system (see:
#' https://spatialreference.org/ref/epsg/ for details).
#'
#' @returns An sf dataframe object of of stream buffers neatly clipped to reach
#' endpoints.
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
#' cross_section_lines <- cross_section_lines(center_line = center_line,
#'   points = pol,
#'   cross_profile_length = 250,
#'   epsg = 26910)
#'
#' # Make distance field continuious
#' fix_order <- cross_section_lines[order(cross_section_lines$l_id,
#' cross_section_lines$p_id), ]
#'
#' fix_order$us_distance_m <- cumsum(fix_order$distance_m)
#' plot(fix_order['us_distance_m'])
#'
#' # Overwrite original
#' cross_section_lines <- fix_order
#'
#' # Apply clipped buffer
#' buff <- clean_reach_buffer(
#' center_line = center_line,
#' buffer_width = 50,
#' cross_section_lines = cross_section_lines,
#' us_distance_colname = 'us_distance_m',
#' epsg = 26910)
#'
#'
#' # Plot to visualize
#' plot(sf::st_geometry(center_line[center_line$id == 1, ]))
#' plot(sf::st_geometry(pol[pol$l_id == 1, ]), add = TRUE, col = "red")
#' plot(sf::st_geometry(cross_section_lines[cross_section_lines$l_id == 1, ]),
#' add = TRUE, col = "green")
#' plot(sf::st_geometry(buff[buff$l_id == 1, ]),
#' add = TRUE, col = "grey")
#'
#' }
#'
#' @export
clean_reach_buffer <- function(center_line = NA,
                               buffer_width = 100,
                               cross_section_lines = NA,
                               us_distance_colname = "us_distance_m",
                               epsg = 26910) {
  # ============================================
  # Create buffer
  # ============================================

  if (is.numeric(buffer_width)) {
    cbuff <- sf::st_buffer(center_line, dist = buffer_width)
    max_buff_length <- buffer_width
  } else {
    if (buffer_width %in% colnames(center_line)) {
      char_buff <- center_line[, buffer_width]
      sf::st_geometry(char_buff) <- NULL
      char_buff <- char_buff[, 1]
      char_buff <- ifelse(is.na(char_buff), 0, char_buff)
      char_buff <- ifelse(char_buff < 0, 0, char_buff)
      max_buff_length <- max(char_buff[char_buff > 0])
      center_line$my_buff <- char_buff
      cbuff <-
        sf::st_buffer(center_line, dist = center_line$my_buff)
    } else {
      stop(paste0("Column ", buffer_width, " not found in centerlines"))
    }
  }


  # Dissolve buffer
  cbuff$mjx_diss <- "1"
  # plot(sf::st_geometry(cbuff[1,]))
  # Drop empty geometry
  cbuff <- cbuff[!sf::st_is_empty(cbuff), , drop = FALSE]

  # Check for bad length clip - buffer too wide
  cline_legnth <-
    min(sf::st_length(cross_section_lines), na.rm = TRUE)
  cline_legnth <- as.numeric(cline_legnth)
  if (max_buff_length > cline_legnth) {
    warning("Buffer too wide and cut lines too short. Missing sections...")
  }

  # Dissolve
  b2 <- dplyr::group_by(cbuff, "mjx_diss")
  buffer_raw <- dplyr::summarise(b2)
  buffer_raw$mjx_diss <- NULL
  buffer_raw$`"mjx_diss"` <- NULL
  # plot(sf::st_geometry(buffer_raw))


  # ============================================
  # Clip buffer by lines
  # ============================================

  # Only keep valid cross sections
  csl_fix <- cross_section_lines
  build_list <- list()
  # New buffer object
  buff_diss <- buffer_raw

  # Trim long tails on cross sections
  buffer_trim <- sf::st_buffer(buffer_raw, 0.1)
  csl_fix <-
    suppressWarnings({
      sf::st_intersection(csl_fix, buffer_trim)
    })

  # Convert multipart to single part
  csl_fix <-
    suppressWarnings({
      sf::st_cast(csl_fix, "MULTILINESTRING")
    })
  csl_fix <-
    suppressWarnings({
      sf::st_cast(csl_fix, "LINESTRING")
    })

  # Drop segments not crossing centerline
  int <-
    suppressWarnings({
      sf::st_intersects(csl_fix, center_line)
    })
  int <- lapply(int, length)
  int <- unlist(int)
  csl_fix <- csl_fix[which(int == 1),]

  # Drop line segments with start end points
  buff_margin <- sf::st_cast(buffer_raw, "MULTILINESTRING")
  buff_margin <- sf::st_cast(buff_margin, "LINESTRING")
  buff_margin <- sf::st_buffer(buff_margin, 0.2)

  # Start point on line
  pt_start <- lwgeom::st_startpoint(csl_fix)
  int <- sf::st_intersects(pt_start, buff_margin)
  int <- lapply(int, length)
  int <- unlist(int)
  csl_fix <- csl_fix[which(int == 1),]

  # End point on line
  pt_end <- lwgeom::st_endpoint(csl_fix)
  int <- sf::st_intersects(pt_end, buff_margin)
  int <- lapply(int, length)
  int <- unlist(int)
  csl_fix <- csl_fix[which(int == 1),]


  # Drop intersecting lines
  int <- sf::st_intersects(csl_fix, csl_fix)
  intb <- lapply(int, length)
  intb <- unlist(intb)

  csl_fix_int <- csl_fix[which(intb > 1),]
  bad_lines <- int[which(intb > 1)]
  keep_first <- lapply(bad_lines, function(x) {
    return(x[1])
  })
  keep_first <- unlist(keep_first)

  merge_sets <- c(which(intb == 1), keep_first)

  # Drop cross lines and keep good lines
  csl_fix <- csl_fix[merge_sets,]

  if (nrow(csl_fix) == 0) {
    stop("Buffer too wide relative to cross section lines...")
  }

  # Split polygon by clean lines
  buff_split <- lwgeom::st_split(buffer_raw, csl_fix)

  # Remove sliver lines
  buff_diss <- sf::st_collection_extract(buff_split, "POLYGON")


  # Determine what distance field to use
  if (is.na(us_distance_colname)) {
    csl_fix$tmp_xxyd <- 1:nrow(csl_fix)
  } else {
    if (us_distance_colname %in% colnames(csl_fix)) {
      vals <- csl_fix[, us_distance_colname]
      sf::st_geometry(vals) <- NULL
      vals <- vals[, 1]
      csl_fix$tmp_xxyd <- vals
    } else {
      stop("us_distance_colname not present in cross_section_lines...")
    }
  }


  # Join line attributes to polygons
  # Join by upstream distance attributes
  # determine if ditance column is supplied... or use
  csl_fix_b <- sf::st_buffer(csl_fix, 0.05)
  int <- sf::st_intersects(buff_diss, csl_fix_b)

  # Get upstream line segment
  get_us <- function(x, csl_fix) {
    x <- unlist(x)
    y <- csl_fix[x,]
    target <- max(y$tmp_xxyd, na.rm = TRUE)
    us_keep <- y[which(y$tmp_xxyd == target),]
    us_keep <- us_keep[1,]
    sf::st_geometry(us_keep) <- NULL
    #ds_keep <- y[which.min(y$tmp_xxyd), ]
    #ds_keep <- ds_keep[1, ]
    return(us_keep)
  }

  us_segs <- lapply(int, get_us, csl_fix = csl_fix)
  us_segs <- do.call("rbind", us_segs)
  us_segs$tmp_xxyd <- NULL

  buff_diss_data <- cbind(buff_diss, us_segs)

  # plot(buff_diss_data['us_distance_m'], border = NA)

  return(buff_diss_data)


}
