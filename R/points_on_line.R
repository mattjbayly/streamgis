#' Generate Points on Line
#'
#' @description Samples points on stream centerline
#'
#' @details Samples points at a regular interval along stream center line
#' these points can be used to construct stream cross sectional profiles
#' and provide utility for other applications in hydrology and aquatic
#' habitat assessments. Output fields include line segement id (l_id),
#' point id (p_id, unique to each line segement). Distance (for each point
#' on each line segement from line segment origin) and group according to
#' whether point represents are `start` point, `end` point or `sample` point.
#' Filter out start and end points if they are not needed.
#'
#' @param center_line sf dataframe. Stream center line spatial data. Must be
#' imported as an sf datamfrace object of LINESTRING or MULTILINESTRING. The
#' object must have a column named `id` to represent the id field. The object
#' must also be projected to a local UTM projection with units of meters.
#' @param point_spacing Numeric. Point spacing distance in units of meters.
#' @param epsg Numeric. EPSG code for local UTM projection system (see:
#' https://spatialreference.org/ref/epsg/ for details).
#' @param reverse_coordinates Logical. If TRUE, coordinates will be reversed.
#'
#' @returns An sf dataframe object of points sampled along stream center lines.
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
#' # Plot to visualize
#' plot(sf::st_geometry(center_line[center_line$id == 1, ]))
#' plot(sf::st_geometry(pol[pol$l_id == 1, ]), add = TRUE, col = "red")
#'
#'
#' }
#'
#' @export
points_on_line <- function(center_line = NA,
                           point_spacing = 50,
                           epsg = 26910,
                           reverse_coordinates = FALSE) {

  # Cut with cross sectional profiles
  if (nrow(center_line) < 1) {
    stop("Center line is empty")
  }

  if (epsg == 26910) {
    message("Assigning EPSG:26910 UTM UTM zone 10N for input data")
  }

  # Assign projection if not already set
  sf::st_crs(center_line) <- epsg


  all_points <- list()

  # Loop through multiline string
  for (j in 1:nrow(center_line)) {

    # j = 1 # debugging
    line <- center_line[j, ]
    line <- suppressWarnings({
      sf::st_cast(line, "LINESTRING")
    })

    if(reverse_coordinates) {
      line <- sf::st_reverse(line)
      # plot(st_geometry(line))
      # plot(st_geometry(new_line))
    }


    # Need class sp for some functions
    linesp <- methods::as(line, "Spatial")

    segment_length <- rgeos::gLength(linesp)

    if(point_spacing > segment_length) {

      print(paste0("Line segment length is ", round(segment_length, 0), " m "))
      print("Sampling single point...")
      # Single sample on line
      crds <- sf::st_coordinates(line)
      crds <- as.data.frame(crds)
      mdist <- as.numeric(sf::st_length(line))
      x_set <- crds$X[c(1, nrow(crds))]
      y_set <- crds$Y[c(1, nrow(crds))]

      # Build df
      add_pt <- data.frame(l_id = line$id, p_id = c(1,2),
                           group = c("start", "end"),
                           distance_m = c(0, mdist), X = x_set, Y = y_set)

      all_points[[j]] <- add_pt
      warning("Point spacing cannot be longer than line segment length")
      next
    }

    # Determine number of sample points to generate
    d_int  <-  rgeos::gLength(linesp) / point_spacing

    # Find distance of last sample point
    max_int <- floor(d_int)
    max_dist <- max_int * point_spacing

    # Sequence of point spacing
    p_distances <- seq(point_spacing, max_dist, by = point_spacing)

    # Sample points along line with origin starting at zero
    p_on_line <- rgeos::gInterpolate(linesp,  d = p_distances)

    # Sample points at regular interval (random origin)
    # p_on_line <- sp::spsample(linesp, n = d_int, type = "regular")

    # Create coordinates for start and end points of each line segment
    coords <- sf::st_coordinates(line)
    coords <- as.data.frame(coords)

    check_1 <- unique(coords$L1)
    check_2 <- unique(coords$L2)

    if (check_1 != 1) {
      stop("MULTILINESTRING geom not accounted for...")
    }

    if (length(check_2) > 0) {
      if (check_2 != 1) {
        stop("MULTILINESTRING geom not accounted for...")
      }
    }


    coords_start <- coords[1, ]
    coords_end <- coords[nrow(coords), ]

    coords_end <- as.data.frame(coords_end)
    coords_end <- coords_end[, 1:2]
    colnames(coords_end) <- c("X", "Y")

    coords_start <- as.data.frame(coords_start)
    coords_start <- coords_start[, 1:2]
    colnames(coords_start) <- c("X", "Y")

    coords_start$group <- "start"
    coords_end$group <- "end"


    coord_samp <- sp::coordinates(p_on_line)
    coord_samp <- as.data.frame(coord_samp)
    coord_samp <- coord_samp[, 1:2]
    colnames(coord_samp) <- c("X", "Y")
    coord_samp$group <- "sample"

    coord_all <- rbind(coords_start, coord_samp)
    coord_all <- rbind(coord_all, coords_end)

    # Convert back to sp point
    ca_sp <- coord_all
    sp::coordinates(ca_sp) <- ~ X + Y


    # Calculate distances to point on line
    m_distances <-
      rgeos::gProject(linesp, ca_sp, normalized = FALSE)
    m_distances <- round(m_distances, 2)

    coord_all$distance_m <- m_distances

    if (nrow(coord_all) > 5) {
      dist_check <- coord_all$distance_m[4] - coord_all$distance_m[3]
      dist_check / point_spacing
      point_spacing / dist_check
      diff <- abs(1 - (point_spacing / dist_check))
      if (diff > 0.1) {
        stop("Large error in distances over 10 percent")
      }
    }

    # Return object as sf points object
    coord_all$p_id <- 1:nrow(coord_all)


    # Add id for parent line segment
    coord_all$l_id <- line$id

    # Fix column order
    coord_all <- coord_all[, c("l_id", "p_id", "group", "distance_m", "X", "Y")]


    # Add to list object
    all_points[[j]] <- coord_all

    # run for next segment

  }

  output <- do.call("rbind", all_points)

  # Make spatial
  sp::coordinates(output) <- ~X+Y

  output <- sf::st_as_sf(output)

  # Assign projection
  sf::st_crs(output) <- epsg

  return(output)

}
