#' BCFWA Stream Geometry
#'
#' @description Extract geometry from BCFWA data for target river
#'
#' @details Extracts x,y and z geometry from the BCFWA (British Columbia
#' Freshwater Atlas) for a target river/creek of interest. Specify upstream
#' and downstream LFID (linear feature ID).
#' See FWA_STREAM_NETWORKS_SP.gdb at the FTP link.
#' (FTP: ftp://ftp.geobc.gov.bc.ca/sections/outgoing/bmgs/FWA_Public).
#' Additional steps involve reordering line features (where necessary).
#' Processing is done for a single
#'
#' @param bcfwa sf dataframe. Imported BCFWA streamline geometry. Import
#' directly as gdb or gpkg to avoid losing the z (or M) elevation geometry.
#' Do not convert to shapefile. Usually this object is imported by a BCFWA
#' group code. See example.
#' @param upstream numeric. Upstream linear
#' feature ID from BCFWA. See field (LINEAR_FEATURE_ID)... or spatial point
#' @param downstream numeric. Downstream linear feature ID
#' from BCFWA. See field (LINEAR_FEATURE_ID)... or spatial point
#' @param epsg Numeric. EPSG code for local UTM projection system (see:
#' https://spatialreference.org/ref/epsg/ for details).
#'
#' @returns An sf dataframe object of the target creek/river and a dataframe
#' of coordinates.
#'
#' @examples
#' \dontrun{
#' library(streamgis)
#' # Import BCFWA streamlines from FWA_STREAM_NETWORKS_SP.gdb
#' # Find target group code (layer) for area of interest (e.g., 'LNIC')
#' # center_line <- st_read("./path/to/BCFWA/FWA_STREAM_NETWORKS_SP.gdb",
#' # layer = "LNIC")
#'
#' # or continue with default provided for tutorial
#' fname <- system.file("extdata", "bcfwa.gpkg", package="streamgis")
#' bcfwa <- sf::st_read(fname)
#'
#' ds <- bcfwa_geometry(bcfwa = bcfwa,
#' upstream = 701794363,
#' downstream = 701773410,
#' epsg = 26910)
#'
#' # ds_path: is the original geometry
#' # coordinates: is the x,y,z coordinates
#' names(ds)
#'
#' # View the GIS data
#' plot(sf::st_geometry(ds$ds_path))
#'
#' # View coordinates for longitudial profile
#' df <- ds$coordinates
#' plot(df$us_distance_m/1000, df$Z, type = 'l',
#' xlab = "Upstream Distance (km)", ylab = "Elevation (m)")
#'
#' # Beginning of line
#' head(df, 3)
#'
#' # End of line
#' tail(df, 3)
#'
#' # Validate length
#' seg <- bcfwa[bcfwa$GNIS_NAME == "Coldwater River", ]
#' tot_length <- sf::st_length(seg)
#' as.numeric(sum(tot_length))
#'
#'
#'
#' }
#'
#' @export
bcfwa_geometry <- function(bcfwa = NA,
                           upstream = 701794363,
                           downstream = 701773410,
                           epsg = 26910) {

  # Drop any isolated segments
  bcfwa$FWA_WATERSHED_CODE <- as.character(bcfwa$FWA_WATERSHED_CODE)
  bcfwa$LOCAL_WATERSHED_CODE <- as.character(bcfwa$LOCAL_WATERSHED_CODE)
  bcfwa <- bcfwa[!(is.na(bcfwa$FWA_WATERSHED_CODE)), ]
  bcfwa <- bcfwa[!(is.na(bcfwa$LOCAL_WATERSHED_CODE)), ]
  bcfwa <- bcfwa[!(grepl("999-999999$", bcfwa$FWA_WATERSHED_CODE)), ]
  bcfwa <- bcfwa[!(grepl("999-999999$", bcfwa$LOCAL_WATERSHED_CODE)), ]


  # Local UTM zone
  bcfwa <- sf::st_transform(bcfwa, epsg)



  bcfwa <-
    suppressWarnings({
      sf::st_cast(bcfwa, "MULTILINESTRING")
    })
  bcfwa <-
    suppressWarnings({
      sf::st_cast(bcfwa, "LINESTRING")
    })

  # Add on line reach id (rid)
  bcfwa$rid <- 1:nrow(bcfwa)
  bcfwa$lfid <- bcfwa$LINEAR_FEATURE_ID

  # Drop z geometry
  bcfwa_bu <- bcfwa


  # Use end point to start at upstream side
  nodes <- lwgeom::st_endpoint(bcfwa_bu)
  nodes <- sf::st_as_sf(nodes)

  # add on ID fields
  nodes$rid <- bcfwa_bu$rid
  nodes$lfid <- bcfwa_bu$lfid
  nodes$name <- bcfwa_bu$rid

  # Lines are draw from downstream to upstream
  # start point is downstream end point is upstream

  int <- sf::st_intersects(nodes)
  int <- lapply(int, length)
  int <- unlist(int)

  if (max(int) > 1) {
    stop("Line draw direction variables. Some segments backwards")
  }



  # Build SF network
  net <- sfnetworks::as_sfnetwork(bcfwa_bu, directed = FALSE)
  net <- sfnetworks::activate(net, "edges")
  net <- dplyr::mutate(net, weight = sfnetworks::edge_length())


  # Downstream point
  if(class(downstream)[1] %in% c("numeric", "character")) {
    from_pt_ds <- nodes[nodes$lfid == downstream, ]
    from_pt_ds <- from_pt_ds[1, ]
  } else {
   # spatial
   from_pt_ds <- downstream
  }

  # Upstream point
  if(class(upstream)[1] %in% c("numeric", "character")) {
    to_pt_us <- nodes[nodes$lfid == upstream, ]
    to_pt_us <- to_pt_us[1, ]
  } else {
    # spatial
    to_pt_us <- upstream
  }


  # Find shortest path
  path <- sfnetworks::st_network_paths(net,
                                       from = from_pt_ds,
                                       to = to_pt_us)


  ds_route_ids <- path$edge_paths[[1]]
  ds_route <- bcfwa_bu[ds_route_ids, ]


  # Get route geometry
  coords <- sf::st_coordinates(ds_route)
  coords <- as.data.frame(coords)


  # Calculate Cartesian distance between points
  coords$dist <- NA
  n <- nrow(coords)
  coords$dist[2:n] <- sqrt(((coords$X[2:n] - coords$X[1:n-1]) ^ 2) +
                             ((coords$Y[2:n] - coords$Y[1:n-1]) ^ 2))

  # Cumulative upstream distance
  coords$dist <- ifelse(is.na(coords$dist), 0, coords$dist)
  coords$us_distance_m <- cumsum(coords$dist)


  s1 <- sum(coords$dist, na.rm = TRUE)
  s2 <- as.numeric(sum(sf::st_length(ds_route)))
  diff <- abs(1 - s1/s2)
  if(diff > 0.05) {
    warning("Cumulative length error greater than 5%...")
  }


  # Build return object
  ret_obj <- list()


  ret_obj$ds_path <- ds_route
  ret_obj$coordinates <- coords

  return(ret_obj)




}
