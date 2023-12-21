#' Get UTM epsg
#'
#' @description Gets the local UTM epsg code
#'
#' @param sf_object sf dataframe.
#'
#' @details This function takes a sf dataframe object and returns the local UTM.
#' If single polygon or line epsg code will be sampled from the centroid. If
#' sf_object consists of multiple objects in the form the bounding box
#'
#' @returns Numeric epsg code.
#'
#' @examples
#' \dontrun{
#'
#' library(streamgis)
#' # Import a simple stream center line
#' # center_line <- st_read("./path/to/my/file.gpkg", layer = "layer name")
#' fname <- system.file("extdata", "center_line.gpkg", package="streamgis")
#' center_line <- sf::st_read(fname)
#' get_utm_epsg(sf_object = center_line)
#' }
#'
#' @export
get_utm_epsg <- function(sf_object) {

  # Transform polygon to 4326 lat long
  poly <- sf_object

  if(nrow(poly) > 1) {
    # bbox
    bbox <- suppressWarnings({
      sf::st_bbox(poly)
    })
    bbox <- sf::st_as_sfc(bbox)
    poly_ll <- sf::st_transform(bbox, 4326)
    # Take centroid
    cent <- suppressWarnings({
      sf::st_centroid(poly_ll)
    })
  } else {
    poly_ll <- sf::st_transform(poly, 4326)
    # Take centroid
    cent <- suppressWarnings({
      sf::st_centroid(poly_ll)
    })
  }

  coords <- sf::st_coordinates(cent)
  coords <- as.matrix(coords)
  longitude <- coords[, "X"]

  # Determine UTM zone for this longitude
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }

  utm_zone <- long2UTM(longitude)

  # target 32610 format
  utm_zone <- as.character(utm_zone)
  if(nchar(utm_zone) == 1) {
    utm_zone <- paste0("0", utm_zone)
  }

  epsg <- paste0("326", utm_zone)

  if(nchar(epsg) != 5) {
    stop("Out of range 1...")
  }

  epsg <- as.numeric(epsg)

  if(epsg < 32606) {
    stop("Out of range 2...")
  }

  if(epsg > 32615) {
    stop("Out of range 3...")
  }

  return(epsg)

}
