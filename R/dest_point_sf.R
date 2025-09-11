#' Destination point on an ellipsoid (internal)
#'
#' @description
#' Compute destination point(s) from a start point, initial bearing (degrees),
#' and distance (meters) on a reference ellipsoid using Vincenty's direct
#' method, with a spherical great-circle fallback for non-convergent cases.
#'
#' @details
#' - Input points must be in a geographic CRS (lon/lat; e.g., EPSG:4326).
#' - Accepts an `sf` object with point geometry or an `sfc_POINT`.
#' - Defaults use WGS84 (`a = 6378137`, `f = 1/298.257223563`).
#' - If `bearing_deg` or `distance_m` are length-1, they are recycled to the
#'   number of input points.
#'
#' @param pts An `sf` object with point geometry or an `sfc_POINT` in a
#'   geographic CRS (lon/lat).
#' @param bearing_deg Numeric vector of initial bearings (degrees, clockwise
#'   from north).
#' @param distance_m Numeric vector of distances in meters.
#' @param a Semi-major axis of the reference ellipsoid (meters). Default WGS84.
#' @param f Flattening of the reference ellipsoid. Default WGS84.
#' @param tol Convergence tolerance for Vincenty iteration.
#' @param maxit Maximum iterations for Vincenty solution.
#'
#' @return An `sfc_POINT` with the same geographic CRS as `pts`.
#'
#' @references
#' Vincenty, T. (1975). Direct and inverse solutions of geodesics on the
#' ellipsoid with application of nested equations. Survey Review, 23(176), 88-93.
#'
#' @keywords internal
#' @noRd
dest_point_sf <- function(pts, bearing_deg, distance_m,
                          a = 6378137.0, f = 1/298.257223563,
                          tol = 1e-12, maxit = 100L) {

  bearing_deg <- as.numeric(bearing_deg)
  # Accept sf or sfc POINT
  g <- sf::st_geometry(pts)
  if (is.null(g)) g <- pts
  if (!inherits(g, "sfc")) stop("`pts` must be sf or sfc.")
  if (!inherits(g, "sfc_POINT")) g <- sf::st_cast(g, "POINT", warn = FALSE)
  if (is.na(sf::st_crs(g)) || !sf::st_is_longlat(g))
    stop("`pts` must have a geographic CRS (e.g., EPSG:4326).")

  xy <- sf::st_coordinates(g)
  n  <- nrow(xy)
  if (length(bearing_deg) == 1L) bearing_deg <- rep(bearing_deg, n)
  if (length(distance_m) == 1L)  distance_m  <- rep(distance_m,  n)
  stopifnot(length(bearing_deg) == n, length(distance_m) == n)

  # Constants
  b <- a * (1 - f)

  # Inputs in radians
  deg2rad <- pi / 180
  rad2deg <- 180 / pi
  phi1    <- xy[, 2] * deg2rad
  lambda1 <- xy[, 1] * deg2rad
  alpha1  <- bearing_deg * deg2rad
  s       <- distance_m

  # Reduced latitude
  U1        <- atan((1 - f) * tan(phi1))
  sinU1     <- sin(U1)
  cosU1     <- cos(U1)
  sin_alpha1 <- sin(alpha1)
  cos_alpha1 <- cos(alpha1)

  # Sigma1 and alpha
  sigma1    <- atan2(tan(U1), cos_alpha1)
  sin_alpha <- cosU1 * sin_alpha1
  cos2_alpha <- 1 - sin_alpha^2

  # Auxiliary values
  u2 <- cos2_alpha * (a^2 - b^2) / (b^2)
  A  <- 1 + u2/16384 * (4096 + u2 * (-768 + u2 * (320 - 175*u2)))
  B  <- u2/1024  * (256  + u2 * (-128 + u2 * (74  - 47*u2)))

  # Iterate sigma
  sigma <- s / (b * A)
  for (iter in seq_len(maxit)) {
    two_sigma_m  <- 2 * sigma1 + sigma
    cos2_sigma_m <- cos(two_sigma_m)
    sin_sigma    <- sin(sigma)
    cos_sigma    <- cos(sigma)
    delta_sigma  <- B * sin_sigma * (cos2_sigma_m + (B/4) * (cos_sigma * (-1 + 2 * cos2_sigma_m^2) -
                                                               (B/6) * cos2_sigma_m * (-3 + 4 * sin_sigma^2) * (-3 + 4 * cos2_sigma_m^2)))
    sigma_new <- s/(b*A) + delta_sigma
    if (max(abs(sigma_new - sigma), na.rm = TRUE) < tol) { sigma <- sigma_new; break }
    sigma <- sigma_new
  }
  converged <- iter < maxit

  # Compute latitude phi2
  sin_sigma <- sin(sigma); cos_sigma <- cos(sigma)
  tmp       <- sinU1 * sin_sigma - cosU1 * cos_sigma * cos_alpha1
  sin_phi2  <- sinU1 * cos_sigma + cosU1 * sin_sigma * cos_alpha1
  cos_phi2  <- sqrt(sin_alpha^2 + tmp^2)
  phi2      <- atan2(sin_phi2, (1 - f) * cos_phi2)

  # Compute longitude difference
  lambda <- atan2(sin_sigma * sin_alpha1, cosU1 * cos_sigma - sinU1 * sin_sigma * cos_alpha1)
  C      <- (f/16) * cos2_alpha * (4 + f * (4 - 3 * cos2_alpha))
  two_sigma_m <- 2 * sigma1 + sigma
  L <- lambda - (1 - C) * f * sin_alpha *
    (sigma + C * sin_sigma * (cos(two_sigma_m) + C * cos_sigma * (-1 + 2 * cos(two_sigma_m)^2)))
  lambda2 <- lambda1 + L

  # Fallback to spherical great-circle for any non-converged cases
  if (any(!converged)) {
    idx   <- which(!converged)
    delta <- s[idx] / a
    phi1i    <- phi1[idx]; lambda1i <- lambda1[idx]; alpha1i <- alpha1[idx]
    sin_phi2i <- sin(phi1i) * cos(delta) + cos(phi1i) * sin(delta) * cos(alpha1i)
    phi2i     <- asin(sin_phi2i)
    yi        <- sin(alpha1i) * sin(delta) * cos(phi1i)
    xi        <- cos(delta) - sin(phi1i) * sin_phi2i
    lambda2i  <- lambda1i + atan2(yi, xi)
    phi2[idx]    <- phi2i
    lambda2[idx] <- lambda2i
  }

  # Back to degrees, wrap lon to [-180, 180)
  lat <- phi2 * rad2deg
  lon <- ((lambda2 * rad2deg + 540) %% 360) - 180

  sf::st_sfc(
    mapply(function(x, y) sf::st_point(c(x, y)), lon, lat, SIMPLIFY = FALSE),
    crs = sf::st_crs(g)
  )
}
