#' Query BC waterbody names and drainage paths
#'
#' @description Look up waterbodies by name in the BC Freshwater Atlas
#' waterbody names table. Supports two modes: find all waterbodies that drain
#' *into* a target waterbody (\code{"upstream"}), or retrieve the downstream
#' drainage path *from* a target waterbody (\code{"downstream"}).
#'
#' @param waterbody Character string. Name of the target waterbody to query
#'   (matched against the \code{GNIS_NAME} column). Matching is
#'   case-insensitive.
#' @param mode Character string. One of \code{"upstream"} or
#'   \code{"downstream"}.
#'   \itemize{
#'     \item \code{"upstream"} — returns all waterbody records whose
#'       \code{DOWNSTREAM_PATH} contains the target waterbody name (i.e.
#'       waterbodies that eventually drain into the target).
#'     \item \code{"downstream"} — returns the record(s) for the target
#'       waterbody itself, which include its \code{DOWNSTREAM_PATH} field
#'       describing the drainage route to the ocean or terminal waterbody.
#'   }
#' @param gnis_id Optional integer or character vector of GNIS IDs to filter
#'   results.
#' @param fwa_watershed_code Optional character vector of FWA watershed codes
#'   to filter results.
#' @param waterbody_type Optional character vector of waterbody types to filter
#'   results. Valid types: \code{"Creek"}, \code{"River"}, \code{"Lake"},
#'   \code{"Slough"}, \code{"Pond"}, \code{"Channel"}, \code{"Unknown"}.
#'
#' @return A \code{data.frame} with the following columns (subset of the
#'   full lookup table, filtered by the query):
#' \describe{
#'   \item{SOURCE}{Source of the waterbody name. One of \code{"BCFWA"},
#'     \code{"BCFWA Lakes"}, \code{"FISS"} (Fisheries Information Summary
#'     System), or \code{"SISS"} (Stream Inventory Sample Sites).}
#'   \item{NAMED_WATERSHED_ID}{Named watershed ID from the BCFWA.}
#'   \item{GNIS_ID}{Geographic Names Information System (GNIS) ID from the
#'     BCFWA.}
#'   \item{GNIS_NAME}{Canada Gazette official name for the waterbody.}
#'   \item{FWA_WATERSHED_CODE}{FWA watershed code from the BCFWA. A
#'     hierarchical dash-delimited code identifying the waterbody's position
#'     in the provincial drainage network.}
#'   \item{AREA_HA}{Area of the watershed in hectares, measured at its
#'     outlet.}
#'   \item{DOWNSTREAM_PATH}{Text string describing the downstream drainage
#'     path from the waterbody to the ocean or terminal waterbody. Segments
#'     are separated by \code{" > "} (e.g.
#'     \code{"Bridge Creek > Canim River > Fraser River"}).}
#'   \item{WATERBODY_TYPE}{Type of waterbody: \code{"Creek"}, \code{"River"},
#'     \code{"Lake"}, \code{"Slough"}, \code{"Pond"}, \code{"Channel"}, or
#'     \code{"Unknown"}.}
#'   \item{CENTROID_LATITUDE}{Approximate latitude of the watershed or
#'     waterbody centroid (decimal degrees, WGS 84).}
#'   \item{CENTROID_LONGITUDE}{Approximate longitude of the watershed or
#'     waterbody centroid (decimal degrees, WGS 84).}
#' }
#'
#' @details
#' The underlying lookup table
#' (\code{inst/extdata/bcfwa_resources/bc_waterbody_names.csv}) contains
#' 18,955 named waterbodies compiled from three provincial sources:
#' \itemize{
#'   \item \strong{BCFWA} — BC Freshwater Atlas named streams and watersheds.
#'   \item \strong{BCFWA Lakes} — BC Freshwater Atlas named lakes.
#'   \item \strong{FISS} — Fisheries Information Summary System.
#'   \item \strong{SISS} — Stream Inventory Sample Sites layer.
#' }
#'
#' @examples
#' # Find all waterbodies upstream of the Nicola River
#' up <- bc_waterbody_names("Nicola River", mode = "upstream")
#' head(up)
#'
#' # Only creeks upstream of the Nicola River
#' up_ck <- bc_waterbody_names("Nicola River", mode = "upstream",
#'                             waterbody_type = "Creek")
#' head(up_ck)
#'
#' # Get the downstream drainage path for Nicola River
#' dn <- bc_waterbody_names("Nicola River", mode = "downstream")
#' dn$DOWNSTREAM_PATH
#'
#' @source BC Freshwater Atlas (BCFWA), Fisheries Information Summary System
#'   (FISS), and Stream Inventory Sample Sites (SISS) layer. Province of
#'   British Columbia.
#'
#' @export
bc_waterbody_names <- function(waterbody,
                               mode = c("upstream", "downstream"),
                               gnis_id = NULL,
                               fwa_watershed_code = NULL,
                               waterbody_type = NULL) {

  mode <- match.arg(mode)

  # --- input validation ------------------------------------------------
  if (!is.character(waterbody) || length(waterbody) != 1 || is.na(waterbody)) {
    stop("`waterbody` must be a single character string.")
  }

  valid_types <- c("Creek", "River", "Lake", "Slough",
                   "Pond", "Channel", "Unknown")
  if (!is.null(waterbody_type)) {
    bad <- setdiff(waterbody_type, valid_types)
    if (length(bad) > 0) {
      stop("Invalid `waterbody_type`: ",
           paste(bad, collapse = ", "),
           ". Must be one of: ",
           paste(valid_types, collapse = ", "))
    }
  }

  # --- load lookup table -----------------------------------------------
  f <- system.file("extdata", "bcfwa_resources",
                   "bc_waterbody_names.csv",
                   package = "streamgis")
  if (f == "") {
    stop("bc_waterbody_names.csv not found. ",
         "Is the streamgis package installed correctly?")
  }

  lut <- utils::read.csv(f, stringsAsFactors = FALSE)

  # Drop any trailing empty column from the CSV
  lut <- lut[, colnames(lut) != "X", drop = FALSE]

  # --- query -----------------------------------------------------------
  if (mode == "upstream") {
    # Find rows where the target waterbody appears in DOWNSTREAM_PATH.
    # Use word-boundary matching so "Nicola River" does not match
    # "San Nicola River" etc.
    pattern <- paste0("(^|> )", waterbody, "( >|$)")
    idx <- grepl(pattern, lut$DOWNSTREAM_PATH, ignore.case = TRUE)
    out <- lut[idx, , drop = FALSE]
  } else {
    # mode == "downstream"
    # Return the record(s) whose GNIS_NAME matches the target.
    out <- lut[tolower(lut$GNIS_NAME) == tolower(waterbody), , drop = FALSE]
  }

  # --- optional filters ------------------------------------------------
  if (!is.null(gnis_id)) {
    out <- out[out$GNIS_ID %in% gnis_id, , drop = FALSE]
  }

  if (!is.null(fwa_watershed_code)) {
    out <- out[out$FWA_WATERSHED_CODE %in% fwa_watershed_code, , drop = FALSE]
  }

  if (!is.null(waterbody_type)) {
    out <- out[out$WATERBODY_TYPE %in% waterbody_type, , drop = FALSE]
  }

  rownames(out) <- NULL
  out
}
