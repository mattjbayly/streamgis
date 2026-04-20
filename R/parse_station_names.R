#' Parse station location strings into structured hydrological components
#'
#' This function takes a character vector of raw station location names and
#' parses them into structured components including the primary waterbody,
#' upstream/downstream references, unnamed tributaries, and other geographic
#' descriptors.
#'
#' The parsing uses rule-based pattern matching and heuristics to distinguish
#' between waterbodies (e.g., Creek, River, Lake) and other features (e.g.,
#' bridges, roads). Abbreviations such as "u/s", "d/s", "Ck", "Cr", "R.",
#' and "L." are normalized before parsing.
#'
#' @param x Character vector of raw station names.
#'
#' @return A data.frame with the following columns:
#' \describe{
#'   \item{raw_name}{Original input string}
#'   \item{waterbody}{Primary waterbody name (e.g., "Rock Creek"). Set to
#'     "Unnamed Tributary" when the site is on a tributary.}
#'   \item{upstream_of_waterbody}{Waterbody that the site is upstream of}
#'   \item{downstream_of_waterbody}{Waterbody that the site is downstream of}
#'   \item{unnamed_tributary_of}{Waterbody that an unnamed tributary flows
#'     into}
#'   \item{at_other}{Non-waterbody location reference (e.g., bridge, road)}
#'   \item{upstream_of_other}{Non-waterbody feature the site is upstream of}
#'   \item{downstream_of_other}{Non-waterbody feature the site is downstream
#'     of}
#'   \item{waterbody_type}{Type of waterbody: Creek, River, Lake, Pond,
#'     Slough, Channel, Marsh, Wetland, Stream, Lagoon, Brook, Narrows,
#'     Tributary, or Unknown}
#' }
#'
#' @examples
#' x <- c(
#'   "Big River downstream of Gordon Creek",
#'   "Tributary to Rock Creek",
#'   "Deep Creek at Farewell Bridge",
#'   "EAGLE R U/S 3 VALLEY LK",
#'   "Stoney Creek, Tributary 3A, in East Grove Park",
#'   "Tilbury Slough: T2-TL"
#' )
#' parse_station_names(x)
#'
#' @export
parse_station_names <- function(x) {

  # Known waterbody type keywords (title case canonical forms)
  wb_types <- c(
    "Creek", "River", "Lake", "Pond", "Slough",
    "Channel", "Marsh", "Wetland", "Stream", "Lagoon",
    "Brook", "Narrows"
  )

  # Well-known BC rivers commonly referenced without "River" keyword.
  # Short-hand (title case) -> full name. Derived from short-hand-fullname.csv.
  well_known_rivers <- c(
    "Fraser" = "Fraser River",
    "Liard" = "Liard River",
    "Peace" = "Peace River",
    "Columbia" = "Columbia River",
    "Thompson" = "Thompson River",
    "Skeena" = "Skeena River",
    "Stikine" = "Stikine River",
    "Nechako" = "Nechako River",
    "Kootenay" = "Kootenay River",
    "Kechika" = "Kechika River",
    "Nass" = "Nass River",
    "Muskwa" = "Muskwa River",
    "Chilcotin" = "Chilcotin River",
    "Finlay" = "Finlay River",
    "Taku" = "Taku River",
    "Stuart" = "Stuart River",
    "Okanagan" = "Okanagan River",
    "Dease" = "Dease River",
    "Beatton" = "Beatton River",
    "Teslin" = "Teslin River",
    "West Road" = "West Road (Blackwater) River",
    "Bulkley" = "Bulkley River",
    "Petitot" = "Petitot River",
    "Quesnel" = "Quesnel River",
    "Sikanni" = "Sikanni Chief River",
    "Inklin" = "Inklin River",
    "Babine" = "Babine River",
    "Clearwater" = "Clearwater River",
    "Tachie" = "Tachie River",
    "Iskut" = "Iskut River",
    "Halfway" = "Halfway River",
    "Hay" = "Hay River",
    "Prophet" = "Prophet River",
    "Harrison" = "Harrison River",
    "Alsek" = "Alsek River",
    "Kettle" = "Kettle River",
    "Omineca" = "Omineca River",
    "Dean" = "Dean River",
    "Similkameen" = "Similkameen River",
    "Toad" = "Toad River",
    "Nicola" = "Nicola River",
    "Middle" = "Middle River",
    "Turnagain" = "Turnagain River",
    "Nation" = "Nation River",
    "Chilko" = "Chilko River",
    "Fontas" = "Fontas River",
    "Kotcho" = "Kotcho River",
    "Murray" = "Murray River",
    "Stellako" = "Stellako River",
    "Klinaklini" = "Klinaklini River",
    "Homathko" = "Homathko River",
    "Parsnip" = "Parsnip River",
    "Mcgregor" = "McGregor River",
    "Shuswap" = "Shuswap River",
    "Bonaparte" = "Bonaparte River",
    "Bella Coola" = "Bella Coola River",
    "Bell-Irving" = "Bell-Irving River",
    "Ingenika" = "Ingenika River",
    "Tatshenshini" = "Tatshenshini River",
    "Atlin" = "Atlin River",
    "Wapiti" = "Wapiti River",
    "Mahood" = "Mahood River",
    "Nazko" = "Nazko River",
    "Sahtaneh" = "Sahtaneh River",
    "Kiskatinaw" = "Kiskatinaw River",
    "Nakina" = "Nakina River",
    "Wannock" = "Wannock River",
    "Canim" = "Canim River",
    "Nahlin" = "Nahlin River",
    "Jennings" = "Jennings River",
    "Gataga" = "Gataga River",
    "Chilako" = "Chilako River",
    "Bowron" = "Bowron River",
    "Klappan" = "Klappan River",
    "Sustut" = "Sustut River",
    "Tuya" = "Tuya River",
    "Spatsizi" = "Spatsizi River",
    "Slocan" = "Slocan River",
    "Tetachuck" = "Tetachuck River",
    "Sheslay" = "Sheslay River",
    "Adams" = "Adams River",
    "Dunedin" = "Dunedin River",
    "Mesilinka" = "Mesilinka River",
    "Cariboo" = "Cariboo River",
    "Tsaytis" = "Tsaytis River",
    "Willow" = "Willow River",
    "Taseko" = "Taseko River",
    "Pend-D'Oreille" = "Pend-d'Oreille River",
    "Zymoetz" = "Zymoetz River",
    "Ospika" = "Ospika River",
    "Sukunka" = "Sukunka River",
    "Kahntah" = "Kahntah River",
    "Horsefly" = "Horsefly River",
    "Kitlope" = "Kitlope River",
    "Pitman" = "Pitman River",
    "Chutine" = "Chutine River",
    "Atnarko" = "Atnarko River",
    "Racing" = "Racing River",
    "Frog" = "Frog River",
    "Cottonwood" = "Cottonwood River",
    "Swift" = "Swift River",
    "Gladys" = "Gladys River",
    "Shekilie" = "Shekilie River",
    "Mess" = "Mess Creek",
    "Graham" = "Graham River",
    "San Jose" = "San Jose River",
    "Kitsumkalum" = "Kitsumkalum River",
    "Kiwigana" = "Kiwigana River",
    "Crooked" = "Crooked River",
    "Doig" = "Doig River",
    "Osilinka" = "Osilinka River",
    "Kispiox" = "Kispiox River",
    "Granby" = "Granby River",
    "Cameron" = "Cameron River",
    "Tsea" = "Tsea River",
    "Kitimat" = "Kitimat River",
    "Southgate" = "Southgate River",
    "Tuchodi" = "Tuchodi River",
    "Endako" = "Endako River",
    "Seton" = "Seton River",
    "Baezaeko" = "Baezaeko River",
    "Moberly" = "Moberly River",
    "Chilanko" = "Chilanko River",
    "Cranberry" = "Cranberry River",
    "Akie" = "Akie River",
    "Tahltan" = "Tahltan River",
    "Klastline" = "Klastline River",
    "Entiako" = "Entiako River",
    "Tanzilla" = "Tanzilla River",
    "Grayling" = "Grayling River",
    "Smith" = "Smith River",
    "Tulameen" = "Tulameen River",
    "Driftwood" = "Driftwood River",
    "Nimpkish" = "Nimpkish River",
    "Whiting" = "Whiting River",
    "Campbell" = "Campbell River",
    "Unuk" = "Unuk River",
    "Chilkat" = "Chilkat River",
    "Bowser" = "Bowser River",
    "Lardeau" = "Lardeau River"
  )

  # Regex pattern fragment matching any waterbody type keyword
  wb_type_pattern <- paste0(
    "\\b(", paste(wb_types, collapse = "|"), ")\\b"
  )

  # --- Internal helpers -------------------------------------------------------


  # Normalize abbreviations and formatting in a station name string.
  # Expands common short forms (Ck -> Creek, R. -> River, etc.) and
  # standardizes directional keywords (u/s -> upstream of, d/s -> downstream
  # of, etc.).
  normalize_string <- function(s) {
    # Trim whitespace
    s <- trimws(s)

    # Standardize upstream/downstream abbreviations BEFORE expanding
    # waterbody abbreviations (order matters for patterns like "U/S")
    s <- gsub("\\bu/?s\\s+of\\b", "upstream of", s, ignore.case = TRUE)
    s <- gsub("\\bd/?s\\s+of\\b", "downstream of", s, ignore.case = TRUE)
    s <- gsub("\\bUPS\\b", "upstream of", s, ignore.case = TRUE)
    s <- gsub("\\bu/?s\\b", "upstream of", s, ignore.case = TRUE)
    s <- gsub("\\bd/?s\\b", "downstream of", s, ignore.case = TRUE)
    s <- gsub("\\bu\\\\s\\b", "upstream of", s, ignore.case = TRUE)
    s <- gsub("\\bd\\\\s\\b", "downstream of", s, ignore.case = TRUE)

    # Standardize preposition abbreviations
    s <- gsub("\\bNR\\.?(?=\\s)", "Near", s, perl = TRUE, ignore.case = TRUE)
    s <- gsub("\\bADJ\\b", "Adjacent To", s, ignore.case = TRUE)

    # Standardize "@ " to "at "
    s <- gsub("@\\s*", "at ", s)

    # Convert underscores to spaces
    s <- gsub("_", " ", s)

    # Expand waterbody type abbreviations (applied to whole string)
    # Order matters: longer patterns first to avoid partial matches
    s <- gsub("\\bCKS\\b", "Creeks", s, ignore.case = FALSE)
    s <- gsub("\\bCRK\\b", "Creek",  s, ignore.case = TRUE)
    s <- gsub("\\bCK\\b",  "Creek",  s, ignore.case = FALSE)
    s <- gsub("\\bCR\\b",  "Creek",  s, ignore.case = FALSE)
    # "C." as standalone abbreviation (preceded by space/start, followed by
    # space/end) - avoids matching in "B.C.", "O.K.C." etc.
    s <- gsub("(?<=\\s|^)C\\.(?=\\s|$)", "Creek", s, perl = TRUE)
    # Single "C" before a preposition, uppercase word, or at end of string
    s <- gsub(
      "\\bC\\b(?=\\s+(upstream|downstream|at|near|above|below))",
      "Creek", s, perl = TRUE, ignore.case = TRUE
    )
    s <- gsub("(?<=\\s|^)C(?=\\s+[A-Z])", "Creek", s, perl = TRUE)
    s <- gsub("\\bC$", "Creek", s)
    # "R." as standalone abbreviation or glued to next word
    s <- gsub("(?<=\\s|^)R\\.(?=\\s|$)", "River", s, perl = TRUE)
    s <- gsub("(?<=\\s|^)R\\.(?=[A-Za-z])", "River ", s, perl = TRUE)
    s <- gsub(
      "\\bR\\b(?=\\s+(upstream|downstream|at|near|above|below|on|upper|lower|mid|north|south|east|west))",
      "River", s, perl = TRUE, ignore.case = TRUE
    )
    s <- gsub("\\bR\\b(?=\\s+[-0-9])", "River", s, perl = TRUE)
    s <- gsub("\\bR$", "River", s)
    s <- gsub("\\bLK\\b",   "Lake",  s, ignore.case = TRUE)
    # "L." as standalone abbreviation
    s <- gsub("(?<=\\s|^)L\\.(?=\\s|$)", "Lake", s, perl = TRUE)
    s <- gsub("\\bL\\b(?=\\s+(at|near))", "Lake", s, perl = TRUE,
              ignore.case = TRUE)

    # Strip article "the" before location references
    s <- gsub("\\bthe\\s+", "", s, ignore.case = TRUE)

    # Convert to title case for consistency
    s <- to_title_case(s)

    # Collapse multiple spaces
    s <- gsub("\\s+", " ", s)
    s <- trimws(s)

    s
  }

  # Convert a string to title case (first letter of each word capitalized).
  # Also capitalizes after parentheses, hyphens, and slashes.
  to_title_case <- function(s) {
    s <- tolower(s)
    # Capitalize first letter of each word
    s <- gsub("(^|[\\s(/-])([a-z])", "\\1\\U\\2", s, perl = TRUE)
    s
  }

  # Detect the waterbody type keyword from a name string.
  # Returns the LAST matching type keyword to handle cases like
  # "Marsh Lake" (type = Lake, not Marsh).
  detect_type <- function(name) {
    if (is.na(name) || name == "") return(NA_character_)
    all_matches <- gregexpr(wb_type_pattern, name, ignore.case = TRUE)
    positions <- all_matches[[1]]
    if (positions[1] == -1) return("Unknown")
    # Get the last match
    last_pos <- positions[length(positions)]
    last_len <- attr(all_matches[[1]], "match.length")[length(positions)]
    m <- substring(name, last_pos, last_pos + last_len - 1)
    # Title-case the match
    paste0(toupper(substring(tolower(m), 1, 1)),
           substring(tolower(m), 2))
  }

  # Test whether a string looks like a waterbody name (contains a waterbody
  # type keyword).
  is_waterbody <- function(name) {
    if (is.na(name) || name == "") return(FALSE)
    grepl(wb_type_pattern, name, ignore.case = TRUE)
  }

  # Expand well-known river short-hand names to their full name.
  # Only expands if the name does NOT already contain a waterbody type keyword
  # (e.g., "Nass Creek" stays as-is, but "Nass" becomes "Nass River").
  # Returns a list with $name and $type (or unchanged input if no match).
  expand_well_known <- function(name) {
    if (is.na(name) || name == "") return(list(name = name, type = NA_character_))
    # If the name already contains a waterbody type keyword, keep as-is
    if (grepl(wb_type_pattern, name, ignore.case = TRUE)) {
      return(list(name = name, type = NULL))
    }
    # Try exact match against short-hand keys (case-insensitive)
    name_lower <- tolower(trimws(name))
    keys_lower <- tolower(names(well_known_rivers))
    idx <- match(name_lower, keys_lower)
    if (!is.na(idx)) {
      full <- unname(well_known_rivers[idx])
      full_type <- detect_type(full)
      return(list(name = full, type = full_type))
    }
    list(name = name, type = NULL)
  }

  # Extract the waterbody name from a string. Finds the substring
  # up to and including the first waterbody type keyword.
  # When the first keyword IS the first word (e.g., "Marsh Lake", "Lake Laberge"),
  # extends to include a second keyword if present.
  extract_waterbody_name <- function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    # Match everything up to and including the FIRST waterbody type keyword
    m <- regmatches(
      s,
      regexpr(
        paste0("^.*?\\b(", paste(wb_types, collapse = "|"), ")\\b"),
        s, ignore.case = TRUE, perl = TRUE
      )
    )
    if (length(m) == 0 || m == "") return(NA_character_)
    result <- trimws(m)
    # If result is JUST a type keyword by itself (e.g., "Marsh" from "Marsh Lake",
    # or "Lake" from "Lake Laberge"), try to extend
    if (tolower(result) %in% tolower(wb_types)) {
      # First try: extend to a second type keyword (e.g., "Marsh Lake")
      m2 <- regmatches(
        s,
        regexpr(
          paste0("^.*?\\b(", paste(wb_types, collapse = "|"),
                 ")\\b.*?\\b(", paste(wb_types, collapse = "|"), ")\\b"),
          s, ignore.case = TRUE, perl = TRUE
        )
      )
      if (length(m2) > 0 && m2 != "") {
        result <- trimws(m2)
      } else {
        # Second try: include the next word as a proper name
        # (e.g., "Lake Laberge" where Laberge is not a keyword)
        m3 <- regmatches(
          s,
          regexpr(
            paste0("^\\S+\\s+\\S+"),
            s, perl = TRUE
          )
        )
        if (length(m3) > 0 && m3 != "") {
          result <- trimws(m3)
        }
      }
    }
    result
  }

  # --- Initialize output data.frame ------------------------------------------
  n <- length(x)
  out <- data.frame(
    raw_name                = x,
    waterbody               = rep(NA_character_, n),
    upstream_of_waterbody   = rep(NA_character_, n),
    downstream_of_waterbody = rep(NA_character_, n),
    unnamed_tributary_of    = rep(NA_character_, n),
    at_other                = rep(NA_character_, n),
    upstream_of_other       = rep(NA_character_, n),
    downstream_of_other     = rep(NA_character_, n),
    waterbody_type          = rep(NA_character_, n),
    stringsAsFactors        = FALSE
  )

  # --- Main parsing loop ------------------------------------------------------
  for (i in seq_along(x)) {
    raw <- x[i]

    # Skip NA or empty strings
    if (is.na(raw) || trimws(raw) == "") next

    s <- normalize_string(raw)

    # ------------------------------------------------------------------
    # 0. Strip leading station/site codes and non-waterbody prefixes
    # ------------------------------------------------------------------
    station_code <- NA_character_

    # Pattern: "LL01 at Nechako River..." - code + preposition + waterbody
    if (grepl("^[A-Za-z]{1,4}[0-9-]+\\s+(At|Near|On)\\s+", s,
              ignore.case = TRUE)) {
      station_code <- trimws(sub("\\s+(At|Near|On)\\s+.*", "", s,
                                 ignore.case = TRUE))
      s <- trimws(sub("^[A-Za-z]{1,4}[0-9-]+\\s+(At|Near|On)\\s+", "", s,
                       ignore.case = TRUE))
    }
    # Pattern: "LL07 Nechako River..." - code directly before waterbody
    # (no preposition separator)
    else if (grepl("^[A-Za-z]{1,4}[0-9-]+\\s+", s) && is_waterbody(s)) {
      potential_code <- sub("\\s+.*", "", s)
      rest_after_code <- trimws(sub("^[A-Za-z]{1,4}[0-9-]+\\s+", "", s))
      if (is_waterbody(rest_after_code)) {
        station_code <- potential_code
        s <- rest_after_code
      }
    }

    # Strip non-waterbody prefixes before dash (e.g., "Bosk - Horsefly River")
    if (grepl("^[^-]+\\s+-\\s+", s) && is_waterbody(s)) {
      prefix <- trimws(sub("\\s+-\\s+.*", "", s))
      after_dash <- trimws(sub("^[^-]+\\s+-\\s+", "", s))
      if (!is_waterbody(prefix) && is_waterbody(after_dash)) {
        s <- after_dash
      }
    }

    # Pattern: leading number (3+ digits) before waterbody (e.g., "400 Hat Creek")
    # Only 3+ digits to avoid stripping short numbers like "3 Valley Lake"
    if (grepl("^\\d{3,}\\s+", s) && is_waterbody(s)) {
      rest_after_num <- trimws(sub("^\\d{3,}\\s+", "", s))
      if (is_waterbody(rest_after_num)) {
        station_code <- sub("\\s+.*", "", s)
        s <- rest_after_num
      }
    }

    # Pattern: "OUTFALL N <waterbody>" prefix
    if (grepl("^Outfall\\s+\\S+\\s+", s, ignore.case = TRUE) &&
        is_waterbody(s)) {
      rest_after <- trimws(sub("^Outfall\\s+\\S+\\s+", "", s,
                                ignore.case = TRUE))
      if (is_waterbody(rest_after)) {
        station_code <- trimws(sub("\\s+[^ ]+$", "",
                                   sub(paste0("\\s+", rest_after, ".*"), "", s)))
        s <- rest_after
      }
    }

    # Pattern: "<text> discharge to <waterbody>" (e.g., "Enderby Stp Discharge To Shuswap River")
    if (grepl("\\bdischarge\\s+to\\b", s, ignore.case = TRUE) &&
        is_waterbody(s)) {
      after_discharge <- trimws(sub(".*\\bdischarge\\s+to\\s+", "", s,
                                     ignore.case = TRUE))
      if (is_waterbody(after_discharge)) {
        station_code <- trimws(sub("\\s*\\bdischarge\\s+to\\b.*", "", s,
                                    ignore.case = TRUE))
        s <- after_discharge
      }
    }

    # Pattern: leading abbreviated code with period (e.g., "Quesn. Fraser River")
    if (grepl("^\\S+\\.\\s+", s) && is_waterbody(s)) {
      code_prefix <- sub("^(\\S+\\.)\\s+.*", "\\1", s)
      rest_after <- trimws(sub("^\\S+\\.\\s+", "", s))
      if (is_waterbody(rest_after) && !is_waterbody(code_prefix)) {
        station_code <- code_prefix
        s <- rest_after
      }
    }

    # Pattern: company/organization name before waterbody
    if (grepl("\\b(Industries|Ltd|Inc|Corp|Company|Enterprises|Mills|Mines|Mining)\\b",
              s, ignore.case = TRUE) && is_waterbody(s)) {
      corp_match <- regexpr(
        "\\b(Industries|Ltd|Inc|Corp|Company|Enterprises|Mills|Mines|Mining)\\b",
        s, ignore.case = TRUE
      )
      after_corp <- trimws(substring(s, corp_match + attr(corp_match, "match.length")))
      if (is_waterbody(after_corp)) {
        station_code <- trimws(substring(s, 1,
                                          corp_match + attr(corp_match, "match.length") - 1))
        s <- after_corp
      }
    }

    # Pattern: leading non-waterbody text with alphanumeric code before waterbody
    # (e.g., "Cabin Site Dunn02 Dunn Creek")
    if (!is.na(station_code) || !grepl("\\b[A-Za-z]+\\d+\\b", s)) {
      # skip
    } else if (is_waterbody(s)) {
      code_match <- regexpr("\\b[A-Za-z]+\\d+[A-Za-z0-9-]*\\b", s)
      if (code_match > 0) {
        after_code <- trimws(substring(
          s, code_match + attr(code_match, "match.length")
        ))
        if (is_waterbody(after_code)) {
          before_code <- trimws(substring(s, 1, code_match - 1))
          code_text <- trimws(regmatches(s, code_match))
          station_code <- if (nchar(before_code) > 0) {
            paste(before_code, code_text)
          } else {
            code_text
          }
          s <- after_code
        }
      }
    }

    # ------------------------------------------------------------------
    # 1. Tributary patterns
    # ------------------------------------------------------------------

    # "Tributary to <waterbody>" or "Trib to <waterbody>"
    if (grepl("\\btributary\\s+to\\b", s, ignore.case = TRUE)) {
      ref <- trimws(sub(".*\\btributary\\s+to\\s+", "", s, ignore.case = TRUE))
      out$waterbody[i] <- "Unnamed Tributary"
      out$unnamed_tributary_of[i] <- ref
      out$waterbody_type[i] <- "Tributary"
      next
    }

    # "<waterbody> tributary" without "to" (e.g., "Squawkum Creek tributary")
    if (grepl("\\btributary$", s, ignore.case = TRUE)) {
      ref <- trimws(sub("\\s*\\btributary$", "", s, ignore.case = TRUE))
      out$waterbody[i] <- "Unnamed Tributary"
      out$unnamed_tributary_of[i] <- ref
      out$waterbody_type[i] <- "Tributary"
      next
    }

    # "<waterbody>, Tributary <ID>, <location>" pattern
    # e.g., "Stoney Creek, Tributary 3A, in East Grove Park"
    if (grepl(",\\s*Tributary\\s+\\w+", s, ignore.case = TRUE)) {
      parent_wb <- trimws(sub(",.*", "", s))
      # Location info after the tributary ID
      location_part <- sub(".*,\\s*Tributary\\s+\\w+\\s*,?\\s*", "", s,
                           ignore.case = TRUE)
      # Strip leading preposition
      location_part <- sub(
        "^(at|near|in|by|on)\\s+", "", location_part, ignore.case = TRUE
      )

      out$waterbody[i] <- "Unnamed Tributary"
      out$unnamed_tributary_of[i] <- parent_wb
      out$waterbody_type[i] <- "Tributary"
      if (nchar(trimws(location_part)) > 0) {
        out$at_other[i] <- trimws(location_part)
      }
      next
    }

    # "<waterbody> Tributary <ID>" without comma
    # e.g., "Michel Creek Tributary 1"
    if (grepl(
      paste0("\\b(", paste(wb_types, collapse = "|"),
             ")\\s+Tributary\\s+\\w+"),
      s, ignore.case = TRUE
    )) {
      parent_wb <- extract_waterbody_name(
        sub("\\s+Tributary\\s+.*", "", s, ignore.case = TRUE)
      )
      if (!is.na(parent_wb)) {
        out$waterbody[i] <- "Unnamed Tributary"
        out$unnamed_tributary_of[i] <- parent_wb
        out$waterbody_type[i] <- "Tributary"
        next
      }
    }

    # "Wigwam River - S6 Tributary ..." patterns
    if (grepl("\\bTributary\\b", s, ignore.case = TRUE) &&
        grepl("-\\s*S\\d+\\s+Tributary", s, ignore.case = TRUE)) {
      parent_wb <- extract_waterbody_name(sub("\\s*-.*", "", s))
      out$waterbody[i] <- "Unnamed Tributary"
      if (!is.na(parent_wb)) {
        out$unnamed_tributary_of[i] <- parent_wb
        out$at_other[i] <- parent_wb
      }
      out$waterbody_type[i] <- "Tributary"
      next
    }

    # ------------------------------------------------------------------
    # 2. Colon-separated patterns (e.g., "Tilbury Slough: T2-TL")
    # ------------------------------------------------------------------
    if (grepl(":", s) && !grepl("(upstream|downstream|above|below)", s,
                                ignore.case = TRUE)) {
      parts <- strsplit(s, ":\\s*")[[1]]
      wb_part <- trimws(parts[1])
      other_part <- trimws(paste(parts[-1], collapse = ": "))

      # Strip semicolons from wb_part
      wb_part <- trimws(gsub(";.*", "", wb_part))

      out$waterbody[i] <- wb_part
      if (nchar(other_part) > 0) {
        out$at_other[i] <- other_part
      }
      out$waterbody_type[i] <- detect_type(wb_part)
      next
    }

    # ------------------------------------------------------------------
    # 3. Comma-separated patterns (common in urban creek data)
    #    e.g., "Lynn Creek, near Ross Rd. in Lynn Canyon Park"
    # ------------------------------------------------------------------
    if (grepl(",", s) &&
        !grepl("(upstream|downstream|above|below)\\s+of", s,
               ignore.case = TRUE)) {
      parts <- strsplit(s, ",\\s*", perl = TRUE)[[1]]
      wb_part <- trimws(parts[1])
      rest <- trimws(paste(parts[-1], collapse = ", "))

      # Strip leading prepositions from the rest
      rest <- sub(
        "^(at|near|in|by|on|behind|off|between)\\s+",
        "", rest, ignore.case = TRUE
      )

      # Check if wb_part looks like a waterbody
      if (is_waterbody(wb_part)) {
        out$waterbody[i] <- wb_part

        # Handle branch annotations like "(east branch)" in rest
        branch <- ""
        if (grepl("\\((east|west|north|south)\\s+branch\\)", rest,
                  ignore.case = TRUE)) {
          branch_match <- regmatches(
            rest,
            regexpr("\\([^)]+branch\\)", rest, ignore.case = TRUE)
          )
          branch <- branch_match
          rest <- trimws(gsub("\\([^)]+branch\\)", "", rest))
        }

        if (nchar(rest) > 0) {
          at_val <- rest
          if (nchar(branch) > 0) {
            at_val <- paste(rest, branch)
          }
          out$at_other[i] <- at_val
        }
        out$waterbody_type[i] <- detect_type(wb_part)
      } else {
        # Not clearly a waterbody; treat whole string as waterbody name
        out$waterbody[i] <- s
        out$waterbody_type[i] <- detect_type(s)
      }
      next
    }

    # ------------------------------------------------------------------
    # 4. Upstream / downstream / above / below patterns
    # ------------------------------------------------------------------

    # Build regex for upstream/downstream split keywords
    us_pattern <- "(?i)\\b(upstream\\s+of|above)\\b"
    ds_pattern <- "(?i)\\b(downstream\\s+of|below)\\b"

    # Handle cases with BOTH upstream and downstream references
    # e.g., "Blurton Creek at HWY 97A Near Mara Lake"
    #   handled later in "at/near" section

    if (grepl(us_pattern, s, perl = TRUE)) {
      parts <- strsplit(s, us_pattern, perl = TRUE)[[1]]
      wb <- trimws(parts[1])
      ref <- trimws(parts[2])

      # The wb part might contain "at <location>" info
      # e.g., "Stamp River At Robertson Creek Hatchery" - handle "at" within wb
      at_info <- NA_character_
      if (grepl("\\b(at|near|in|on|by)\\b", wb, ignore.case = TRUE) &&
          !grepl("^(at|near|in|on|by)\\b", wb, ignore.case = TRUE)) {
        at_parts <- strsplit(
          wb,
          "\\s+(?:at|near|in|on|by)\\s+",
          perl = TRUE
        )[[1]]
        if (length(at_parts) >= 2) {
          wb <- trimws(at_parts[1])
          at_info <- trimws(paste(at_parts[-1], collapse = " "))
        }
      }

      # Extract the waterbody name from wb (may have trailing text like
      # distance markers)
      wb_name <- extract_waterbody_name(wb)
      if (!is.na(wb_name)) {
        out$waterbody[i] <- wb_name
        # Any remainder after the wb name goes to at_other
        wb_extra <- trimws(substring(wb, nchar(wb_name) + 1))
        wb_extra <- sub("^[-,;:]\\s*", "", wb_extra)
        wb_extra <- trimws(wb_extra)
        if (nchar(wb_extra) > 0 && is.na(at_info)) {
          at_info <- wb_extra
        }
      } else {
        out$waterbody[i] <- wb
      }

      # Classify the reference: extract waterbody name and any remainder
      ref_wb <- extract_waterbody_name(ref)
      if (!is.na(ref_wb)) {
        out$upstream_of_waterbody[i] <- ref_wb
        extra <- trimws(substring(ref, nchar(ref_wb) + 1))
        extra <- sub("^[-,;:]\\s*", "", extra)
        # Strip leading prepositions from leftover text
        extra <- sub(
          "^(At|Near|In|On|By)\\s+", "", trimws(extra),
          ignore.case = TRUE
        )
        extra <- trimws(extra)
        if (nchar(extra) > 0) {
          if (is.na(at_info)) {
            at_info <- extra
          }
        }
      } else {
        out$upstream_of_other[i] <- ref
      }

      if (!is.na(at_info) && nchar(at_info) > 0) {
        if (!is.na(station_code)) {
          at_info <- paste0(at_info, " (", station_code, ")")
        }
        out$at_other[i] <- at_info
      } else if (!is.na(station_code)) {
        out$at_other[i] <- station_code
      }

      out$waterbody_type[i] <- detect_type(
        if (!is.na(out$waterbody[i])) out$waterbody[i] else wb
      )
      next
    }

    if (grepl(ds_pattern, s, perl = TRUE)) {
      parts <- strsplit(s, ds_pattern, perl = TRUE)[[1]]
      wb <- trimws(parts[1])
      ref <- trimws(parts[2])

      # Handle "at <location>" within wb part
      at_info <- NA_character_
      if (grepl("\\b(at|near|in|on|by)\\b", wb, ignore.case = TRUE) &&
          !grepl("^(at|near|in|on|by)\\b", wb, ignore.case = TRUE)) {
        at_parts <- strsplit(
          wb,
          "\\s+(?:at|near|in|on|by)\\s+",
          perl = TRUE
        )[[1]]
        if (length(at_parts) >= 2) {
          wb <- trimws(at_parts[1])
          at_info <- trimws(paste(at_parts[-1], collapse = " "))
        }
      }

      # Extract the waterbody name from wb
      wb_name <- extract_waterbody_name(wb)
      if (!is.na(wb_name)) {
        out$waterbody[i] <- wb_name
        wb_extra <- trimws(substring(wb, nchar(wb_name) + 1))
        wb_extra <- sub("^[-,;:]\\s*", "", wb_extra)
        wb_extra <- trimws(wb_extra)
        if (nchar(wb_extra) > 0 && is.na(at_info)) {
          at_info <- wb_extra
        }
      } else {
        out$waterbody[i] <- wb
      }

      # Classify the reference
      ref_wb <- extract_waterbody_name(ref)
      if (!is.na(ref_wb)) {
        out$downstream_of_waterbody[i] <- ref_wb
        extra <- trimws(substring(ref, nchar(ref_wb) + 1))
        extra <- sub("^[-,;:]\\s*", "", extra)
        extra <- sub(
          "^(At|Near|In|On|By)\\s+", "", trimws(extra),
          ignore.case = TRUE
        )
        extra <- trimws(extra)
        if (nchar(extra) > 0 && is.na(at_info)) {
          at_info <- extra
        }
      } else {
        out$downstream_of_other[i] <- ref
      }

      if (!is.na(at_info) && nchar(at_info) > 0) {
        if (!is.na(station_code)) {
          at_info <- paste0(at_info, " (", station_code, ")")
        }
        out$at_other[i] <- at_info
      } else if (!is.na(station_code)) {
        out$at_other[i] <- station_code
      }

      out$waterbody_type[i] <- detect_type(
        if (!is.na(out$waterbody[i])) out$waterbody[i] else wb
      )
      next
    }

    # ------------------------------------------------------------------
    # 5. "at outlet of" / "at inlet of" patterns
    #    e.g., "Pack River At Outlet Of Mcleod Lake"
    # ------------------------------------------------------------------
    if (grepl("\\b(at|near)\\s+(outlet|inlet)\\s+of\\b", s,
              ignore.case = TRUE)) {
      wb <- trimws(sub(
        "\\s+(at|near)\\s+(outlet|inlet)\\s+of\\s+.*", "", s,
        ignore.case = TRUE
      ))
      ref <- trimws(sub(
        ".*\\b(outlet|inlet)\\s+of\\s+", "", s, ignore.case = TRUE
      ))
      direction_word <- tolower(regmatches(
        s,
        regexpr("\\b(outlet|inlet)\\b", s, ignore.case = TRUE)
      ))

      out$waterbody[i] <- wb

      if (is_waterbody(ref)) {
        # "at outlet of Lake X" -> upstream_of (the site is at the outlet,
        # so it is upstream of the downstream waterbody, but more precisely
        # the ref lake is upstream of the site)
        out$upstream_of_waterbody[i] <- ref
      }
      out$at_other[i] <- if (!is.na(station_code)) {
        paste0(
          toupper(substring(direction_word, 1, 1)),
          substring(direction_word, 2),
          " (", station_code, ")"
        )
      } else {
        paste0(
          toupper(substring(direction_word, 1, 1)),
          substring(direction_word, 2)
        )
      }

      out$waterbody_type[i] <- detect_type(wb)
      next
    }

    # ------------------------------------------------------------------
    # 6. Parenthetical patterns (without upstream/downstream)
    #    e.g., "Cowichan River (Upper Catalyst)"
    # ------------------------------------------------------------------
    if (grepl("\\(", s) &&
        !grepl("(upstream|downstream|above|below)", s, ignore.case = TRUE)) {
      # Extract parenthetical content (usually station codes or descriptors)
      paren_content <- regmatches(s, gregexpr("\\(([^)]+)\\)", s))[[1]]
      paren_content <- gsub("[()]", "", paren_content)
      paren_content <- trimws(paren_content)

      # Strip parenthetical from main string for further parsing
      s_no_paren <- trimws(gsub("\\s*\\([^)]*\\)", "", s))

      # Check for "at/near" pattern in the string without parentheses
      loc_prep_in_paren <- paste0(
        "\\b(at|near|in|on|by|behind|off|under|adjacent\\s+to)\\b"
      )

      if (grepl(loc_prep_in_paren, s_no_paren, ignore.case = TRUE)) {
        split_pos <- regexpr(
          paste0("\\s+", loc_prep_in_paren, "\\s+"),
          s_no_paren, ignore.case = TRUE, perl = TRUE
        )
        if (split_pos > 0) {
          wb <- trimws(substring(s_no_paren, 1, split_pos - 1))
          rest <- trimws(substring(
            s_no_paren,
            split_pos + attr(split_pos, "match.length")
          ))

          out$waterbody[i] <- wb

          # Check if rest is a waterbody reference
          ref_wb <- extract_waterbody_name(rest)
          if (!is.na(ref_wb) && ref_wb == rest && is_waterbody(wb)) {
            out$upstream_of_waterbody[i] <- ref_wb
            if (length(paren_content) > 0) {
              out$at_other[i] <- paste(paren_content, collapse = "; ")
            }
          } else {
            # Combine rest with parenthetical info
            combined <- rest
            if (length(paren_content) > 0) {
              combined <- paste0(rest, " (", paste(paren_content,
                                                    collapse = "; "), ")")
            }
            out$at_other[i] <- combined
          }

          out$waterbody_type[i] <- detect_type(wb)
          next
        }
      }

      # No "at/near" - just waterbody + parenthetical descriptor
      wb_name <- extract_waterbody_name(s_no_paren)
      if (!is.na(wb_name)) {
        remainder <- trimws(substring(s_no_paren, nchar(wb_name) + 1))
        remainder <- sub("^[-,;:]\\s*", "", remainder)
        remainder <- trimws(remainder)
        if (nchar(remainder) > 0 &&
            !tolower(remainder) %in% tolower(wb_types)) {
          # Trailing text after waterbody keyword (e.g., "Williams Lake Lf ...")
          # but NOT when remainder is a type keyword (e.g., "River" in
          # "Williams Lake River")
          out$waterbody[i] <- wb_name
          at_parts <- remainder
          if (length(paren_content) > 0) {
            at_parts <- paste0(remainder, " (",
                                paste(paren_content, collapse = "; "), ")")
          }
          out$at_other[i] <- at_parts
        } else {
          # Waterbody name is the whole string
          out$waterbody[i] <- wb_name
          if (length(paren_content) > 0) {
            out$at_other[i] <- paste(paren_content, collapse = ", ")
          }
        }
      } else {
        out$waterbody[i] <- s_no_paren
        if (length(paren_content) > 0) {
          out$at_other[i] <- paste(paren_content, collapse = ", ")
        }
      }
      out$waterbody_type[i] <- detect_type(
        if (!is.na(out$waterbody[i])) out$waterbody[i] else s_no_paren
      )
      next
    }

    # ------------------------------------------------------------------
    # 7. "at" / "near" / "in" / "on" / "by" / "behind" / "off" /
    #    "adjacent to" / "between" / "opposite" / "north of" etc.
    # ------------------------------------------------------------------
    loc_prep_pattern <- paste0(
      "\\b(at|near|in|on|by|behind|off|under|adjacent\\s+to|between|opposite|",
      "north\\s+of|south\\s+of|east\\s+of|west\\s+of|southeast\\s+of|",
      "southwest\\s+of|northeast\\s+of|northwest\\s+of)\\b"
    )

    if (grepl(loc_prep_pattern, s, ignore.case = TRUE)) {
      # Split at the first occurrence of a location preposition,
      # capturing which preposition was used
      split_result <- regexpr(
        paste0("\\s+", loc_prep_pattern, "\\s+"),
        s, ignore.case = TRUE, perl = TRUE
      )

      if (split_result > 0) {
        wb <- trimws(substring(s, 1, split_result - 1))
        matched_prep <- tolower(trimws(regmatches(
          s, split_result
        )))
        rest <- trimws(substring(
          s,
          split_result + attr(split_result, "match.length")
        ))

        # Prepositions that imply confluence / waterbody reference
        confluence_preps <- c("at")

        # If wb doesn't look like a waterbody, it might be something like
        # "Outfall 4 Baker Creek" where the waterbody comes after
        if (!is_waterbody(wb) && is_waterbody(rest)) {
          wb_in_rest <- extract_waterbody_name(rest)
          if (!is.na(wb_in_rest)) {
            out$waterbody[i] <- wb_in_rest
            out$at_other[i] <- wb
            out$waterbody_type[i] <- detect_type(wb_in_rest)
            next
          }
        }

        # Extract waterbody name from wb (strip trailing descriptors)
        wb_extracted <- extract_waterbody_name(wb)
        if (!is.na(wb_extracted) && nchar(wb_extracted) < nchar(wb)) {
          wb_extra <- trimws(substring(wb, nchar(wb_extracted) + 1))
          wb_extra <- sub("^[-,;:]\\s*", "", wb_extra)
          wb_extra <- trimws(wb_extra)
          # Don't strip if remainder is a type keyword
          # (e.g., "Williams Lake River" - keep "River" as part of name)
          if (nchar(wb_extra) > 0 &&
              !tolower(wb_extra) %in% tolower(wb_types)) {
            wb <- wb_extracted
            rest <- paste(wb_extra, matched_prep, rest)
          }
        }

        out$waterbody[i] <- wb

        # Determine if reference is a waterbody or non-waterbody
        ref_wb <- extract_waterbody_name(rest)
        remainder <- if (!is.na(ref_wb)) {
          trimws(substring(rest, nchar(ref_wb) + 1))
        } else {
          ""
        }
        remainder <- sub("^[-,;:]\\s*", "", remainder)
        remainder <- trimws(remainder)

        # Only classify as upstream_of_waterbody when both parts are
        # waterbodies AND the preposition implies confluence (i.e., "at")
        if (!is.na(ref_wb) && is_waterbody(wb) &&
            matched_prep %in% confluence_preps) {
          if (nchar(remainder) == 0) {
            # Reference is exactly a waterbody (e.g., "Bighorn Creek")
            out$upstream_of_waterbody[i] <- ref_wb
          } else if (grepl(
            "^(Rd\\b|Road\\b|Rd\\.|St\\b|St\\.|Ave\\b|Dr\\b|Hwy\\b|Highway\\b|Blvd\\b)",
            remainder, ignore.case = TRUE
          )) {
            # Remainder starts with a road/address suffix -> whole thing
            # is a place name, not a waterbody reference
            other_val <- rest
            if (!is.na(station_code)) {
              other_val <- paste0(rest, " (", station_code, ")")
            }
            out$at_other[i] <- other_val
          } else {
            # Waterbody name + additional descriptor
            out$upstream_of_waterbody[i] <- ref_wb
            if (nchar(remainder) > 0) {
              out$at_other[i] <- remainder
            }
          }
        } else {
          # Not a waterbody reference -> at_other
          other_val <- rest
          if (!is.na(station_code)) {
            other_val <- paste0(rest, " (", station_code, ")")
          }
          out$at_other[i] <- other_val
        }

        out$waterbody_type[i] <- detect_type(wb)
        next
      }
    }

    # ------------------------------------------------------------------
    # 8. Dash-separated descriptors (e.g., "Gerrard Creek - Otter Rock",
    #    "Hill Creek - Fence 1")
    # ------------------------------------------------------------------
    if (grepl("\\s+-\\s+", s) && is_waterbody(s)) {
      parts <- strsplit(s, "\\s+-\\s+")[[1]]
      wb_part <- trimws(parts[1])
      desc_part <- trimws(paste(parts[-1], collapse = " - "))

      if (is_waterbody(wb_part)) {
        out$waterbody[i] <- wb_part
        if (nchar(desc_part) > 0) {
          out$at_other[i] <- desc_part
        }
        out$waterbody_type[i] <- detect_type(wb_part)
        next
      } else if (is_waterbody(desc_part)) {
        # Prefix is not a waterbody but the part after dash is
        # e.g., "Bosk - Horsefly River u/s of ..."
        # Re-parse with the prefix stripped and use it as a descriptor
        s <- desc_part
        # Fall through to continue parsing
      }
    }

    # ------------------------------------------------------------------
    # 9. Simple "<Waterbody> <Descriptor>" patterns
    #    e.g., "Goat River Mid", "Penticton Creek Headwaters"
    # ------------------------------------------------------------------
    if (is_waterbody(s)) {
      wb_name <- extract_waterbody_name(s)
      if (!is.na(wb_name)) {
        remainder <- trimws(substring(s, nchar(wb_name) + 1))

        out$waterbody[i] <- wb_name
        if (nchar(remainder) > 0) {
          # Strip leading dash or other punctuation
          remainder <- sub("^[-,;:]\\s*", "", remainder)
          remainder <- trimws(remainder)
          if (nchar(remainder) > 0) {
            out$at_other[i] <- remainder
          }
        }

        if (!is.na(station_code) && is.na(out$at_other[i])) {
          out$at_other[i] <- station_code
        } else if (!is.na(station_code) && !is.na(out$at_other[i])) {
          out$at_other[i] <- paste0(out$at_other[i], " (",
                                    station_code, ")")
        }

        out$waterbody_type[i] <- detect_type(wb_name)
        next
      }
    }

    # ------------------------------------------------------------------
    # 10. Default: no recognized pattern - use string as waterbody name
    # ------------------------------------------------------------------
    out$waterbody[i] <- s
    out$waterbody_type[i] <- detect_type(s)

    if (!is.na(station_code)) {
      out$at_other[i] <- station_code
    }
  }

  # --- Post-processing: expand well-known river short-hand names -----------
  wb_cols <- c("waterbody", "upstream_of_waterbody",
               "downstream_of_waterbody", "unnamed_tributary_of")
  for (i in seq_len(n)) {
    for (col in wb_cols) {
      val <- out[[col]][i]
      if (is.na(val) || val == "") next
      expanded <- expand_well_known(val)
      if (expanded$name != val) {
        out[[col]][i] <- expanded$name
        # Update waterbody_type if this is the primary waterbody column
        if (col == "waterbody" && !is.null(expanded$type)) {
          out$waterbody_type[i] <- expanded$type
        }
      }
    }

    # Check upstream_of_other / downstream_of_other: if the value matches a
    # well-known river, move it to the corresponding waterbody column
    for (pair in list(
      c("upstream_of_other", "upstream_of_waterbody"),
      c("downstream_of_other", "downstream_of_waterbody")
    )) {
      other_col <- pair[1]
      wb_col <- pair[2]
      val <- out[[other_col]][i]
      if (is.na(val) || val == "") next
      expanded <- expand_well_known(val)
      if (expanded$name != val && !is.null(expanded$type)) {
        # Only move if the waterbody column is currently empty
        if (is.na(out[[wb_col]][i])) {
          out[[wb_col]][i] <- expanded$name
          out[[other_col]][i] <- NA_character_
        }
      }
    }
  }

  out
}
