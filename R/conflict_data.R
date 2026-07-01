#' Load conflict and mobilisation data
#'
#' @description
#' Loads one of nine conflict, protest, or revolutionary episode datasets
#' bundled with the package. All new datasets (`scad`, `ucdp_prio`, `ucdp_vpp`,
#' `mm`, `mmad`) are returned at the **country-year** level; event- or
#' campaign-level sources are pre-aggregated inside this function.
#'
#' For legacy campaign datasets (`navco1.3`, `navco2.1`, `beissinger`, `csra`)
#' the function returns campaign-year rows. Use `add_conflict(aggregate = TRUE)`
#' (the default) to collapse these to country-year automatically.
#'
#' @param start_year First year to include.
#' @param end_year Last year to include.
#' @param dataset Dataset to load. One of:
#'   \describe{
#'     \item{`"navco1.3"`}{NAVCO 1.3 nonviolent campaign onsets. Coverage: 1900-2006.}
#'     \item{`"navco2.1"`}{NAVCO 2.1 campaign-years. Coverage: 1945-2013.}
#'     \item{`"beissinger"`}{Beissinger revolutionary episode onsets. Coverage: 1900-2014.}
#'     \item{`"csra"`}{HSE CSRA revolutionary episodes. Coverage: 1900-2022.}
#'     \item{`"scad"`}{SCAD 2018 social conflict events, Africa and Latin America.
#'       Coverage: 1990-2018. Prefix: `scad_`.}
#'     \item{`"ucdp_prio"`}{UCDP/PRIO Armed Conflict Dataset v26.1.
#'       Coverage: 1946-2025. Prefix: `ucdp_prio_`.}
#'     \item{`"ucdp_vpp"`}{UCDP Violent Political Protest Dataset v26.1.
#'       Coverage: 1989-2025. Prefix: `ucdp_vpp_`. Requires the `readxl` package.}
#'     \item{`"mm"`}{Mass Mobilization Project v4 (Clark and Regan).
#'       Coverage: 1990-2019. Prefix: `mm_`.}
#'     \item{`"mmad"`}{Mass Mobilization in Autocracies Database.
#'       Coverage: 2003-2022. Prefix: `mmad_`.}
#'   }
#' @param coding_system Country coding system: `"cow"` or `"gw"`.
#'   `ucdp_prio` and `ucdp_vpp` use GW codes natively; when
#'   `coding_system = "cow"` they are converted via `countrycode` and
#'   countries without a COW equivalent are dropped.
#'
#' @return A data frame. Legacy datasets: one row per campaign per country-year.
#'   New datasets: one row per country-year.
#'
#' @examples
#' navco <- conflict_data(1990, 2010, dataset = "navco2.1", coding_system = "cow")
#'
#' scad  <- conflict_data(1995, 2015, dataset = "scad",      coding_system = "cow")
#'
#' ucdp  <- conflict_data(1990, 2020, dataset = "ucdp_prio", coding_system = "gw")
#'
#' @export
conflict_data <- function(
    start_year    = 1945,
    end_year      = 2013,
    dataset       = c("navco1.3", "navco2.1", "beissinger", "csra",
                      "scad", "ucdp_prio", "ucdp_vpp", "mm", "mmad"),
    coding_system = c("cow", "gw")
) {
  check_year_range(start_year, end_year)
  dataset       <- match.arg(dataset)
  coding_system <- match.arg(coding_system)

  # max() that returns NA instead of -Inf / Inf when every value in a group is NA
  .safe_max <- function(x) {
    r <- suppressWarnings(max(as.numeric(x), na.rm = TRUE))
    if (is.infinite(r)) NA_real_ else r
  }

  # ── Legacy datasets: load once, then process in per-dataset blocks ────────
  legacy_datasets <- c("navco1.3", "navco2.1", "beissinger", "csra")

  if (dataset %in% legacy_datasets) {
    file_name <- dplyr::case_when(
      dataset == "navco1.3"   ~ "NAVCO_1.3.RData",
      dataset == "navco2.1"   ~ "NAVCO_2.1.RData",
      dataset == "beissinger" ~ "revolutionary_episodes_beissinger.csv",
      dataset == "csra"       ~ "revolutionary_episodes_csra.csv"
    )
    path <- system.file("extdata", file_name, package = "peacebuilder")
    if (path == "") {
      stop("Could not find `", file_name, "` in package extdata.", call. = FALSE)
    }
    if (grepl("\\.RData$", file_name)) {
      data <- read_rdata_from_extdata(file_name)
    } else {
      # suppressMessages: Beissinger CSV has a duplicate "ongoing" column;
      # readr renames them to ongoing...14 / ongoing...77 and prints a message
      data <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    }
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # SCAD 2018 - Social Conflict in Africa / Latin America
  # Event-level -> aggregated to country-year inside this function
  # ═══════════════════════════════════════════════════════════════════════════
  if (dataset == "scad") {
    path_africa <- system.file("extdata", "SCAD2018Africa_Final.csv",       package = "peacebuilder")
    path_latam  <- system.file("extdata", "SCAD2018LatinAmerica_Final.csv", package = "peacebuilder")
    if (path_africa == "" || path_latam == "") {
      stop("Could not find SCAD CSV files in package extdata.", call. = FALSE)
    }

    africa <- readr::read_csv(path_africa, show_col_types = FALSE,
                              locale = readr::locale(encoding = "latin1"))
    latam  <- readr::read_csv(path_latam,  show_col_types = FALSE,
                              locale = readr::locale(encoding = "latin1"))

    # Africa uses typo "lgtbq_issue"; LatAm uses correct "lgbtq_issue" - harmonise
    if ("lgtbq_issue" %in% names(africa)) {
      africa <- dplyr::rename(africa, lgbtq_issue = .data$lgtbq_issue)
    }

    raw <- dplyr::bind_rows(africa, latam)

    # Substantive numeric columns to keep; -9 / -99 / "" -> NA
    keep_scad <- c("ccode", "styr", "etype", "escalation", "npart", "ndeath",
                   "repress", "cgovtarget", "rgovtarget", "female_event", "lgbtq_issue")

    data <- raw %>%
      dplyr::select(dplyr::all_of(keep_scad)) %>%
      dplyr::mutate(
        dplyr::across(
          -dplyr::all_of(c("ccode", "styr")),
          ~ suppressWarnings(as.integer(
            ifelse(. %in% c(-9L, -99L, "-9", "-99", ""), NA_integer_, as.integer(.))
          ))
        )
      ) %>%
      dplyr::filter(.data$styr >= start_year & .data$styr <= end_year) %>%
      dplyr::mutate(cow = as.integer(.data$ccode)) %>%
      dplyr::rename(year = .data$styr) %>%
      dplyr::group_by(.data$cow, .data$year) %>%
      dplyr::summarise(
        scad_onset          = 1L,
        scad_n_events       = dplyr::n(),
        scad_ndeath_total   = sum(.data$ndeath,      na.rm = TRUE),
        scad_npart_max      = as.integer(.safe_max(.data$npart)),
        scad_escalation_max = as.integer(.safe_max(.data$escalation)),
        scad_repress_max    = as.integer(.safe_max(.data$repress)),
        scad_cgovtarget     = as.integer(.safe_max(.data$cgovtarget)),
        scad_rgovtarget     = as.integer(.safe_max(.data$rgovtarget)),
        scad_female_event   = as.integer(.safe_max(.data$female_event)),
        scad_lgbtq_issue    = as.integer(.safe_max(.data$lgbtq_issue)),
        .groups = "drop"
      )

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(gw = suppressWarnings(
          countrycode::countrycode(.data$cow, "cown", "gwn")
        )) %>%
        tidyr::drop_na(.data$gw) %>%
        dplyr::select(-.data$cow) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    } else {
      data <- dplyr::select(data, .data$cow, .data$year, dplyr::everything())
    }
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # UCDP/PRIO Armed Conflict Dataset v26.1
  # Conflict-year; gwno_loc can be comma-separated for multi-country conflicts
  # -> expanded and aggregated to country-year
  # ═══════════════════════════════════════════════════════════════════════════
  if (dataset == "ucdp_prio") {
    path <- system.file("extdata", "UcdpPrioConflict_v26_1.csv", package = "peacebuilder")
    if (path == "") {
      stop("Could not find `UcdpPrioConflict_v26_1.csv` in package extdata.", call. = FALSE)
    }

    raw <- readr::read_csv(path, show_col_types = FALSE)

    # Drop: free-text party/location names, date strings, version string
    # region excluded: source has comma-separated values like "1, 3" for multi-region
    # conflicts; as.integer() would introduce NA. Region is not analytically useful
    # at country-year level after gwno_loc expansion.
    keep_ucdp <- c("gwno_loc", "year", "incompatibility", "intensity_level",
                   "cumulative_intensity", "type_of_conflict", "ep_end")

    data <- raw %>%
      dplyr::select(dplyr::all_of(keep_ucdp)) %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      # Expand comma-separated GW codes (159 multi-country conflicts in v26.1)
      dplyr::mutate(gwno_loc = strsplit(.data$gwno_loc, ",\\s*")) %>%
      tidyr::unnest(.data$gwno_loc) %>%
      dplyr::mutate(
        gwno_loc             = as.integer(trimws(.data$gwno_loc)),
        incompatibility      = as.integer(.data$incompatibility),
        intensity_level      = as.integer(.data$intensity_level),
        cumulative_intensity = as.integer(.data$cumulative_intensity),
        type_of_conflict     = as.integer(.data$type_of_conflict),
        ep_end               = as.integer(.data$ep_end)
      ) %>%
      dplyr::group_by(.data$gwno_loc, .data$year) %>%
      dplyr::summarise(
        ucdp_prio_onset                = 1L,
        ucdp_prio_n_conflicts          = dplyr::n(),
        ucdp_prio_intensity_max        = as.integer(.safe_max(.data$intensity_level)),
        ucdp_prio_war                  = as.integer(.safe_max(.data$intensity_level) == 2),
        ucdp_prio_cumulative_intensity = as.integer(.safe_max(.data$cumulative_intensity)),
        ucdp_prio_type_max             = as.integer(.safe_max(.data$type_of_conflict)),
        ucdp_prio_intrastate           = as.integer(
          any(.data$type_of_conflict %in% c(3L, 4L), na.rm = TRUE)
        ),
        ucdp_prio_interstate           = as.integer(
          any(.data$type_of_conflict == 2L, na.rm = TRUE)
        ),
        ucdp_prio_incompatibility_max  = as.integer(.safe_max(.data$incompatibility)),
        ucdp_prio_ep_end               = as.integer(.safe_max(.data$ep_end)),
        .groups = "drop"
      )

    if (coding_system == "cow") {
      data <- data %>%
        dplyr::mutate(
          cow = suppressWarnings(
            as.integer(countrycode::countrycode(.data$gwno_loc, "gwn", "cown"))
          )
        ) %>%
        tidyr::drop_na(.data$cow) %>%
        dplyr::select(-.data$gwno_loc) %>%
        dplyr::select(.data$cow, .data$year, dplyr::everything())
    } else {
      data <- data %>%
        dplyr::rename(gw = .data$gwno_loc) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    }
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # UCDP Violent Political Protest Dataset v26.1
  # Dyad-year -> aggregated to country-year
  # NOTE: "Terrority" is a spelling error in the source data (should be "Territory")
  # Requires: readxl
  # ═══════════════════════════════════════════════════════════════════════════
  if (dataset == "ucdp_vpp") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package 'readxl' is required to load UCDP VPP data. ",
        "Install it with: install.packages('readxl')",
        call. = FALSE
      )
    }
    path <- system.file("extdata", "UCDP_VPP_Dataset_v26_1.xlsx", package = "peacebuilder")
    if (path == "") {
      stop("Could not find `UCDP_VPP_Dataset_v26_1.xlsx` in package extdata.", call. = FALSE)
    }

    raw <- readxl::read_xlsx(path)

    # Drop: dyad/side/location/region text, outcome text, version string
    data <- raw %>%
      dplyr::select(
        gwno_loc        = "GWNOLoc",
        year            = "Year",
        region_id       = "Region ID",
        incompatibility = "Incompatibility",
        intensity       = "Intensity"
      ) %>%
      dplyr::mutate(
        gwno_loc  = as.integer(.data$gwno_loc),
        year      = as.integer(.data$year),
        region_id = as.integer(.data$region_id),
        intensity = as.integer(.data$intensity),
        # Convert text incompatibility to numeric (typo "Terrority" preserved as-is from source)
        incompatibility_num = dplyr::case_when(
          .data$incompatibility == "Government" ~ 1L,
          .data$incompatibility == "Terrority"  ~ 2L,
          TRUE                                  ~ NA_integer_
        )
      ) %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::group_by(.data$gwno_loc, .data$year) %>%
      dplyr::summarise(
        ucdp_vpp_onset              = 1L,
        ucdp_vpp_n_dyads            = dplyr::n(),
        ucdp_vpp_intensity_max      = as.integer(.safe_max(.data$intensity)),
        ucdp_vpp_region_id          = as.integer(.safe_max(.data$region_id)),
        ucdp_vpp_gov_conflict       = as.integer(
          any(.data$incompatibility_num == 1L, na.rm = TRUE)
        ),
        ucdp_vpp_territory_conflict = as.integer(
          any(.data$incompatibility_num == 2L, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    if (coding_system == "cow") {
      data <- data %>%
        dplyr::mutate(
          cow = suppressWarnings(
            as.integer(countrycode::countrycode(.data$gwno_loc, "gwn", "cown"))
          )
        ) %>%
        tidyr::drop_na(.data$cow) %>%
        dplyr::select(-.data$gwno_loc) %>%
        dplyr::select(.data$cow, .data$year, dplyr::everything())
    } else {
      data <- data %>%
        dplyr::rename(gw = .data$gwno_loc) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    }
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # Mass Mobilization Project v4 (Clark & Regan 2016)
  # Event-level -> aggregated to country-year
  # Column list decoded from binary RData:
  #   id, country, ccode, year, region, protest, protestnumber,
  #   startday/month/year, endday/month/year, protesterviolence, location,
  #   participants_category, participants, protesteridentity,
  #   protesterdemand1-4, stateresponse1-7, sources, notes
  # ═══════════════════════════════════════════════════════════════════════════
  if (dataset == "mm") {
    raw <- read_rdata_from_extdata("mmALL_073120.RData")

    # Drop: free-text identifiers, date detail, and redundant/uninformative columns
    drop_mm <- c("id", "country", "location", "sources", "notes",
                 "startday", "startmonth", "startyear",
                 "endday",   "endmonth",   "endyear",
                 "protest",               # always 1; redundant with mm_onset
                 "protestnumber",         # sequential event ID; max == mm_n_protests
                 "region",               # text label ("Africa" / "Latin America")
                 "participants_category") # text size category; use mm_participants instead

    # Numeric columns: aggregate with max() across events in country-year
    num_mm <- c("protesterviolence", "participants")

    # Text-coded categorical columns: collapse unique non-NA values with "; "
    # (these are stored as character/factor with text labels in the RData, NOT integer codes)
    text_mm <- c("protesteridentity",
                 "protesterdemand1", "protesterdemand2", "protesterdemand3", "protesterdemand4",
                 "stateresponse1", "stateresponse2", "stateresponse3",
                 "stateresponse4", "stateresponse5", "stateresponse6", "stateresponse7")

    data <- raw %>%
      dplyr::select(-dplyr::any_of(drop_mm)) %>%
      # Factors -> character first to preserve text labels (old-style R factor storage)
      dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
      # Coerce only the genuinely numeric columns
      dplyr::mutate(
        ccode             = suppressWarnings(as.integer(.data$ccode)),
        year              = suppressWarnings(as.integer(.data$year)),
        protesterviolence = suppressWarnings(as.integer(.data$protesterviolence)),
        participants      = suppressWarnings(as.numeric(.data$participants))
      ) %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::mutate(cow = .data$ccode) %>%
      dplyr::group_by(.data$cow, .data$year) %>%
      dplyr::summarise(
        mm_onset      = 1L,
        mm_n_protests = dplyr::n(),
        dplyr::across(
          dplyr::all_of(num_mm),
          ~ { r <- suppressWarnings(max(., na.rm = TRUE)); if (is.infinite(r)) NA_real_ else r },
          .names = "mm_{.col}"
        ),
        dplyr::across(
          dplyr::all_of(text_mm),
          ~ { vals <- sort(unique(na.omit(.))); if (length(vals) == 0L) NA_character_ else paste(vals, collapse = "; ") },
          .names = "mm_{.col}"
        ),
        .groups = "drop"
      )

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(gw = suppressWarnings(
          countrycode::countrycode(.data$cow, "cown", "gwn")
        )) %>%
        tidyr::drop_na(.data$gw) %>%
        dplyr::select(-.data$cow) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    } else {
      data <- dplyr::select(data, .data$cow, .data$year, dplyr::everything())
    }
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # Mass Mobilization in Autocracies Database (MMAD)
  # Event-level -> aggregated to country-year
  # side: 0 = pro-government, 1 = anti-government, 3 = other/unknown
  # ═══════════════════════════════════════════════════════════════════════════
  if (dataset == "mmad") {
    path <- system.file("extdata", "mmad_events.csv", package = "peacebuilder")
    if (path == "") {
      stop("Could not find `mmad_events.csv` in package extdata.", call. = FALSE)
    }

    raw <- readr::read_csv(path, show_col_types = FALSE)

    # Drop: geographic detail (GeoNames location ID, place name, coordinates)
    data <- raw %>%
      dplyr::select(
        .data$cowcode, .data$event_date, .data$side,
        .data$numreports, .data$max_scope, .data$max_partviolence,
        .data$max_secengagement, .data$mean_avg_numparticipants
      ) %>%
      dplyr::mutate(
        year = as.integer(substr(.data$event_date, 1L, 4L)),
        cow  = as.integer(.data$cowcode),
        side = as.integer(.data$side),
        # Source data stores missing values as the string "NA"
        numreports               = suppressWarnings(as.numeric(.data$numreports)),
        max_scope                = suppressWarnings(as.numeric(.data$max_scope)),
        max_partviolence         = suppressWarnings(as.numeric(.data$max_partviolence)),
        max_secengagement        = suppressWarnings(as.numeric(.data$max_secengagement)),
        mean_avg_numparticipants = suppressWarnings(as.numeric(.data$mean_avg_numparticipants))
      ) %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::group_by(.data$cow, .data$year) %>%
      dplyr::summarise(
        mmad_onset             = 1L,
        mmad_n_events          = dplyr::n(),
        mmad_n_anti_gov        = sum(.data$side == 1L, na.rm = TRUE),
        mmad_numreports_total  = sum(.data$numreports,  na.rm = TRUE),
        mmad_max_scope         = as.integer(.safe_max(.data$max_scope)),
        mmad_max_partviolence  = as.integer(.safe_max(.data$max_partviolence)),
        mmad_max_secengagement = as.integer(.safe_max(.data$max_secengagement)),
        mmad_mean_participants = mean(.data$mean_avg_numparticipants, na.rm = TRUE),
        .groups = "drop"
      )

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(gw = suppressWarnings(
          countrycode::countrycode(.data$cow, "cown", "gwn")
        )) %>%
        tidyr::drop_na(.data$gw) %>%
        dplyr::select(-.data$cow) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    } else {
      data <- dplyr::select(data, .data$cow, .data$year, dplyr::everything())
    }
  }

  # ═══════════════════════════════════════════════════════════════════════════
  # Legacy datasets
  # ═══════════════════════════════════════════════════════════════════════════
  if (dataset == "navco1.3") {
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 7:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = "CAMPAIGN") %>%
        dplyr::mutate(
          nvc1.3_ONSET = 1L,
          cow = suppressWarnings(countrycode::countrycode(.data$LOCATION, "country.name", "cown")),
          cow = ifelse(.data$LOCATION == "Serbia", 345L, .data$cow)
        ) %>%
        dplyr::rename(year = .data$BYEAR) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with("nvc1.3_"))
    }
    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 7:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = "CAMPAIGN") %>%
        dplyr::mutate(
          nvc1.3_ONSET = 1L,
          gw = suppressWarnings(countrycode::countrycode(.data$LOCATION, "country.name", "gwn")),
          gw = ifelse(.data$LOCATION == "Serbia", 340L, .data$gw)
        ) %>%
        dplyr::rename(year = .data$BYEAR) %>%
        dplyr::select(.data$gw, .data$year, dplyr::starts_with("nvc1.3_"))
    }
  }

  if (dataset == "navco2.1") {
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 18:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = "camp_name") %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
          nvc2.1_ONSET = dplyr::if_else(
            .data$year == min(.data$year, na.rm = TRUE), 1L, 0L
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename(cow = .data$loc_cow) %>%
        dplyr::mutate(cow = ifelse(.data$location == "Serbia", 345L, .data$cow)) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with("nvc2.1_"))
    }
    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 18:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = "camp_name") %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
          nvc2.1_ONSET = dplyr::if_else(
            .data$year == min(.data$year, na.rm = TRUE), 1L, 0L
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(gw = suppressWarnings(
          countrycode::countrycode(.data$loc_cow, "cown", "gwn")
        )) %>%
        dplyr::mutate(gw = ifelse(.data$location == "Serbia", 340L, .data$gw)) %>%
        dplyr::select(.data$gw, .data$year, dplyr::starts_with("nvc2.1_"))
    }
  }

  if (dataset == "beissinger") {
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::mutate(
          cow = suppressWarnings(countrycode::countrycode(.data$cowcode, "cown", "cown")),
          cow = ifelse(.data$location == "Serbia", 345L, .data$cow)
        ) %>%
        dplyr::rename(
          year               = .data$startyear,
          beissinger_endyear = .data$endyear
        ) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 11:ncol(data)) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = "nameofrevolution") %>%
        dplyr::mutate(beissinger_onset = 1L) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with("beissinger_"))
    }
    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(
          gw = suppressWarnings(countrycode::countrycode(.data$cowcode, "cown", "gwn")),
          gw = ifelse(.data$location == "Serbia", 340L, .data$gw)
        ) %>%
        dplyr::rename(
          year               = .data$startyear,
          beissinger_endyear = .data$endyear
        ) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 11:ncol(data)) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = "nameofrevolution") %>%
        dplyr::mutate(beissinger_onset = 1L) %>%
        dplyr::select(.data$gw, .data$year, dplyr::starts_with("beissinger_"))
    }
  }

  if (dataset == "csra") {
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename(onset = all) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 10:ncol(data)) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = "name") %>%
        dplyr::mutate(cow = suppressWarnings(
          countrycode::countrycode(.data$country, "country.name", "cown")
        )) %>%
        dplyr::mutate(cow = ifelse(.data$country == "Serbia", 345L, .data$cow)) %>%
        dplyr::rename(year = .data$start_year) %>%
        dplyr::select(.data$cow, .data$year, .data$end_year, dplyr::starts_with("csra_"))
    }
    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename(onset = all) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 10:ncol(data)) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = "name") %>%
        dplyr::mutate(gw = suppressWarnings(
          countrycode::countrycode(.data$country, "country.name", "gwn")
        )) %>%
        dplyr::mutate(gw = ifelse(.data$country == "Serbia", 340L, .data$gw)) %>%
        dplyr::rename(year = .data$start_year) %>%
        dplyr::select(.data$gw, .data$year, .data$end_year, dplyr::starts_with("csra_"))
    }
  }

  return(data)
}
