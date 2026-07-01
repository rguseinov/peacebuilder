#' Load conflict and revolutionary episode data
#'
#' @description
#' Loads selected conflict or revolutionary episode datasets included with
#' the package.
#'
#' @param start_year First year to include.
#' @param end_year Last year to include.
#' @param dataset Dataset to load. One of `"navco1.3"`, `"navco2.1"`,
#'   `"beissinger"`, or `"csra"`.
#' @param coding_system Country coding system. Either `"cow"` or `"gw"`.
#'
#' @return A data frame containing the selected conflict dataset.
#'
#' @export
conflict_data <- function(
    start_year = 1945,
    end_year = 2013,
    dataset = c('navco1.3', 'navco2.1', 'beissinger', 'csra'),
    coding_system = c("cow", "gw")
){

  check_year_range(start_year, end_year)

  dataset <- match.arg(dataset)
  coding_system <- match.arg(coding_system)

  file_name <- dplyr::case_when(
    dataset == "navco1.3" ~ "NAVCO_1.3.RData",
    dataset == "navco2.1" ~ "NAVCO_2.1.RData",
    dataset == "beissinger" ~ "revolutionary_episodes_beissinger.csv",
    dataset == "csra" ~ "revolutionary_episodes_csra.csv"
  )

  path <- system.file(
    "extdata",
    file_name,
    package = "peacebuilder"
  )

  if (path == "") {
    stop(
      "Could not find `", file_name, "` in package extdata.",
      call. = FALSE
    )
  }

  if (grepl("\\.RData$", file_name)) {
    data <- read_rdata_from_extdata(file_name)
  } else {
    data <- readr::read_csv(path, show_col_types = FALSE)
  }

  if (dataset == 'navco1.3'){
    # FIX: ONSET was computed before rename_with, putting it beyond the rename
    # range (ncol(data)+1), so it was never prefixed and was dropped by
    # starts_with('nvc1.3_') in select. Solution: add it after rename_with,
    # already named with the prefix.
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 7:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 'CAMPAIGN') %>%
        dplyr::mutate(
          nvc1.3_ONSET = 1L,
          cow = suppressWarnings(countrycode::countrycode(.data$LOCATION, 'country.name', 'cown')),
          cow = ifelse(.data$LOCATION == 'Serbia', 345L, .data$cow)
        ) %>%
        dplyr::rename(year = .data$BYEAR) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with('nvc1.3_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 7:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 'CAMPAIGN') %>%
        dplyr::mutate(
          nvc1.3_ONSET = 1L,
          gw = suppressWarnings(countrycode::countrycode(.data$LOCATION, 'country.name', 'gwn')),
          gw = ifelse(.data$LOCATION == 'Serbia', 340L, .data$gw)
        ) %>%
        dplyr::rename(year = .data$BYEAR) %>%
        # nvc1.3_EYEAR already captured by starts_with; explicit mention removed
        dplyr::select(.data$gw, .data$year, dplyr::starts_with('nvc1.3_'))
    }
  }

  if (dataset == 'navco2.1'){
    # FIX: same ONSET positional bug as navco1.3. Also removed commented-out
    # dead code.
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 18:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 'camp_name') %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
          nvc2.1_ONSET = dplyr::if_else(
            .data$year == min(.data$year, na.rm = TRUE),
            1L,
            0L
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename(cow = .data$loc_cow) %>%
        dplyr::mutate(cow = ifelse(.data$location == 'Serbia', 345L, .data$cow)) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with('nvc2.1_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 18:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 'camp_name') %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
          nvc2.1_ONSET = dplyr::if_else(
            .data$year == min(.data$year, na.rm = TRUE),
            1L,
            0L
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(gw = suppressWarnings(countrycode::countrycode(.data$loc_cow, 'cown', 'gwn'))) %>%
        dplyr::mutate(gw = ifelse(.data$location == 'Serbia', 340L, .data$gw)) %>%
        dplyr::select(.data$gw, .data$year, dplyr::starts_with('nvc2.1_'))
    }
  }

  if (dataset == 'beissinger'){
    # FIX 1: rename_with(.cols = 11:ncol(data)) started at col 11, but endyear
    # is at col 10, so select(.data$beissinger_endyear) failed. Solution:
    # explicitly rename endyear before the batch prefix rename.
    #
    # FIX 2: cowcode is at col 18 (>= 11), so batch rename made it
    # 'beissinger_cowcode', then rename(cow = .data$cowcode) failed. Solution:
    # derive cow/gw from cowcode BEFORE the batch rename.
    #
    # FIX 3 (gw path): .cols = 'CAMPAIGN' → 'CAMPAIGN' does not exist in
    # beissinger. Correct column is 'nameofrevolution'.
    #
    # FIX 4: ONSET had same positional bug as navco datasets.
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::mutate(
          cow = suppressWarnings(countrycode::countrycode(.data$cowcode, 'cown', 'cown')),
          cow = ifelse(.data$location == 'Serbia', 345L, .data$cow)
        ) %>%
        dplyr::rename(
          year             = .data$startyear,
          beissinger_endyear = .data$endyear
        ) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 11:ncol(data)) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 'nameofrevolution') %>%
        dplyr::mutate(beissinger_onset = 1L) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with('beissinger_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(
          gw = suppressWarnings(countrycode::countrycode(.data$cowcode, 'cown', 'gwn')),
          gw = ifelse(.data$location == 'Serbia', 340L, .data$gw)
        ) %>%
        dplyr::rename(
          year               = .data$startyear,
          beissinger_endyear = .data$endyear
        ) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 11:ncol(data)) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 'nameofrevolution') %>%
        dplyr::mutate(beissinger_onset = 1L) %>%
        dplyr::select(.data$gw, .data$year, dplyr::starts_with('beissinger_'))
    }
  }

  if (dataset == 'csra'){
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename(onset = all) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 10:ncol(data)) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 'name') %>%
        dplyr::mutate(cow = suppressWarnings(countrycode::countrycode(.data$country, 'country.name', 'cown'))) %>%
        dplyr::mutate(cow = ifelse(.data$country == 'Serbia', 345L, .data$cow)) %>%
        dplyr::rename(year = .data$start_year) %>%
        dplyr::select(.data$cow, .data$year, .data$end_year, dplyr::starts_with('csra_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename(onset = all) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 10:ncol(data)) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 'name') %>%
        dplyr::mutate(gw = suppressWarnings(countrycode::countrycode(.data$country, 'country.name', 'gwn'))) %>%
        dplyr::mutate(gw = ifelse(.data$country == 'Serbia', 340L, .data$gw)) %>%
        dplyr::rename(year = .data$start_year) %>%
        dplyr::select(.data$gw, .data$year, .data$end_year, dplyr::starts_with('csra_'))
    }
  }
  return(data)
}
