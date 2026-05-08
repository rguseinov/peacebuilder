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
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::mutate(ONSET = 1L) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 7:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 'CAMPAIGN') %>%
        dplyr::mutate(cow = suppressWarnings(countrycode::countrycode(.data$LOCATION, 'country.name', 'cown'))) %>%
        dplyr::mutate(cow = ifelse(.data$LOCATION == 'Serbia', 345, .data$cow)) %>%
        dplyr::rename(year = .data$BYEAR) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with('nvc1.3_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(ONSET = 1L) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 7:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc1.3_", .), .cols = 'CAMPAIGN') %>%
        dplyr::mutate(gw = suppressWarnings(countrycode::countrycode(.data$LOCATION, 'country.name', 'gwn'))) %>%
        dplyr::mutate(gw = ifelse(.data$LOCATION == 'Serbia', 340, .data$gw)) %>%
        dplyr::rename(year = .data$BYEAR) %>%
        dplyr::select(.data$gw, .data$year, .data$nvc1.3_EYEAR, dplyr::starts_with('nvc1.3_'))
    }
  }

  if (dataset == 'navco2.1'){
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
          ONSET = dplyr::if_else(
            .data$year == min(.data$year, na.rm = TRUE),
            1L,
            0L
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 18:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 'camp_name') %>%
        dplyr::rename(cow = .data$loc_cow) %>%
        #dplyr::mutate(cow = suppressWarnings(countrycode::countrycode(.data$LOCATION, 'country.name', 'cown'))) %>%
        dplyr::mutate(cow = ifelse(.data$location == 'Serbia', 345, .data$cow)) %>%
        dplyr::select(.data$cow, .data$year, dplyr::starts_with('nvc2.1_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
          ONSET = dplyr::if_else(
            .data$year == min(.data$year, na.rm = TRUE),
            1L,
            0L
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 18:ncol(data)) %>%
        dplyr::rename_with(~ paste0("nvc2.1_", .), .cols = 'camp_name') %>%
        dplyr::mutate(gw = suppressWarnings(countrycode::countrycode(.data$loc_cow, 'cown', 'gwn'))) %>%
        dplyr::mutate(gw = ifelse(.data$location == 'Serbia', 340, .data$gw)) %>%
        dplyr::select(.data$gw, .data$year, dplyr::starts_with('nvc2.1_'))
    }
  }

  if (dataset == 'beissinger'){
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::mutate(onset = 1L) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 11:ncol(data)) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 'nameofrevolution') %>%
        dplyr::rename(cow = .data$cowcode) %>%
        dplyr::mutate(cow = ifelse(.data$location == 'Serbia', 345, .data$cow)) %>%
        dplyr::rename(year = .data$startyear) %>%
        dplyr::select(.data$cow, .data$year, .data$beissinger_endyear, dplyr::starts_with('beissinger_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(onset = 1L) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 11:ncol(data)) %>%
        dplyr::rename_with(~ paste0("beissinger_", .), .cols = 'CAMPAIGN') %>%
        dplyr::mutate(gw = suppressWarnings(countrycode::countrycode(.data$beissinger_cowcode, 'cown', 'gwn'))) %>%
        dplyr::mutate(gw = ifelse(.data$location == 'Serbia', 340, .data$gw)) %>%
        dplyr::rename(year = .data$startyear) %>%
        dplyr::select(.data$gw, .data$year, .data$beissinger_endyear, dplyr::starts_with('beissinger_'))
    }
  }

  if (dataset == 'csra'){
    if (coding_system == "cow") {
      data <- data %>%
        dplyr::rename(onset = all) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 10:ncol(data)) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 'name') %>%
        dplyr::mutate(cow = suppressWarnings(countrycode::countrycode(.data$country, 'country.name', 'cown'))) %>%
        dplyr::mutate(cow = ifelse(.data$country == 'Serbia', 345, .data$cow)) %>%
        dplyr::rename(year = .data$start_year) %>%
        dplyr::select(.data$cow, .data$year, .data$end_year, dplyr::starts_with('csra_'))
    }

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::rename(onset = all) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 10:ncol(data)) %>%
        dplyr::rename_with(~ paste0("csra_", .), .cols = 'name') %>%
        dplyr::mutate(gw = suppressWarnings(countrycode::countrycode(.data$country, 'country.name', 'gwn'))) %>%
        dplyr::mutate(gw = ifelse(.data$country == 'Serbia', 340, .data$gw)) %>%
        dplyr::rename(year = .data$start_year) %>%
        dplyr::select(.data$gw, .data$year, .data$end_year, dplyr::starts_with('csra_'))
    }
  }
  return(data)
}
