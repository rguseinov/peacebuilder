#' Build a state panel
#'
#' @param start_year First year.
#' @param end_year Last year.
#' @param coding_system Country coding system. Either `"cow"` or `"gw"`.
#' @param exclude_microstates Logical.
#' @param exclude_non_un Logical.
#' @param exclude_islands Logical.
#'
#' @return A data frame with state-year observations.
#'
#' @export
build_states_panel <- function(
    start_year = 1946,
    end_year = 2013,
    coding_system = c("cow", "gw"),
    exclude_microstates = TRUE,
    exclude_non_un = TRUE,
    exclude_islands = FALSE
) {
  check_year_range(start_year, end_year)
  coding_system <- match.arg(coding_system)
  if (coding_system == "cow") {
    return(
      build_states_cow_panel(
        start_year = start_year,
        end_year = end_year,
        exclude_microstates = exclude_microstates,
        exclude_non_un = exclude_non_un,
        exclude_islands = exclude_islands
      )
    )
  }
  if (coding_system == "gw") {
    return(
      build_states_gw_panel(
        start_year = start_year,
        end_year = end_year,
        exclude_microstates = exclude_microstates,
        exclude_non_un = exclude_non_un
      )
    )
  }
}

build_states_cow_panel <- function(
    start_year = 1946,
    end_year = 2013,
    exclude_microstates = TRUE,
    exclude_non_un = TRUE,
    exclude_islands = FALSE
) {

  check_year_range(start_year, end_year)

  microstates <- states::cowstates %>%
    dplyr::filter(.data$microstate) %>%
    dplyr::distinct(.data$cowcode)

  states <- states::state_panel(
    start = start_year,
    end = end_year,
    by = "year",
    useGW = FALSE
  ) %>%
    dplyr::rename(cow = .data$cowcode)

  #Exclude Kosovo, Taiwan, Hong Kong, EU, South Vietnam
  if (exclude_non_un) {
    non_un_entities <- c(347, 713, 817, 995, 997)
    states <- states %>%
      dplyr::filter(!.data$cow %in% non_un_entities)
  }

  #Exclude Dominica, Grenada, Saint Lucia, Saint Vincent and the Grenadines, Antigua and Barbuda, Saint Kitts and Nevis
  if (exclude_islands) {
    small_island_states <- c(54, 55, 56, 57, 58, 60)
    states <- states %>%
      dplyr::filter(!.data$cow %in% small_island_states)
  }

  if (exclude_microstates) {
    states <- states %>%
      dplyr::filter(!(.data$cow %in% microstates$cowcode))
  }

  states <- states %>%
    dplyr::mutate(
      country = suppressWarnings(countrycode::countrycode(.data$cow, "cown", "country.name")),
      country = ifelse(.data$cow == 260, "German Federal Republic", .data$country),
      country = ifelse(.data$country == "Yugoslavia" & .data$year >= 2006, "Serbia", .data$country),
      cow = dplyr::case_when(
        .data$cow == 315 ~ 316,
        .data$cow == 260 ~ 255,
        TRUE ~ .data$cow
      )
    ) %>%
    tidyr::drop_na(country) %>%
    dplyr::filter(!(.data$cow == 255 & .data$year == 1990 & .data$country == "German Federal Republic")) %>%
    dplyr::arrange(.data$cow, .data$year)

  check_unique_key(states, c("cow", "year"))

  return(states)
}

build_states_gw_panel <- function(
    start_year = 1946,
    end_year = 2013,
    exclude_microstates = TRUE,
    exclude_non_un = TRUE
) {

  check_year_range(start_year, end_year)

  microstates <- states::gwstates %>%
    dplyr::filter(.data$microstate) %>%
    dplyr::distinct(.data$gwcode)

  states <- states::state_panel(
    start = start_year,
    end = end_year,
    by = "year",
    useGW = TRUE
  ) %>%
    dplyr::rename(gw = .data$gwcode)

  #Exclude Kosovo, Taiwan, South Vietnam
  if (exclude_non_un) {
    non_un_entities <- c(347, 713, 817)
    states <- states %>%
      dplyr::filter(!.data$gw %in% non_un_entities)
  }

  if (exclude_microstates) {
    states <- states %>%
      dplyr::filter(!(.data$gw %in% microstates$gwcode))
  }

  states <- states %>%
    dplyr::mutate(
      country = suppressWarnings(countrycode::countrycode(.data$gw, "gwn", "country.name")),
      country = ifelse(.data$gw == 260, "German Federal Republic", .data$country),
      country = ifelse(.data$gw == 255, "Germany", .data$country),
      country = ifelse(.data$country == "Yugoslavia" & .data$year >= 2006, "Serbia", .data$country),
      #gw = dplyr::case_when(
        #.data$gw == 315 ~ 316,
        #.data$gw == 255 ~ 260,
        #.data$gw == 345 ~ 340,
      #  TRUE ~ .data$gw
      #)
    ) %>%
    tidyr::drop_na(gw) %>%
    tidyr::drop_na(country) %>%
    #dplyr::filter(!(.data$gw == 255 & .data$year == 1990 & .data$country == "German Federal Republic")) %>%
    dplyr::arrange(.data$gw, .data$year)

  check_unique_key(states, c("gw", "year"))

  return(states)
}
