#' Load GDP per capita data
#'
#' @description
#' Loads GDP per capita data from the Gapminder dataset included with the
#' package and returns a country-year dataset using either COW or
#' Gleditsch-Ward country codes.
#'
#' @param start_year Integer. First year to include.
#' @param end_year Integer. Last year to include.
#' @param coding_system Character. Country coding system to use.
#'   Either `"cow"` for Correlates of War codes or `"gw"` for
#'   Gleditsch-Ward codes.
#'
#' @return A data frame with country-year GDP indicators. If
#'   `coding_system = "cow"`, the data frame contains a `cow` column.
#'   If `coding_system = "gw"`, it contains a `gw` column. The returned
#'   data also include `year`, `gdp_pcap`, `log_gdp_pcap`, and
#'   `gdp_growth`.
#'
#' @source
#' GDP per capita data from Gapminder. Gapminder data are distributed under
#' CC BY 4.0; please cite Gapminder and the original data providers.
#'
#' @examples
#' gdp_cow <- load_gdp_data(coding_system = "cow")
#'
#' gdp_gw <- load_gdp_data(coding_system = "gw")
#'
#' @export
load_gdp_data <- function(
    start_year = 1945,
    end_year = 2019,
    coding_system = c("cow", "gw")
    ) {
  path <- system.file(
    "extdata",
    "gdp_gapminder_v32.csv",
    package = "peacebuilder"
  )

  if (path == "") {
    stop(
      "Could not find `gdp_gapminder_v32.csv` in package extdata.",
      call. = FALSE
    )
  }

  coding_system <- match.arg(coding_system)
  gdp_source <- readr::read_csv(path, show_col_types = FALSE)

  if (coding_system == "cow") {
    gdp_data <- gdp_source %>%
      dplyr::mutate(
        cow = suppressWarnings(countrycode::countrycode(
          .data$name,
          origin = "country.name",
          destination = "cown"
        )),
        cow = dplyr::case_when(
          .data$name == "Serbia" ~ 345,
          .data$name == "China" ~ 710,
          TRUE ~ .data$cow
        )
      ) %>%
      tidyr::drop_na(.data$cow) %>%
      dplyr::arrange(.data$cow, .data$year) %>%
      dplyr::group_by(.data$cow) %>%
      dplyr::mutate(gdp_growth = ((.data$gdp_pcap - dplyr::lag(.data$gdp_pcap)) / dplyr::lag(.data$gdp_pcap) * 100),
             log_gdp_pcap = log(.data$gdp_pcap)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::select(.data$cow, .data$year, .data$gdp_pcap, .data$log_gdp_pcap, .data$gdp_growth)
  }

  if (coding_system == "gw") {
    gdp_data <- gdp_source %>%
      dplyr::mutate(
        gw = suppressWarnings(countrycode::countrycode(
          .data$name,
          origin = "country.name",
          destination = "gwn"
        )),
        gw = dplyr::case_when(
          .data$name == "Serbia" ~ 340,
          .data$name == "China" ~ 710,
          TRUE ~ .data$gw
        )
      ) %>%
      tidyr::drop_na(.data$gw) %>%
      dplyr::arrange(.data$gw, .data$year) %>%
      dplyr::group_by(.data$gw) %>%
      dplyr::mutate(gdp_growth = ((.data$gdp_pcap - dplyr::lag(.data$gdp_pcap)) / dplyr::lag(.data$gdp_pcap) * 100),
             log_gdp_pcap = log(.data$gdp_pcap)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::select(.data$gw, .data$year, .data$gdp_pcap, .data$log_gdp_pcap, .data$gdp_growth)
  }

  return(gdp_data)
}
