#' @source
#' GDP per capita data from Gapminder. Gapminder data are distributed under
#' CC BY 4.0; please cite Gapminder and the original data providers.
load_gdp_data <- function(coding_system = c("cow", "gw")) {
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
      dplyr::arrange(cow, year) %>%
      dplyr::group_by(cow) %>%
      dplyr::mutate(gdp_growth = ((gdp_pcap - dplyr::lag(gdp_pcap)) / dplyr::lag(gdp_pcap) * 100),
             log_gdp_pcap = log(gdp_pcap + 1)) %>%
      dplyr::ungroup() %>%
      dplyr::select(cow, year, gdp_pcap, log_gdp_pcap, gdp_growth)
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
          .data$name == "Serbia" ~ 345,
          .data$name == "China" ~ 710,
          TRUE ~ .data$gw
        )
      ) %>%
      tidyr::drop_na(.data$gw) %>%
      dplyr::arrange(gw, year) %>%
      dplyr::group_by(gw) %>%
      dplyr::mutate(gdp_growth = ((gdp_pcap - dplyr::lag(gdp_pcap)) / dplyr::lag(gdp_pcap) * 100),
             log_gdp_pcap = log(gdp_pcap + 1)) %>%
      dplyr::ungroup() %>%
      dplyr::select(gw, year, gdp_pcap, log_gdp_pcap, gdp_growth)
  }

  return(gdp_data)
}
