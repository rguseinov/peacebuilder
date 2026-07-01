#' Load leader data
#'
#' @description
#' Loads country-year leader data from one of two datasets included with
#' the package. Each row in the output represents one country-year, with the
#' leader who held power at the end of the year. When multiple leaders served
#' in the same year (i.e., a leadership transition occurred), the leader with
#' the latest start date is retained.
#'
#' @param start_year Integer. First year to include.
#' @param end_year Integer. Last year to include.
#' @param dataset Character. Dataset to load:
#'   \describe{
#'     \item{`"archigos"`}{Archigos 4.1 (Goemans, Gleditsch & Chiozza). Leader
#'       spells with entry/exit mode coding. Coverage: 1875–2015.}
#'     \item{`"reign"`}{REIGN Leader List (Bell 2021). Leader spells with
#'       military background coding. Coverage: 1921–2021.}
#'   }
#' @param coding_system Character. Country coding system: `"cow"` or `"gw"`.
#'
#' @return A country-year data frame. Columns differ by dataset:
#'
#'   **archigos:** `cow`/`gw`, `year`, `leader`, `entry`, `exit`,
#'   `irregular_entry`, `irregular_exit`, `female_leader`, `yrborn`,
#'   `posttenurefate`, `leader_tenure`.
#'
#'   **reign:** `cow`/`gw`, `year`, `leader`, `female_leader`,
#'   `military_bg`, `birthyear`, `leader_tenure`.
#'
#' @source
#' **Archigos:** Goemans, H.E., Gleditsch, K.S., & Chiozza, G. (2009).
#' Introducing Archigos: A Dataset of Political Leaders.
#' *Journal of Peace Research*, 46(2), 269–283.
#' \url{http://ksgleditsch.com/archigos.html}
#'
#' **REIGN:** Bell, C. (2021). REIGN: Rulers, Elections, and Irregular
#' Governance Dataset. One Earth Future Foundation.
#' \url{https://oefdatascience.github.io/REIGN.github.io/}
#'
#' @examples
#' arch <- load_leader_data(1990, 2010, dataset = "archigos", coding_system = "cow")
#'
#' reign <- load_leader_data(1990, 2010, dataset = "reign", coding_system = "gw")
#'
#' @export
load_leader_data <- function(
    start_year    = 1945,
    end_year      = 2015,
    dataset       = c("archigos", "reign"),
    coding_system = c("cow", "gw")
) {
  check_year_range(start_year, end_year)
  dataset       <- match.arg(dataset)
  coding_system <- match.arg(coding_system)

  if (dataset == "archigos") {
    path <- system.file("extdata", "archigos_4.1.txt", package = "peacebuilder")
    if (path == "") stop("Could not find `archigos_4.1.txt` in package extdata.", call. = FALSE)

    raw <- readr::read_tsv(
      path,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "latin1")
    )

    data <- raw %>%
      dplyr::mutate(
        startdate       = as.Date(.data$startdate),
        enddate         = as.Date(.data$enddate),
        start_yr        = as.integer(format(.data$startdate, "%Y")),
        end_yr          = as.integer(format(.data$enddate,   "%Y")),
        irregular_entry = as.integer(.data$entry %in% c("Irregular", "Foreign Imposition")),
        irregular_exit  = as.integer(.data$exit  %in% c("Irregular", "Foreign", "Suicide")),
        female_leader   = as.integer(.data$gender == "F"),
        ccode           = as.integer(.data$ccode)
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(year = list(seq(.data$start_yr, .data$end_yr))) %>%
      tidyr::unnest(.data$year) %>%
      dplyr::ungroup() %>%
      # Multiple leaders per year → keep the one with the latest start date
      dplyr::group_by(.data$ccode, .data$year) %>%
      dplyr::slice_max(.data$startdate, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(leader_tenure = as.integer(.data$year - .data$start_yr + 1L)) %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::select(
        cow             = .data$ccode,
        .data$year,
        .data$leader,
        .data$entry,
        .data$exit,
        .data$irregular_entry,
        .data$irregular_exit,
        .data$female_leader,
        .data$yrborn,
        .data$posttenurefate,
        .data$leader_tenure
      )

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(
          gw = suppressWarnings(countrycode::countrycode(.data$cow, "cown", "gwn"))
        ) %>%
        tidyr::drop_na(.data$gw) %>%
        dplyr::select(-.data$cow) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    }
  }

  if (dataset == "reign") {
    path <- system.file("extdata", "reign_leader_list.csv", package = "peacebuilder")
    if (path == "") stop("Could not find `reign_leader_list.csv` in package extdata.", call. = FALSE)

    # suppressMessages: readr renames empty trailing column headers to ...15/...16
    raw <- suppressMessages(
      readr::read_csv(path, show_col_types = FALSE,
                      locale = readr::locale(encoding = "latin1"))
    ) %>%
      dplyr::select(where(~ !all(is.na(.))))

    data <- raw %>%
      dplyr::filter(!is.na(.data$syear), !is.na(.data$eyear)) %>%
      dplyr::mutate(
        syear         = as.integer(.data$syear),
        eyear         = as.integer(.data$eyear),
        smonth        = as.integer(.data$smonth),
        gender_int    = suppressWarnings(as.integer(.data$gender)),
        # In REIGN: 1 = male, 0 = female, -99 = unknown
        female_leader = dplyr::case_when(
          .data$gender_int == 0L  ~ 1L,
          .data$gender_int == 1L  ~ 0L,
          TRUE                    ~ NA_integer_
        ),
        military_bg   = as.integer(.data$militarycareer),
        ccode         = as.integer(.data$ccode),
        # Sort key for year-end leader selection
        start_ym      = .data$syear * 12L + .data$smonth
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(year = list(seq(.data$syear, .data$eyear))) %>%
      tidyr::unnest(.data$year) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$ccode, .data$year) %>%
      dplyr::slice_max(.data$start_ym, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(leader_tenure = as.integer(.data$year - .data$syear + 1L)) %>%
      dplyr::filter(.data$year >= start_year & .data$year <= end_year) %>%
      dplyr::select(
        cow           = .data$ccode,
        .data$year,
        .data$leader,
        .data$female_leader,
        .data$military_bg,
        birthyear     = .data$birthyear,
        .data$leader_tenure
      )

    if (coding_system == "gw") {
      data <- data %>%
        dplyr::mutate(
          gw = suppressWarnings(countrycode::countrycode(.data$cow, "cown", "gwn"))
        ) %>%
        tidyr::drop_na(.data$gw) %>%
        dplyr::select(-.data$cow) %>%
        dplyr::select(.data$gw, .data$year, dplyr::everything())
    }
  }

  return(data)
}
