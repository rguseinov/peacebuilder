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
    dataset = c('navco1.3', 'navco2.1', 'beissinger', 'csra')
){

  check_year_range(start_year, end_year)

  dataset <- match.arg(dataset)

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

  }

  if (dataset == 'navco2.1'){

  }

  if (dataset == 'beissinger'){

  }

  if (dataset == 'csra'){

  }
  return('WORK IN PROGRESS')
}
