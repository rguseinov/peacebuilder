#' Add GDP per capita data to a state panel
#'
#' @description
#' Joins Gapminder GDP data onto an existing state panel. The coding system
#' (`"cow"` or `"gw"`) and year range are detected automatically from the
#' panel. Added columns: `gdp_pcap`, `log_gdp_pcap`, `gdp_growth`.
#'
#' @param panel A data frame produced by `build_states_panel()`, containing
#'   a `cow` or `gw` column and a `year` column.
#'
#' @return The input panel with GDP columns added via left join.
#'
#' @examples
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_gdp()
#'
#' @export
add_gdp <- function(panel) {
  coding_system <- detect_coding_system(panel)
  yr <- range(panel$year, na.rm = TRUE)

  gdp <- load_gdp_data(
    start_year    = yr[1],
    end_year      = yr[2],
    coding_system = coding_system
  )

  dplyr::left_join(panel, gdp, by = c(coding_system, "year"))
}


#' Add V-Dem indicators to a state panel
#'
#' @description
#' Joins V-Dem country-year data onto an existing state panel. The coding
#' system and year range are detected automatically from the panel.
#'
#' Requires the `vdemdata` package:
#' `remotes::install_github("vdeminstitute/vdemdata")`
#'
#' @param panel A data frame produced by `build_states_panel()`.
#' @param vars Character vector of V-Dem variable names. If `NULL`, a default
#'   set of democracy, civil liberties, civil society, and rule-of-law
#'   indicators is used. See `load_vdem_data()` for the full default list.
#'
#' @return The input panel with V-Dem columns added via left join.
#'
#' @examples
#' \dontrun{
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_vdem(vars = c("v2x_polyarchy", "v2x_libdem", "v2x_rule"))
#' }
#'
#' @export
add_vdem <- function(panel, vars = NULL) {
  coding_system <- detect_coding_system(panel)
  yr <- range(panel$year, na.rm = TRUE)

  vdem <- load_vdem_data(
    vars          = vars,
    start_year    = yr[1],
    end_year      = yr[2],
    coding_system = coding_system
  )

  dplyr::left_join(panel, vdem, by = c(coding_system, "year"))
}


#' Add conflict data to a state panel
#'
#' @description
#' Joins a conflict or revolutionary episode dataset onto an existing
#' state panel at the country-year level.
#'
#' Because conflict datasets are campaign-level or campaign-year-level,
#' multiple records can exist for the same country-year. By default
#' (`aggregate = TRUE`), the function aggregates to country-year by taking
#' the maximum of all numeric columns — which yields a binary onset indicator
#' and flags any active campaign for that country-year. A count of campaigns
#' (`n_campaigns`) is also added.
#'
#' Set `aggregate = FALSE` to skip aggregation and get the raw join. This will
#' produce duplicate rows when multiple campaigns overlap in the same
#' country-year, so use it only if you plan to handle that yourself.
#'
#' @param panel A data frame produced by `build_states_panel()`.
#' @param dataset Dataset to load. One of `"navco1.3"`, `"navco2.1"`,
#'   `"beissinger"`, or `"csra"`.
#' @param aggregate Logical. If `TRUE` (default), aggregates to country-year
#'   before joining. If `FALSE`, performs a raw left join (may produce
#'   duplicate rows).
#'
#' @return The input panel with conflict columns added via left join.
#'
#' @examples
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_conflict(dataset = "navco2.1")
#'
#' # Raw join — researcher handles aggregation manually
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_conflict(dataset = "navco1.3", aggregate = FALSE)
#'
#' @export
add_conflict <- function(panel, dataset, aggregate = TRUE) {
  coding_system <- detect_coding_system(panel)
  yr <- range(panel$year, na.rm = TRUE)

  conflicts <- conflict_data(
    start_year    = yr[1],
    end_year      = yr[2],
    dataset       = dataset,
    coding_system = coding_system
  )

  id_col <- coding_system

  if (aggregate) {
    has_dupes <- conflicts |>
      dplyr::count(.data[[id_col]], .data$year) |>
      dplyr::filter(.data$n > 1) |>
      nrow() > 0

    if (has_dupes) {
      message(
        "Multiple campaigns per country-year detected in '", dataset, "'. ",
        "Aggregating to country-year using max() for numeric columns. ",
        "Use `aggregate = FALSE` or `conflict_data()` for campaign-level data."
      )
    }

    conflicts <- conflicts |>
      dplyr::group_by(.data[[id_col]], .data$year) |>
      dplyr::mutate(n_campaigns = dplyr::n()) |>
      dplyr::summarise(
        dplyr::across(where(is.numeric), \(x) max(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  dplyr::left_join(panel, conflicts, by = c(id_col, "year"))
}
