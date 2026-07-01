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


#' Add conflict or protest data to a state panel
#'
#' @description
#' Joins a conflict, protest, or revolutionary episode dataset onto an existing
#' state panel at the country-year level.
#'
#' **New datasets** (`"scad"`, `"ucdp_prio"`, `"ucdp_vpp"`, `"mm"`, `"mmad"`)
#' are already at the country-year level when loaded, so the `aggregate`
#' argument has no effect for them.
#'
#' **Legacy campaign datasets** (`"navco1.3"`, `"navco2.1"`, `"beissinger"`,
#' `"csra"`) can have multiple rows per country-year when campaigns overlap.
#' With `aggregate = TRUE` (the default) these are collapsed to country-year
#' by taking the maximum of all numeric columns, and an `n_campaigns` count
#' column is added. Set `aggregate = FALSE` to keep campaign-level rows (may
#' produce duplicates).
#'
#' @param panel A data frame produced by `build_states_panel()`.
#' @param dataset Dataset to load. One of:
#'   \describe{
#'     \item{`"navco1.3"`}{NAVCO 1.3 nonviolent campaign onsets (1900-2006).}
#'     \item{`"navco2.1"`}{NAVCO 2.1 campaign-years (1945-2013).}
#'     \item{`"beissinger"`}{Beissinger revolutionary episode onsets (1900-2014).}
#'     \item{`"csra"`}{HSE CSRA revolutionary episodes (1900-2022).}
#'     \item{`"scad"`}{SCAD 2018 social conflict events, Africa and Latin America
#'       (1990-2018). Prefix: `scad_`.}
#'     \item{`"ucdp_prio"`}{UCDP/PRIO Armed Conflict Dataset v26.1 (1946-2025).
#'       Prefix: `ucdp_prio_`.}
#'     \item{`"ucdp_vpp"`}{UCDP Violent Political Protest v26.1 (1989-2025).
#'       Prefix: `ucdp_vpp_`. Requires the `readxl` package.}
#'     \item{`"mm"`}{Mass Mobilization Project v4 (1990-2019). Prefix: `mm_`.}
#'     \item{`"mmad"`}{Mass Mobilization in Autocracies Database (2003-2022).
#'       Prefix: `mmad_`.}
#'   }
#' @param aggregate Logical. If `TRUE` (default), aggregates to country-year
#'   before joining (relevant for legacy campaign datasets only). If `FALSE`,
#'   performs a raw left join.
#'
#' @return The input panel with conflict/protest columns added via left join.
#'
#' @examples
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_conflict(dataset = "navco2.1")
#'
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_conflict(dataset = "ucdp_prio")
#'
#' panel <- build_states_panel(2005, 2020, coding_system = "cow") |>
#'   add_conflict(dataset = "mmad")
#'
#' # Raw join for legacy datasets — researcher handles aggregation manually
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


#' Add leader data to a state panel
#'
#' @description
#' Joins country-year leader data onto an existing state panel. The coding
#' system and year range are detected automatically from the panel.
#'
#' Each country-year is assigned the leader who held power at the end of that
#' year (i.e., the leader with the latest start date when multiple leaders
#' served in the same year).
#'
#' @param panel A data frame produced by `build_states_panel()`.
#' @param dataset Dataset to use: `"archigos"` (default) or `"reign"`.
#'   See `load_leader_data()` for details.
#'
#' @return The input panel with leader columns added via left join.
#'
#' @examples
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_leader_data(dataset = "archigos")
#'
#' panel <- build_states_panel(1990, 2010, coding_system = "cow") |>
#'   add_leader_data(dataset = "reign")
#'
#' @export
add_leader_data <- function(panel, dataset = c("archigos", "reign")) {
  dataset       <- match.arg(dataset)
  coding_system <- detect_coding_system(panel)
  yr            <- range(panel$year, na.rm = TRUE)

  leaders <- load_leader_data(
    start_year    = yr[1],
    end_year      = yr[2],
    dataset       = dataset,
    coding_system = coding_system
  )

  dplyr::left_join(panel, leaders, by = c(coding_system, "year"))
}
