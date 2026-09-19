#' Fetch one row per campaign/episode onset from several conflict datasets
#'
#' @param datasets Character vector of `conflict_data()` dataset keys.
#' @param start_year,end_year Integer year bounds.
#' @param coding_system `"cow"` or `"gw"`.
#'
#' @return A data frame with `dataset`, `unit` (the coding-system code),
#'   `year`, and `region` (World Bank 7-region classification, via
#'   `countrycode::countrycode()`).
#'
#' @details
#' `"navco1.3"`, `"beissinger"`, `"csra"`, and `"mec"` are already one row
#' per campaign/episode. `"navco2.1"` is not: `conflict_data()` returns one
#' row per campaign-*year*, so a single long-running campaign contributes
#' many rows. Comparing that directly against onset-coded datasets would
#' overstate NAVCO 2.1's event counts by roughly the average campaign
#' duration. To keep the comparison on the same onset-level footing as the
#' source article, `"navco2.1"` is filtered here to `nvc2.1_ONSET == 1`,
#' `conflict_data()`'s own per-campaign onset flag (keyed on the numeric
#' campaign `id`, not the free-text campaign name, which is not unique --
#' e.g. three distinct "Myanmar Regime Change Campaign" entries share that
#' name). That flag is computed over the dataset's full native coverage
#' before `start_year`/`end_year` filtering, so campaigns already under way
#' at `start_year` are not mistaken for new onsets.
#'
#' `countrycode::countrycode()`'s `"region"` destination is not a clean
#' 7-category classification: a handful of pre-1990 historical entities
#' (Yemen Arab Republic, Yemen People's Republic, the United Arab
#' Republic) still carry the World Bank's older "Middle East & North
#' Africa" label, while every other country in that region carries the
#' newer, longer "Middle East, North Africa, Afghanistan & Pakistan"
#' label -- these historical COW/GW codes do appear in campaign data
#' covering the 1950s-80s, so left as-is they'd split one region into two
#' bars. The newer label is recoded to the shorter, older one here (kept
#' as the canonical form for display) so the region breakdown stays at
#' exactly 7 categories.
#'
#' @keywords internal
fetch_campaign_events <- function(datasets, start_year, end_year, coding_system) {
  region_dest <- if (coding_system == "cow") "cown" else "gwn"

  rows <- lapply(datasets, function(d) {
    x <- conflict_data(
      start_year = start_year, end_year = end_year,
      dataset = d, coding_system = coding_system
    )
    if (d == "navco2.1") {
      x <- x[x$nvc2.1_ONSET == 1L, ]
    }
    data.frame(
      dataset = d,
      unit    = x[[coding_system]],
      year    = x$year,
      stringsAsFactors = FALSE
    )
  })

  events <- dplyr::bind_rows(rows)
  events$region <- suppressWarnings(
    countrycode::countrycode(events$unit, region_dest, "region")
  )
  events$region[
    events$region == "Middle East, North Africa, Afghanistan & Pakistan"
  ] <- "Middle East & North Africa"
  tidyr::drop_na(events, "region")
}


#' Compare regional coverage across conflict/campaign datasets
#'
#' @description
#' Draws a grouped bar chart of the regional distribution of events (World
#' Bank 7-region classification) for any set of `conflict_data()` campaign
#' or episode datasets, in the style of Guseinov, Ustyuzhanin, and Korotayev
#' (2026), Figures 1-3. Comparing several datasets this way shows whether
#' they cover similar parts of the world, or whether one systematically
#' over/under-represents a region relative to the others.
#'
#' @param datasets Character vector of `conflict_data()` dataset keys to
#'   compare, e.g. `c("navco1.3", "beissinger", "csra")`.
#' @param start_year,end_year Integer. Year range to compare (applied to
#'   every dataset).
#' @param coding_system `"cow"` or `"gw"`.
#' @param metric `"share"` (default; each dataset's bars sum to 100%, so
#'   datasets of very different size remain comparable) or `"count"` (raw
#'   number of events).
#'
#' @return A `ggplot` object. Requires the `ggplot2` package.
#'
#' @source
#' Guseinov, R., Ustyuzhanin, V., & Korotayev, A. (2026). Talking about the
#' \[same\] revolution? A comparative analysis of main datasets of
#' revolutionary events. *Defence and Peace Economics*, 1--31.
#' \doi{10.1080/10242694.2026.2704178}
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_regional_coverage(
#'     c("navco1.3", "navco2.1", "beissinger", "csra"),
#'     start_year = 1950, end_year = 2013
#'   )
#' }
#'
#' @export
plot_regional_coverage <- function(
    datasets,
    start_year,
    end_year,
    coding_system = c("cow", "gw"),
    metric        = c("share", "count")
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plot_regional_coverage(). ",
      "Install it with: install.packages('ggplot2')",
      call. = FALSE
    )
  }
  check_year_range(start_year, end_year)
  coding_system <- match.arg(coding_system)
  metric        <- match.arg(metric)

  events <- fetch_campaign_events(datasets, start_year, end_year, coding_system)

  counts <- events |>
    dplyr::count(.data$dataset, .data$region, name = "n")

  if (metric == "share") {
    counts <- counts |>
      dplyr::group_by(.data$dataset) |>
      dplyr::mutate(value = .data$n / sum(.data$n) * 100) |>
      dplyr::ungroup()
    y_lab <- "Share of events (%)"
  } else {
    counts$value <- counts$n
    y_lab <- "Number of events"
  }

  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = .data$region, y = .data$value, fill = .data$dataset)
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_discrete(labels = wrap_labels) +
    ggplot2::labs(x = "Region", y = y_lab, fill = "Dataset") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5))
}


#' Wrap long axis labels onto multiple lines
#'
#' @param labels Character vector of labels.
#' @param width Target line width in characters. Default `12`.
#'
#' @return `labels`, each wrapped at word boundaries and joined with `\n`.
#'
#' @keywords internal
wrap_labels <- function(labels, width = 12) {
  vapply(
    labels,
    function(x) paste(strwrap(x, width = width), collapse = "\n"),
    character(1)
  )
}


#' Compare temporal coverage across conflict/campaign datasets
#'
#' @description
#' Draws a line chart of the number of events per time period for any set
#' of `conflict_data()` campaign or episode datasets, in the style of
#' Guseinov, Ustyuzhanin, and Korotayev (2026), Figures 4-5. Comparing
#' several datasets this way shows whether they track the same broad
#' historical trends, and where one dataset picks up a surge or lull that
#' the others miss.
#'
#' @param datasets Character vector of `conflict_data()` dataset keys to
#'   compare, e.g. `c("navco1.3", "beissinger", "csra")`.
#' @param start_year,end_year Integer. Year range to compare (applied to
#'   every dataset).
#' @param coding_system `"cow"` or `"gw"`.
#' @param period_length Integer. Width in years of each time bin (default
#'   `5`, matching the source article).
#' @param by_region Logical. If `TRUE`, facet by World Bank region (one
#'   panel per region, as in Figure 4). If `FALSE` (default), a single
#'   global panel (as in Figure 5).
#'
#' @return A `ggplot` object. Requires the `ggplot2` package.
#'
#' @source
#' Guseinov, R., Ustyuzhanin, V., & Korotayev, A. (2026). Talking about the
#' \[same\] revolution? A comparative analysis of main datasets of
#' revolutionary events. *Defence and Peace Economics*, 1--31.
#' \doi{10.1080/10242694.2026.2704178}
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_temporal_coverage(
#'     c("navco1.3", "navco2.1", "beissinger", "csra"),
#'     start_year = 1950, end_year = 2013
#'   )
#' }
#'
#' @export
plot_temporal_coverage <- function(
    datasets,
    start_year,
    end_year,
    coding_system  = c("cow", "gw"),
    period_length  = 5,
    by_region      = FALSE
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plot_temporal_coverage(). ",
      "Install it with: install.packages('ggplot2')",
      call. = FALSE
    )
  }
  check_year_range(start_year, end_year)
  coding_system <- match.arg(coding_system)
  check_flag(by_region, "by_region")
  if (!is.numeric(period_length) || length(period_length) != 1L ||
        is.na(period_length) || period_length %% 1 != 0 || period_length < 1) {
    stop("`period_length` must be a single positive whole number.", call. = FALSE)
  }
  period_length <- as.integer(period_length)

  events <- fetch_campaign_events(datasets, start_year, end_year, coding_system)

  period_start <- start_year +
    (events$year - start_year) %/% period_length * period_length
  period_end   <- pmin(period_start + period_length - 1L, end_year)
  events$period <- factor(
    paste(period_start, period_end, sep = "-"),
    levels = unique(paste(period_start, period_end, sep = "-")[order(period_start)])
  )

  group_vars <- if (by_region) {
    c("dataset", "region", "period")
  } else {
    c("dataset", "period")
  }
  counts <- events |>
    dplyr::count(dplyr::across(dplyr::all_of(group_vars)), name = "n")

  p <- ggplot2::ggplot(
    counts,
    ggplot2::aes(
      x = .data$period, y = .data$n, color = .data$dataset,
      group = .data$dataset
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = paste0(period_length, "-year period"), y = "Number of events",
      color = "Dataset"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

  if (by_region) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$region))
  }

  p
}
