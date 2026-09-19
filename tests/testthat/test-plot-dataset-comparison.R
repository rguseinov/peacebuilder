test_that("plot_regional_coverage returns a ggplot and validates its inputs", {
  skip_if_not_installed("ggplot2")

  p <- plot_regional_coverage(c("navco1.3", "csra"), 2000, 2010)
  expect_s3_class(p, "ggplot")

  p_count <- plot_regional_coverage(c("navco1.3", "csra"), 2000, 2010, metric = "count")
  expect_s3_class(p_count, "ggplot")

  expect_error(plot_regional_coverage("navco1.3", 2010, 2000), "less than or equal")
})

test_that("plot_regional_coverage shares sum to 100 within each dataset", {
  skip_if_not_installed("ggplot2")

  p <- plot_regional_coverage(c("navco1.3", "beissinger"), 1990, 2010)
  totals <- p$data |>
    dplyr::group_by(dataset) |>
    dplyr::summarise(total = sum(value), .groups = "drop")

  expect_equal(totals$total, rep(100, nrow(totals)), tolerance = 1e-6)
})

test_that("plot_temporal_coverage returns a ggplot, global and faceted by region", {
  skip_if_not_installed("ggplot2")

  p_global <- plot_temporal_coverage(c("navco1.3", "csra"), 2000, 2010)
  expect_s3_class(p_global, "ggplot")

  p_region <- plot_temporal_coverage(c("navco1.3", "csra"), 2000, 2010, by_region = TRUE)
  expect_s3_class(p_region, "ggplot")
  expect_true("region" %in% names(p_region$data))

  expect_error(
    plot_temporal_coverage("navco1.3", 2000, 2010, period_length = 0),
    "positive whole number"
  )
})

test_that("plot_temporal_coverage bins years into the requested period width", {
  skip_if_not_installed("ggplot2")

  p <- plot_temporal_coverage("navco1.3", 2000, 2009, period_length = 5)
  expect_setequal(as.character(unique(p$data$period)), c("2000-2004", "2005-2009"))
})

test_that("fetch_campaign_events collapses navco2.1 to one row per campaign onset", {
  events <- fetch_campaign_events(c("navco1.3", "navco2.1"), 1950, 2013, "cow")

  navco2.1_n <- sum(events$dataset == "navco2.1")
  navco1.3_n <- sum(events$dataset == "navco1.3")

  # Raw navco2.1 campaign-year data has thousands of rows; onset-collapsed
  # counts should be the same order of magnitude as navco1.3's onset counts,
  # not inflated by average campaign duration.
  expect_lt(navco2.1_n, navco1.3_n * 2)

  raw <- conflict_data(1950, 2013, dataset = "navco2.1", coding_system = "cow")
  expect_gt(nrow(raw), navco2.1_n * 2)
})

test_that("fetch_campaign_events keeps distinct navco2.1 campaigns that share a name", {
  # Grouping onset detection by the free-text campaign name (instead of the
  # dataset's numeric campaign id / its own nvc2.1_ONSET flag) would wrongly
  # collapse distinct campaigns that happen to share a name into one row.
  # "Myanmar Regime Change Campaign" is one such case (3 distinct ids).
  raw <- conflict_data(1945, 2013, dataset = "navco2.1", coding_system = "cow")
  myanmar <- raw[raw$nvc2.1_camp_name == "Myanmar Regime Change Campaign", ]
  expect_gt(length(unique(myanmar$year[myanmar$nvc2.1_ONSET == 1])), 1)

  events <- fetch_campaign_events("navco2.1", 1945, 2013, "cow")
  expect_equal(sum(events$dataset == "navco2.1"), sum(raw$nvc2.1_ONSET == 1))
})

test_that("fetch_campaign_events collapses the region field to 7 clean categories", {
  # countrycode::countrycode()'s "region" destination isn't a clean 7-value
  # classification: a few pre-1990 historical entities (Yemen Arab
  # Republic, Yemen People's Republic, the United Arab Republic) still
  # carry the World Bank's older "Middle East & North Africa" label while
  # every other MENA country carries the newer, longer "Middle East,
  # North Africa, Afghanistan & Pakistan" label. Campaign data covering
  # the 1950s-80s does include those historical codes, so without
  # recoding, one region would silently split into two bars. The shorter
  # label is kept as canonical (better for plot axis text).
  events <- fetch_campaign_events(
    c("navco1.3", "navco2.1", "beissinger", "csra", "mec"), 1950, 2013, "cow"
  )
  expect_length(unique(events$region), 7L)
  expect_true("Middle East & North Africa" %in% events$region)
  expect_false(
    "Middle East, North Africa, Afghanistan & Pakistan" %in% events$region
  )
})
