test_that("build_states_panel returns a data frame", {
  panel <- build_states_panel(1990, 1995)
  expect_s3_class(panel, "data.frame")
  expect_true(all(c("cow", "year", "country") %in% names(panel)))
})

test_that("build_states_panel returns unique cow-year observations", {
  panel <- build_states_panel(1990, 1995)
  dupes <- panel |>
    dplyr::count(cow, year) |>
    dplyr::filter(n > 1)
  expect_equal(nrow(dupes), 0)
})

test_that("build_states_panel validates years", {
  expect_error(
    build_states_panel(2000, 1990),
    "`start_year` must be less than or equal to `end_year`"
  )
})
