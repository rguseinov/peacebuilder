usethis::use_git()

usethis::use_mit_license("Ruslan Guseinov")

usethis::use_r("utils")
usethis::use_r("globals")
usethis::use_r("pipe")
usethis::use_package("readr")
devtools::document()
devtools::load_all()


#### build states panel ####
panel_df <- build_states_panel_(start_year = 1945,
                               end_year = 2019,
                               exclude_microstates = TRUE,
                               exclude_non_un = TRUE,
                               exclude_islands = TRUE,
                               )

panel_df2 <- build_states_panel(start_year = 1900,
                                  end_year = 2024,
                                  exclude_microstates = TRUE,
                                  exclude_non_un = TRUE,
                                  exclude_islands = TRUE, coding_system = 'cow')

dem_data <- load_vdem_data(coding_system = 'gw')
gdp_data <- load_gdp_data(coding_system = 'gw')


dplyr::glimpse(panel_df)
#no duplicates
panel_df |>
  dplyr::count(cow, year) |>
  dplyr::filter(n > 1)

#test the build states panel function
usethis::use_testthat()
usethis::use_test("build_states_panel")
devtools::test()

#full package check
devtools::check()

#Add GDP gapminder
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
system.file("extdata", "gapminder_gdp_v32.csv", package = "peacebuilder")

#Check the package
devtools::document()

devtools::check()

usethis::create_github_token()

usethis::use_git()
gitcreds::gitcreds_set()
usethis::use_github()
