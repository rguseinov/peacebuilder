# peacebuilder

Author: Ruslan Guseinov (rigusseinov@gmail.com)

`peacebuilder` is an R package for constructing panel datasets for political scientists and peace science researchers.

The package provides tools for creating state-year panels and adding selected socio-economic and political indicators. It is designed for workflows that use country-year data with either COW or Gleditsch-Ward country coding schemes.

## Installation

You can install the development version of `peacebuilder` from GitHub:

```r

install.packages("remotes")

remotes::install_github("rguseinov/peacebuilder")
```
## Functions

### `build_states_panel()`

Creates a state-year panel of countries and corresponding country codes. The function supports two country coding systems: Correlates of War (`"cow"`) and Gleditsch-Ward (`"gw"`). A usage example:

```r
panel <- build_states_panel(
  start_year = 1946,
  end_year = 2019,
  coding_system = "cow",
  exclude_microstates = TRUE,
  exclude_non_un = TRUE,
  exclude_islands = FALSE
)
```

### `load_vdem_data()`

Loads V-Dem panel data. The function supports a number of default pre-loaded variables, though users can personally select variables of their interest. The function supports two country coding systems: Correlates of War (`"cow"`) and Gleditsch-Ward (`"gw"`). A usage example:

```r
panel <- build_states_panel(
  start_year = 1946,
  end_year = 2019,
  vars = c('v2x_polyarchy', 'v2x_libdem'),
  coding_system = "cow"
)
```

### `load_gdp_data()`

Loads Gapminder GDP panel data. The function calculates GDP growth rates (%) and logged GDP per capita. The function supports two country coding systems: Correlates of War (`"cow"`) and Gleditsch-Ward (`"gw"`). A usage example:

```r
panel <- load_gdp_data(
  start_year = 1946,
  end_year = 2019,
  coding_system = "cow"
)
```

### `conflict_data()`

Load main domestic conflict data: Nonviolent and Violent Campaigns and Outcomes (NAVCO, versions 1.3 and 2.1), M. Beissinger's Revolutionary Episodes Dataset, HSE University Centre for Stability and Risk Analysis Revolutions Dataset. The function supports two country coding systems: Correlates of War (`"cow"`) and Gleditsch-Ward (`"gw"`). A usage example:

```r
panel <- conflict_data(
  start_year = 1946,
  end_year = 2019,
  dataset = 'navco1.3',
  coding_system = "cow"
)
```
