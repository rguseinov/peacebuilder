# peacebuilder

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`peacebuilder` is an R package for building country-year panel datasets in cross-national peace science research. It provides a flexible workflow for creating state panels and enriching them with socioeconomic, political, and conflict indicators — using either Correlates of War (COW) or Gleditsch-Ward (GW) country coding schemes.

## Installation

```r
# install.packages("remotes")
remotes::install_github("rguseinov/peacebuilder")
```

> **Note:** `load_vdem_data()` and `add_vdem()` require the `vdemdata` package, which is not on CRAN:
> ```r
> remotes::install_github("vdeminstitute/vdemdata")
> ```

## Two workflows, your choice

`peacebuilder` supports two ways of working — use them separately or together.

### Pipeline workflow

Build a complete panel in one chain. Functions automatically detect the coding system and year range from the panel:

```r
library(peacebuilder)
library(dplyr)

panel <- build_states_panel(
    start_year = 1990,
    end_year   = 2015,
    coding_system = "cow"
  ) |>
  add_gdp() |>
  add_vdem(vars = c("v2x_polyarchy", "v2x_libdem", "v2x_rule")) |>
  add_conflict(dataset = "navco2.1") |>
  add_leader_data(dataset = "archigos")
```

### Standalone workflow

Load each dataset independently for inspection or custom merging:

```r
panel     <- build_states_panel(1990, 2015, coding_system = "cow")
gdp_data  <- load_gdp_data(1990, 2015, coding_system = "cow")
vdem_data <- load_vdem_data(
  vars          = c("v2x_polyarchy", "v2x_libdem"),
  start_year    = 1990,
  end_year      = 2015,
  coding_system = "cow"
)
conflicts <- conflict_data(1990, 2015, dataset = "navco2.1", coding_system = "cow")

# Join on your own terms
panel <- panel |>
  left_join(gdp_data,  by = c("cow", "year")) |>
  left_join(vdem_data, by = c("cow", "year")) |>
  left_join(
    conflicts |>
      group_by(cow, year) |>
      summarise(onset = max(nvc2.1_ONSET), .groups = "drop"),
    by = c("cow", "year")
  )
```

## Functions

### `build_states_panel()`

Creates a state-year panel with optional filters:

```r
panel <- build_states_panel(
  start_year        = 1946,
  end_year          = 2019,
  coding_system     = "cow",   # or "gw"
  exclude_microstates = TRUE,
  exclude_non_un    = TRUE,
  exclude_islands   = FALSE
)
```

### `load_gdp_data()` / `add_gdp()`

Gapminder GDP per capita data. Returns `gdp_pcap`, `log_gdp_pcap`, and `gdp_growth`.

```r
# Standalone
gdp <- load_gdp_data(start_year = 1990, end_year = 2015, coding_system = "cow")

# Pipeline
panel |> add_gdp()
```

### `load_vdem_data()` / `add_vdem()`

V-Dem indicators. A default set of democracy, civil society, civil liberties, and rule-of-law variables is loaded when `vars = NULL`.

```r
# Standalone
vdem <- load_vdem_data(
  vars          = c("v2x_polyarchy", "v2x_libdem"),
  start_year    = 1990,
  end_year      = 2015,
  coding_system = "cow"
)

# Pipeline
panel |> add_vdem(vars = c("v2x_polyarchy", "v2x_libdem"))
```

### `conflict_data()` / `add_conflict()`

Loads one of four conflict and revolutionary episode datasets. Supports both COW and GW coding.

| Dataset | Source | Coverage | Output level | Prefix |
|---|---|---|---|---|
| `"navco1.3"` | NAVCO 1.3 | 1900–2006 | Campaign onset | `nvc1.3_` |
| `"navco2.1"` | NAVCO 2.1 | 1945–2013 | Campaign-year | `nvc2.1_` |
| `"beissinger"` | Beissinger Revolutionary Episodes | 1900–2014 | Episode onset | `beissinger_` |
| `"csra"` | HSE CSRA Revolutions Dataset | 1900–2022 | Episode onset | `csra_` |
| `"scad"` | The Social Conflict Analysis Database (SCAD, Africa + Latin America) | 1990–2018 | Country-year onset | `scad_` |
| `"ucdp_prio"` | UCDP/PRIO Armed Conflict v26.1 | 1946–2025 | Country-year onset | `ucdp_prio_` |
| `"ucdp_vpp"` | UCDP Violent Political Protest v26.1 | 1989–2025 | Country-year onset | `ucdp_vpp_` |
| `"mm"` | Mass Mobilization Project v4 | 1990–2020 | Country-year onset | `mm_` |
| `"mmad"` | Mass Mobilization in Autocracies v5 | 2003–2022 | Country-year onset | `mmad_` |

New datasets (`scad`, `ucdp_prio`, `ucdp_vpp`, `mm`, `mmad`) are pre-aggregated to country-year onsets inside `conflict_data()`. Legacy campaign datasets return one row per active campaign; `add_conflict()` collapses these with `max()` by default.

```r
# Standalone
navco   <- conflict_data(1990, 2015, dataset = "navco2.1",  coding_system = "cow")
scad    <- conflict_data(1995, 2015, dataset = "scad",      coding_system = "cow")
ucdp    <- conflict_data(1990, 2020, dataset = "ucdp_prio", coding_system = "gw")
mm_data <- conflict_data(1995, 2015, dataset = "mm",        coding_system = "cow")
mmad    <- conflict_data(2005, 2020, dataset = "mmad",      coding_system = "cow")

# Pipeline — all datasets work with add_conflict()
panel |> add_conflict(dataset = "navco2.1")
panel |> add_conflict(dataset = "ucdp_prio")
panel |> add_conflict(dataset = "mmad")

# Raw join without aggregation (for legacy campaign datasets)
panel |> add_conflict(dataset = "navco1.3", aggregate = FALSE)
```

> **Note:** `ucdp_vpp` requires the `readxl` package: `install.packages("readxl")`

### `load_leader_data()` / `add_leader_data()`

Country-year leader data from two sources. Each row contains the leader who held power at the end of the year; in transition years the latest-starting leader is kept.

| Dataset | Source | Coverage | Key variables |
|---|---|---|---|
| `"archigos"` | Archigos 4.1 | 1875–2015 | `entry`, `exit`, `irregular_entry`, `irregular_exit`, `female_leader`, `yrborn`, `posttenurefate`, `leader_tenure` |
| `"reign"` | REIGN Leader List | 1921–2021 | `female_leader`, `military_bg`, `birthyear`, `leader_tenure` |

```r
# Standalone
arch  <- load_leader_data(1990, 2015, dataset = "archigos", coding_system = "cow")
reign <- load_leader_data(1990, 2015, dataset = "reign",    coding_system = "gw")

# Pipeline
panel |> add_leader_data(dataset = "archigos")
panel |> add_leader_data(dataset = "reign")
```

## Citation

If you use `peacebuilder` in your research, please cite:

> Guseinov, R. (2026). *peacebuilder: Build Peace Science Data Panels*. R package version 0.0.2. https://github.com/rguseinov/peacebuilder

Please also cite the original data sources (V-Dem, Gapminder, NAVCO, Beissinger, CSRA) as appropriate.
