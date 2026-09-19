# contentiousR 0.1.0

## Visualization

* `plot_coverage()` gained a `show_labels` argument (default `TRUE`). Set
  it to `FALSE` to drop the per-row country/code labels, which otherwise
  overlap and become illegible once the panel covers more than a few
  dozen states.
* `plot_regional_coverage()`'s region labels are now horizontal and
  word-wrapped instead of angled, and its region field's canonical
  "Middle East" label is the shorter "Middle East & North Africa" form.
  `plot_temporal_coverage()`'s period labels are now vertical instead of
  angled. Both plots no longer set a `ggplot2` title (redundant with a
  caption in a manuscript).
* Added `plot_regional_coverage()` and `plot_temporal_coverage()` to compare
  regional and temporal event coverage across any combination of
  `conflict_data()` campaign/episode datasets, plus a "Comparing
  revolutionary-event datasets" article reproducing the descriptive exercise
  in Guseinov, Ustyuzhanin & Korotayev (2026).
* Added `add_regions()` to join one or more `countrycode` region
  classifications (`region`, `region23`, `un.region.name`,
  `un.regionsub.name`) onto a state panel by `cow`/`gw` code.

## Data

* Added the Major Episodes of Contention (MEC) dataset: 2,734 globally covered
  reformist and maximalist episodes from 1955 through 2018. MEC is available
  through `conflict_data(dataset = "mec")` and `add_conflict(dataset = "mec")`.
  Episode-level rows are preserved unless the existing `aggregate = TRUE`
  policy is requested.

## Correctness

* `fetch_campaign_events()` (used by `plot_regional_coverage()` and
  `plot_temporal_coverage()`) now compares NAVCO 2.1 at the same
  campaign-onset level as the other datasets, using `conflict_data()`'s own
  `nvc2.1_ONSET` flag (keyed on the numeric campaign id) instead of
  re-deriving onsets from the campaign name, which is not unique and could
  silently merge distinct campaigns that happen to share a name.
* `fetch_campaign_events()`'s `region` field now stays at exactly 7
  categories. `countrycode::countrycode()`'s `"region"` destination
  labels a few pre-1990 historical entities (Yemen Arab Republic, Yemen
  People's Republic, the United Arab Republic) with the World Bank's
  older "Middle East & North Africa" string while every other MENA
  country gets the newer "Middle East, North Africa, Afghanistan &
  Pakistan" string; since campaign data covering the 1950s-80s includes
  those historical codes, the region was silently splitting into two
  bars in `plot_regional_coverage()`/`plot_temporal_coverage()`.
* `gdp_growth` (`load_gdp_data()`, `dataset = "gapminder"`) is now `NA`
  unless the preceding row is genuinely one year earlier for the same
  country, the same gap-safety check `add_lag()` uses.
* Removed `add_pop()`. It was an exact duplicate of `add_population()`
  (same body, same arguments, two visually identical Reference entries)
  and every other `add_*()`/`load_*_data()` pair in the package already
  shares one name (`add_vdem()`/`load_vdem_data()`,
  `add_leader_data()`/`load_leader_data()`, etc.) -- `add_population()`
  is the one that matches `load_population_data()`. Use
  `add_population()` instead.
* Renamed `add_spells()` to `add_spell_duration()` to stop it silently
  shadowing (or being shadowed by) `peacesciencer::add_spells()` when
  both packages are attached in the same session -- the two are similar
  in spirit but not interchangeable (contentiousR's version works on any
  binary `_onset`/`_incidence`/`_ongoing` column, not a fixed set of
  bundled columns).
* `conflict_data()` now applies `start_year` and `end_year` consistently to
  every bundled source.
* UCDP/PRIO output now distinguishes conflict incidence from episode onset.
  `ucdp_prio_onset` is based on `start_date2`; the new
  `ucdp_prio_incidence` column identifies all active conflict-years.
* SCAD country-year totals now exclude UCDP placeholder records, deduplicate
  multi-location rows by event ID, and treat documented negative death-count
  codes as missing.
* All-missing groups now produce `NA` instead of `-Inf`, `NaN`, and associated
  warnings during conflict aggregation.
* Mass Mobilization participant strings are retained in
  `mm_participants_reported`; `mm_participants` summarizes only exact numeric
  reports.
* `add_conflict()` no longer re-aggregates sources that are already unique by
  country-year, preventing character-valued source fields from being dropped.

## API and robustness

* Added `as_peacesciencer_panel()` to supply the state-year key aliases and
  metadata required by `peacesciencer` while preserving contentiousR's API.
* Added `add_from_peacesciencer()` to apply a state-year `peacesciencer`
  function while keeping temporary `ccode`/`gwcode` keys out of the result.
* `build_states_cow_panel()` is internal again (not exported). It's the
  actual COW-panel implementation `build_states_panel(coding_system =
  "cow")` dispatches to, but there's no public reason to call it directly
  instead of `build_states_panel()` -- and its GW counterpart,
  `build_states_gw_panel()`, was never exported in the first place.
* Added strict validation for years, logical flags, panel keys, and output-key
  uniqueness.
* `build_states_panel()` now validates `start_year`/`end_year` against the
  Correlates of War / Gleditsch-Ward state system's actual coverage:
  `1816`-`2025`.
* Added `ucdp_vpp_incidence`. The existing `ucdp_vpp_onset` column remains as
  a compatibility alias and is now documented as incidence rather than onset.
* Removed the `magrittr` dependency in favor of R's native pipe.

## Release infrastructure

* Added broader test coverage, two introductory vignettes, package-level
  documentation, data provenance and licensing notes, pkgdown configuration,
  and GitHub Actions for package checks and site deployment.
* Added a case study article assembling a panel and modeling revolutionary
  onset, and a "Comparing revolutionary-event datasets" article (see
  Visualization above).
* Added a package logo, shown in the README and the pkgdown site navbar.
