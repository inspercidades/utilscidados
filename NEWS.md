# utilscidados 0.0.2

## New features

* `build_readme()` generates a bilingual (English/Portuguese)
  `README.txt` skeleton for a set of dataset files and their join
  keys.
* `read_metadata()`, `update_metadata()`, `create_metadata_sheet()`,
  and `validate_metadata()` manage the Dataverse metadata workbook:
  reading and writing field values by their Portuguese labels,
  cloning a template sheet for a new dataset, and checking a sheet
  for missing required fields, too few keywords, or leftover
  placeholder markers.
* `build_documentation()` now profiles logical columns (previously
  only text, numeric, and Date columns were summarised).

## Documentation

* Fixed an unresolved roxygen `@description` link to `write_with_check()`.
* Package description, README, and package-level docs now cover the
  new README/metadata-workbook helpers.
* Added maintainer ORCID; URLs now point at the `portalcidados` org
  repo.

## Internal

* Explicit `return()` used consistently across `R/`.
* Added test coverage for `utils.R` internal helpers.
