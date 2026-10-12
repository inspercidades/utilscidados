# utilscidados (development version)

* Removed unused internal workbook formatting helpers and
  `write_with_check()` (#20).
* Replaced `readr` and `writexl` in `export_table()` with base R and
  `openxlsx`, while keeping RDS and XLSX export behavior (#21).
* `mapbox_upload()` validates polling settings and stops waiting after
  `max_wait` seconds (#14).
* `export_tables_to_excel()` keeps sheet names unique after truncation,
  including case-insensitive collisions (#11).
* `export_shapefile()` reports the CRS of each format; GeoJSON uses
  EPSG:4326 and other formats retain the input CRS (#15).
* Table, spatial, and Mapbox exports now reject names that become empty
  after sanitizing, before creating files or submitting uploads (#13).
* `build_documentation()` now rejects invalid or duplicate manual column
  descriptions and handles empty date, datetime, and zero-row columns
  without infinite ranges or `NaN` percentages (#16, #17).
* `build_readme()` and `export_tables_to_excel()` now preserve existing
  outputs by default; pass `overwrite = TRUE` to replace them (#12).
* `create_metadata_sheet()` clones template formatting and saves only
  after initial values are applied successfully (#9).
* `update_metadata()` clears stale repeated values and rejects updates
  beyond the template's row capacity (#10).
* `build_readme()` now writes `[TODO]` for `NA` descriptions instead of
  the literal text `NA` (#18).
* `make_short_names()` shortens column names to fit a byte limit (10 by
  default, the Shapefile limit) while keeping them unique (#3).
* `export_shapefile()` now shortens Shapefile column names with
  `make_short_names()`. Long names that collided after abbreviation
  used to make the write fail and leave an empty Shapefile (#3).
* `export_shapefile()` now checks the written Shapefile's feature and
  field counts, and a failed Shapefile write raises an error and
  removes its partial files instead of warning (#3).
* `build_documentation()` gains `short_names`, which adds a
  `Nome no Shapefile` column with each column's Shapefile name (#3).

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
