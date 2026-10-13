# utilscidados 0.1.0

First working release.

## Breaking changes

* Renamed `build_readme()` to `write_dataset_readme()`,
  `build_documentation()` to `build_data_dictionary()`, and
  `create_documentation_index()` to `build_file_index()`. The old names
  are gone, with no deprecated aliases.
* Changed `write_dataset_readme()` and `export_tables_to_excel()` to keep
  existing files by default. Pass `overwrite = TRUE` to replace them
  (#12).
* Changed `export_shapefile()` to raise an error and remove partial files
  when a Shapefile write fails. It previously only warned (#3).
* Removed `write_with_check()` and unused internal workbook formatting
  helpers (#20).

## New features

* Added `make_short_names()`, which shortens column names to fit a byte
  limit (10 by default, the Shapefile limit) and keeps them unique (#3).
* Added `short_names` to `build_data_dictionary()`. It adds a
  `Nome no Shapefile` column with each column's Shapefile name (#3).
* Added a `max_wait` timeout and checks on the polling settings to
  `mapbox_upload()` (#14).
* Added the CRS of each format to the `export_shapefile()` summary.
  GeoJSON uses EPSG:4326, and other formats keep the input CRS (#15).

## Bug fixes

* Fixed `export_shapefile()` failing and leaving an empty Shapefile when
  long column names collided after abbreviation. It now shortens names
  with `make_short_names()` and checks the feature and field counts after
  writing (#3).
* Fixed duplicate sheet names in `export_tables_to_excel()` after
  truncation, including case-insensitive collisions (#11).
* Fixed table, spatial, and Mapbox exports accepting names that become
  empty after sanitizing. They now stop before creating files or
  starting uploads (#13).
* Fixed infinite ranges and `NaN` percentages in
  `build_data_dictionary()` for empty date, datetime, and zero-row
  columns. It also rejects invalid or duplicate manual descriptions
  (#16, #17).
* Fixed `create_metadata_sheet()` saving the workbook before initial
  values were applied. It also clones the template's formatting (#9).
* Fixed `update_metadata()` leaving stale repeated values. It also
  rejects updates beyond the template's row capacity (#10).
* Fixed `write_dataset_readme()` writing the literal text `NA` for
  missing descriptions. It now writes `[TODO]` (#18).

## Dependencies

* Removed `readr` and `writexl`. `export_table()` now writes RDS with
  base R and XLSX with `openxlsx`, with the same output as before (#21).

# utilscidados 0.0.2

## New features

* Added `build_readme()`, which generates a bilingual
  (English/Portuguese) `README.txt` skeleton for a set of dataset files
  and their join keys.
* Added `read_metadata()`, `update_metadata()`, `create_metadata_sheet()`,
  and `validate_metadata()` to manage the Dataverse metadata workbook.
  They read and write fields by their Portuguese labels, clone a
  template sheet for a new dataset, and flag missing required fields,
  too few keywords, or leftover placeholders.
* Added support for logical columns in `build_documentation()`.

## Documentation

* Fixed an unresolved roxygen link to `write_with_check()`.
* Updated the package description, README, and package docs to cover
  the README and metadata workbook helpers.
* Added the maintainer's ORCID and pointed URLs at the `portalcidados`
  repository.
