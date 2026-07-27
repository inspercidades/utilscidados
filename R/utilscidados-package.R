#' utilscidados: Internal Helpers for Insper Cidades Data Work
#'
#' The package supports two recurring workflows at Insper Cidades:
#'
#' 1. Standardised data exports compliant with Insper's Dataverse
#'    protocols, including file-format coverage, sanitised file names,
#'    column-level metadata (data dictionaries), README skeletons, and
#'    metadata workbook management.
#' 2. A simplified shapefile -> Mapbox Studio (GeoPortal) pipeline,
#'    with automatic reprojection to WGS84 and consistent naming.
#'
#' @section Main functions:
#' * [export_table()] - write a data frame to csv / xlsx / parquet / rds / feather
#' * [export_shapefile()] - write an `sf` object to geojson / gpkg / shp / geoparquet
#' * [export_tables_to_excel()] - write a list of data frames to a multi-sheet xlsx
#' * [mapbox_upload()] - upload an `sf` object to Mapbox Studio as a tileset
#' * [build_documentation()] - build a column-level data dictionary
#' * [create_documentation_index()] - build an index of file/table names
#' * [build_readme()] - generate a bilingual README.txt skeleton
#' * [read_metadata()] / [update_metadata()] - read or write values in a
#'   Dataverse metadata workbook
#' * [create_metadata_sheet()] - clone a template sheet for a new dataset
#' * [validate_metadata()] - check a metadata sheet for missing fields or
#'   placeholder markers
#'
#' Pass `extension = "dataverse"` to the exporters to select the
#' Dataverse-compliant subset of formats.
#'
#' @keywords internal
"_PACKAGE"
