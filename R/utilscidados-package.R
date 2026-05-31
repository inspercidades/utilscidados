#' utilscidados: Internal Helpers for Insper Cidades Data Work
#'
#' The package supports two recurring workflows at Insper Cidades:
#'
#' 1. Standardised data exports compliant with Insper's Dataverse
#'    protocols, including file-format coverage, sanitised file names,
#'    and column-level metadata (data dictionaries).
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
#'
#' Pass `extension = "dataverse"` to the exporters to select the
#' Dataverse-compliant subset of formats.
#'
#' @keywords internal
"_PACKAGE"
