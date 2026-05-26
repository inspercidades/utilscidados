# export_shapefile ----

#' Export Spatial Data to Multiple Formats
#'
#' Writes a single `sf` object to one or more spatial file formats in a
#' common output directory, using a sanitized base file name.
#'
#' @param shp An `sf` object.
#' @param out_dir Character scalar with the output directory. If `""`
#'   (default), files are written to the current working directory. The
#'   directory is created if it does not exist.
#' @param file_name Character scalar with the base file name (without
#'   extension). Required. Will be sanitized to be filesystem-safe.
#' @param extension Character vector specifying the format(s) to export.
#'   Valid values:
#'   * `"all"` (default): geojson, gpkg, shp, geoparquet
#'   * `"dataverse"`: geojson, gpkg, shp, geoparquet (suitable for Dataverse)
#'   * `"geojson"`, `"gpkg"`, `"shp"`, `"parquet"`, `"geoparquet"`: a single format
#' @param overwrite Logical. If `TRUE`, existing files are replaced. If
#'   `FALSE` (default), existing files are skipped with a warning.
#'
#' @details
#' * GeoJSON is reprojected to WGS84 (EPSG:4326) before writing.
#' * Shapefile is written to a subdirectory named after the cleaned
#'   file name, since a shapefile is a set of sidecar files (`.shp`,
#'   `.shx`, `.dbf`, ...).
#' * GeoParquet is written via [sfarrow::st_write_parquet()].
#'
#' @return Invisibly returns a character vector of paths to successfully
#'   exported files.
#'
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#' export_shapefile(nc, out_dir = tempdir(), file_name = "north_carolina")
#' export_shapefile(
#'   nc,
#'   out_dir = tempdir(),
#'   file_name = "north_carolina",
#'   extension = "geojson",
#'   overwrite = TRUE
#' )
#' }
#'
#' @seealso [sf::st_write()], [export_table()] for tabular data.
#' @export
export_shapefile <- function(
  shp,
  out_dir = "",
  file_name = NULL,
  extension = "all",
  overwrite = FALSE
) {
  ## Input validation ----
  if (!inherits(shp, "sf")) {
    cli::cli_abort("Argument {.arg shp} must be an {.cls sf} object.")
  }
  if (is.null(file_name)) {
    cli::cli_abort("Must provide a {.arg file_name} for the exported files.")
  }

  valid_ext <- c(
    "all",
    "dataverse",
    "geojson",
    "gpkg",
    "shp",
    "parquet",
    "geoparquet"
  )
  check_extension(extension, valid_ext)

  clean_name <- clean_file_name(file_name)
  out_dir <- resolve_out_dir(out_dir)

  wants <- function(fmt) {
    fmts_dataverse <- c("geojson", "gpkg", "shp", "geoparquet")
    "all" %in% extension ||
      (fmt %in% fmts_dataverse && "dataverse" %in% extension) ||
      fmt %in% extension ||
      (fmt == "geoparquet" && "parquet" %in% extension)
  }

  exported_files <- character()

  ## GeoJSON ----
  if (wants("geojson")) {
    geojson_path <- file.path(out_dir, paste0(clean_name, ".geojson"))
    written <- write_with_check(geojson_path, "GeoJSON", overwrite, function() {
      wgs_shp <- sf::st_transform(shp, crs = 4326)
      sf::st_write(wgs_shp, geojson_path, quiet = TRUE, delete_dsn = overwrite)
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  ## GeoPackage ----
  if (wants("gpkg")) {
    gpkg_path <- file.path(out_dir, paste0(clean_name, ".gpkg"))
    written <- write_with_check(gpkg_path, "GeoPackage", overwrite, function() {
      sf::st_write(shp, gpkg_path, quiet = TRUE, delete_dsn = overwrite)
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  ## Shapefile ----
  if (wants("shp")) {
    shp_dir <- file.path(out_dir, clean_name)
    shp_path <- file.path(shp_dir, paste0(clean_name, ".shp"))

    if (file.exists(shp_path) && !overwrite) {
      cli::cli_warn(
        "Shapefile already exists: {.file {basename(shp_path)}}. Use {.arg overwrite = TRUE} to replace it."
      )
    } else {
      if (!dir.exists(shp_dir)) {
        dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
      }
      tryCatch(
        {
          sf::st_write(shp, shp_path, quiet = TRUE, delete_dsn = overwrite)
          shp_components <- list.files(
            shp_dir,
            pattern = paste0("^", clean_name, "\\."),
            full.names = TRUE
          )
          exported_files <- c(exported_files, shp_components)
          action <- if (overwrite) "Overwritten" else "Exported"
          cli::cli_inform(
            "{action} Shapefile components in directory: {.path {basename(shp_dir)}}"
          )
          cli::cli_inform("Components: {.file {basename(shp_components)}}")
        },
        error = function(e) {
          cli::cli_warn("Failed to export Shapefile: {e$message}")
        }
      )
    }
  }

  ## GeoParquet ----
  if (wants("geoparquet")) {
    parquet_path <- file.path(out_dir, paste0(clean_name, ".parquet"))
    written <- write_with_check(
      parquet_path,
      "GeoParquet",
      overwrite,
      function() {
        sfarrow::st_write_parquet(shp, parquet_path)
      }
    )
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  summarise_exports(exported_files, out_dir, epsg = sf::st_crs(shp)$epsg)
  invisible(exported_files)
}
