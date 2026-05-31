# export_shapefile ----

#' Per-format specifications for `export_shapefile()`
#' @keywords internal
#' @noRd
SHP_FORMATS <- list(
  geojson = list(
    ext = ".geojson",
    label = "GeoJSON",
    dataverse = TRUE,
    writer = function(shp, path, overwrite) {
      wgs_shp <- sf::st_transform(shp, crs = 4326)
      sf::st_write(wgs_shp, path, quiet = TRUE, delete_dsn = overwrite)
    }
  ),
  gpkg = list(
    ext = ".gpkg",
    label = "GeoPackage",
    dataverse = TRUE,
    writer = function(shp, path, overwrite) {
      sf::st_write(shp, path, quiet = TRUE, delete_dsn = overwrite)
    }
  ),
  shp = list(
    label = "Shapefile",
    dataverse = TRUE,
    path_fn = function(shp, out_dir, clean_name) {
      file.path(out_dir, clean_name, paste0(clean_name, ".shp"))
    },
    writer = function(shp, path, overwrite) {
      shp_dir <- dirname(path)
      if (!dir.exists(shp_dir)) {
        dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
      }
      sf::st_write(shp, path, quiet = TRUE, delete_dsn = overwrite)
    },
    enumerate = function(path) {
      clean_name <- tools::file_path_sans_ext(basename(path))
      list.files(
        dirname(path),
        pattern = paste0("^", clean_name, "\\."),
        full.names = TRUE
      )
    }
  ),
  geoparquet = list(
    ext = ".parquet",
    label = "GeoParquet",
    dataverse = TRUE,
    writer = function(shp, path, overwrite) {
      sfarrow::st_write_parquet(shp, path)
    }
  )
)

#' Export Spatial Data to Multiple Formats
#'
#' Writes a single `sf` object to one or more spatial file formats in a
#' common output directory, using a sanitised base file name.
#'
#' @param shp An `sf` object.
#' @param file_name Character scalar with the base file name (no
#'   extension). Required. Will be sanitised for filesystem safety.
#' @param out_dir Character scalar with the output directory. Defaults
#'   to the current working directory; created if it does not exist.
#' @param extension Character vector of format(s) to export. Valid:
#'   * `"all"` (default): geojson, gpkg, shp, geoparquet
#'   * `"dataverse"`: geojson, gpkg, shp, geoparquet
#'   * One or more of: `"geojson"`, `"gpkg"`, `"shp"`,
#'     `"geoparquet"` (or `"parquet"` as a synonym)
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
#' export_shapefile(nc, "north_carolina", out_dir = tempdir())
#' export_shapefile(
#'   nc,
#'   "north_carolina",
#'   out_dir   = tempdir(),
#'   extension = "geojson",
#'   overwrite = TRUE
#' )
#' }
#'
#' @seealso [sf::st_write()], [export_table()].
#' @importFrom sfarrow st_write_parquet
#' @export
export_shapefile <- function(
  shp,
  file_name,
  out_dir = getwd(),
  extension = "all",
  overwrite = FALSE
) {
  if (!inherits(shp, "sf")) {
    cli::cli_abort("Argument {.arg shp} must be an {.cls sf} object.")
  }
  if (missing(file_name) || !is.character(file_name) || length(file_name) != 1) {
    cli::cli_abort("Argument {.arg file_name} must be a single character string.")
  }

  if ("parquet" %in% extension) {
    extension <- unique(c(extension, "geoparquet"))
    extension <- setdiff(extension, "parquet")
  }

  valid_ext <- c("all", "dataverse", names(SHP_FORMATS))
  check_extension(extension, valid_ext)

  clean_name <- clean_file_name(file_name)
  out_dir <- resolve_out_dir(out_dir)

  formats <- resolve_formats(extension, SHP_FORMATS)
  exported_files <- character()
  for (fmt in formats) {
    written <- write_via_spec(
      SHP_FORMATS[[fmt]],
      shp,
      out_dir,
      clean_name,
      overwrite
    )
    exported_files <- c(exported_files, written)
  }

  summarise_exports(exported_files, out_dir, epsg = sf::st_crs(shp)$epsg)
  invisible(exported_files)
}
