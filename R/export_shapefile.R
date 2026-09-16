# export_shapefile ----

SHAPEFILE_COMPONENT_EXTENSIONS <- c(
  "shp",
  "shx",
  "dbf",
  "prj",
  "qpj",
  "cpg",
  "qix",
  "sbn",
  "sbx",
  "fbn",
  "fbx",
  "ain",
  "aih",
  "atx",
  "ixs",
  "mxs",
  "idm",
  "ind",
  "shp.xml"
)

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
    strict = TRUE,
    path_fn = function(shp, out_dir, clean_name) {
      file.path(out_dir, clean_name, paste0(clean_name, ".shp"))
    },
    writer = function(shp, path, overwrite) {
      shp_dir <- dirname(path)
      if (!dir.exists(shp_dir)) {
        dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
      }

      shp <- shorten_sf_names(shp)
      sf::st_write(
        shp,
        path,
        quiet = TRUE,
        delete_dsn = overwrite && file.exists(path)
      )
      check_shapefile(path, n_rows = nrow(shp), n_fields = ncol(shp) - 1)
    },
    enumerate = function(path) {
      return(list_shapefile_parts(path))
    },
    cleanup = function(path) {
      unlink(list_shapefile_parts(path))
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
#' * Shapefile column names longer than 10 bytes are shortened with
#'   [make_short_names()]; the other formats keep the original names.
#'   Use `build_documentation(short_names = TRUE)` to record both names
#'   in the data dictionary.
#' * After writing, the Shapefile's feature and field counts are checked
#'   against `shp`. A failed or incomplete Shapefile raises an error and
#'   its partial files are removed. Failures in the other formats only
#'   warn.
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
#' @seealso [sf::st_write()], [export_table()], [make_short_names()].
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
  if (
    missing(file_name) || !is.character(file_name) || length(file_name) != 1
  ) {
    cli::cli_abort(
      "Argument {.arg file_name} must be a single character string."
    )
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
  return(invisible(exported_files))
}

# Internal helpers ----

#' Shorten the attribute column names of an sf object
#'
#' Applies [make_short_names()] to every column except the geometry and
#' tells the user how many names changed.
#'
#' @param shp An `sf` object.
#' @return `shp` with shortened attribute names.
#' @keywords internal
#' @noRd
shorten_sf_names <- function(shp) {
  geom_col <- attr(shp, "sf_column")
  attr_cols <- setdiff(names(shp), geom_col)
  short <- make_short_names(attr_cols)
  n_changed <- sum(short != attr_cols)

  if (n_changed > 0) {
    names(shp)[match(attr_cols, names(shp))] <- short
    cli::cli_inform(c(
      "i" = "Shortened {n_changed} column name{?s} to fit the Shapefile limit.",
      " " = "Record them with {.code build_documentation(short_names = TRUE)}."
    ))
  }
  return(shp)
}

#' Check that a written Shapefile holds every feature and field
#'
#' Reads only the layer header, so the check is cheap for large files.
#'
#' @param path Path to the `.shp` file.
#' @param n_rows,n_fields Expected feature and attribute field counts.
#' @return `TRUE`, invisibly; aborts on a mismatch.
#' @keywords internal
#' @noRd
check_shapefile <- function(path, n_rows, n_fields) {
  layer <- sf::st_layers(path)
  n_features <- layer$features[1]
  n_written_fields <- layer$fields[1]

  if (!identical(as.numeric(n_features), as.numeric(n_rows))) {
    cli::cli_abort(
      "Shapefile has {n_features} features; expected {n_rows}."
    )
  }
  if (!identical(as.numeric(n_written_fields), as.numeric(n_fields))) {
    cli::cli_abort(
      "Shapefile has {n_written_fields} fields; expected {n_fields}."
    )
  }
  return(invisible(TRUE))
}

#' List the sidecar files that make up a Shapefile
#'
#' @param path Path to the `.shp` file.
#' @keywords internal
#' @noRd
list_shapefile_parts <- function(path) {
  clean_name <- tools::file_path_sans_ext(basename(path))
  candidates <- list.files(dirname(path), full.names = TRUE)
  candidate_names <- tolower(basename(candidates))
  prefix <- paste0(tolower(clean_name), ".")
  has_stem <- startsWith(candidate_names, prefix)
  extensions <- substring(candidate_names, nchar(prefix) + 1)

  return(candidates[has_stem & extensions %in% SHAPEFILE_COMPONENT_EXTENSIONS])
}
