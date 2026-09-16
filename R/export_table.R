# export_table ----

#' Per-format specifications for `export_table()`
#' @keywords internal
#' @noRd
TABLE_FORMATS <- list(
  csv = list(
    label = "csv",
    dataverse = TRUE,
    path_fn = function(dat, out_dir, clean_name) {
      ext <- if (nrow(dat) > 2e6) ".csv.gz" else ".csv"
      file.path(out_dir, paste0(clean_name, ext))
    },
    writer = function(dat, path, overwrite) {
      if (endsWith(path, ".gz")) {
        data.table::fwrite(dat, path, compress = "gzip")
      } else {
        data.table::fwrite(dat, path)
      }
    }
  ),
  xlsx = list(
    ext = ".xlsx",
    label = "xlsx",
    dataverse = TRUE,
    pre = function(dat) {
      if (nrow(dat) > 1e6) {
        cli::cli_alert_danger(
          "The dataset exceeds 1 million rows; consider an alternate format due to Excel's row limit."
        )
      }
    },
    writer = function(dat, path, overwrite) {
      writexl::write_xlsx(dat, path)
    }
  ),
  parquet = list(
    ext = ".parquet",
    label = "parquet",
    dataverse = TRUE,
    writer = function(dat, path, overwrite) {
      arrow::write_parquet(dat, path)
    }
  ),
  rds = list(
    ext = ".rds",
    label = "rds",
    writer = function(dat, path, overwrite) {
      readr::write_rds(dat, path, compress = "gz")
    }
  ),
  feather = list(
    ext = ".feather",
    label = "feather",
    writer = function(dat, path, overwrite) {
      arrow::write_feather(dat, path)
    }
  )
)

#' Export a Data Frame to Multiple Formats
#'
#' Writes a single data frame (or tibble / data.table) to one or more
#' file formats in a common output directory, using a sanitised base
#' file name.
#'
#' @param dat A `data.frame`, `tibble`, or `data.table`.
#' @param file_name Character scalar with the base file name (no
#'   extension). Required. Will be sanitised for filesystem safety.
#' @param out_dir Character scalar with the output directory. Defaults
#'   to the current working directory; created if it does not exist.
#' @param extension Character vector of format(s) to export. Valid:
#'   * `"all"` (default): csv, xlsx, parquet, rds, feather
#'   * `"dataverse"`: csv, xlsx, parquet (formats accepted by Dataverse)
#'   * One or more of: `"csv"`, `"xlsx"`, `"parquet"`, `"rds"`, `"feather"`
#' @param overwrite Logical. If `TRUE`, existing files are replaced. If
#'   `FALSE` (default), existing files are skipped with a warning.
#'
#' @details
#' For data frames above 2 million rows, CSV output is gzip-compressed
#' and gets a `.csv.gz` extension. XLSX output above the Excel row
#' limit (~1,048,576) emits a warning but still attempts to write.
#'
#' @return Invisibly returns a character vector of paths to successfully
#'   exported files.
#'
#' @examples
#' \dontrun{
#' iris_tbl <- tibble::as_tibble(iris)
#' export_table(iris_tbl, "iris", out_dir = tempdir())
#' export_table(
#'   iris_tbl,
#'   "iris",
#'   out_dir   = tempdir(),
#'   extension = c("csv", "parquet"),
#'   overwrite = TRUE
#' )
#' }
#'
#' @seealso [export_shapefile()], [export_tables_to_excel()].
#' @importFrom data.table fwrite
#' @importFrom writexl write_xlsx
#' @importFrom arrow write_parquet write_feather
#' @importFrom readr write_rds
#' @export
export_table <- function(
  dat,
  file_name,
  out_dir = getwd(),
  extension = "all",
  overwrite = FALSE
) {
  if (!is.data.frame(dat)) {
    cli::cli_abort(
      "Argument {.arg dat} must be a {.cls data.frame}, {.cls tibble}, or {.cls data.table}."
    )
  }
  if (
    missing(file_name) || !is.character(file_name) || length(file_name) != 1
  ) {
    cli::cli_abort(
      "Argument {.arg file_name} must be a single character string."
    )
  }

  valid_ext <- c("all", "dataverse", names(TABLE_FORMATS))
  check_extension(extension, valid_ext)

  clean_name <- clean_file_name(file_name)
  out_dir <- resolve_out_dir(out_dir)

  formats <- resolve_formats(extension, TABLE_FORMATS)
  exported_files <- character()
  for (fmt in formats) {
    written <- write_via_spec(
      TABLE_FORMATS[[fmt]],
      dat,
      out_dir,
      clean_name,
      overwrite
    )
    exported_files <- c(exported_files, written)
  }

  summarise_exports(exported_files, out_dir)
  return(invisible(exported_files))
}
