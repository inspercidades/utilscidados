# export_table ----

#' Export a Data Frame to Multiple Formats
#'
#' Writes a single data frame (or tibble / data.table) to one or more file
#' formats in a common output directory, using a sanitized base file name.
#'
#' @param dat A `data.frame`, `tibble`, or `data.table`.
#' @param out_dir Character scalar with the output directory. If `""`
#'   (default), files are written to the current working directory. The
#'   directory is created if it does not exist.
#' @param file_name Character scalar with the base file name (without
#'   extension). Required. Will be sanitized to be filesystem-safe.
#' @param extension Character vector specifying the format(s) to export.
#'   Valid values:
#'   * `"all"` (default): csv, xlsx, parquet, rds, feather
#'   * `"dataverse"`: csv, xlsx, parquet (formats accepted by Dataverse)
#'   * `"csv"`, `"xlsx"`, `"parquet"`, `"rds"`, `"feather"`: a single format
#' @param overwrite Logical. If `TRUE`, existing files are replaced. If
#'   `FALSE` (default), existing files are skipped with a warning.
#'
#' @details
#' For very large data frames (more than 2 million rows), CSV output is
#' gzip-compressed and written with a `.csv.gz` extension via
#' [data.table::fwrite()].
#'
#' XLSX output above the Excel row limit (~1,048,576 rows) emits a warning
#' but still attempts to write. Prefer parquet or csv.gz for large data.
#'
#' @return Invisibly returns a character vector of paths to successfully
#'   exported files.
#'
#' @examples
#' \dontrun{
#' iris_tbl <- tibble::as_tibble(iris)
#' export_table(iris_tbl, out_dir = tempdir(), file_name = "iris")
#' export_table(
#'   iris_tbl,
#'   out_dir = tempdir(),
#'   file_name = "iris",
#'   extension = c("csv", "parquet"),
#'   overwrite = TRUE
#' )
#' }
#'
#' @seealso [export_shapefile()] for spatial data, [export_tables_to_excel()]
#'   for multi-sheet Excel workbooks.
#' @export
export_table <- function(
  dat,
  out_dir = "",
  file_name = NULL,
  extension = "all",
  overwrite = FALSE
) {
  ## Input validation ----
  if (!is.data.frame(dat)) {
    cli::cli_abort(
      "Argument {.arg dat} must be a {.cls data.frame}, {.cls tibble}, or {.cls data.table}."
    )
  }
  if (is.null(file_name)) {
    cli::cli_abort("Must provide a {.arg file_name} for the exported files.")
  }

  valid_ext <- c("all", "dataverse", "csv", "xlsx", "parquet", "rds", "feather")
  check_extension(extension, valid_ext)

  clean_name <- clean_file_name(file_name)
  out_dir <- resolve_out_dir(out_dir)

  wants <- function(fmt) {
    "all" %in% extension ||
      (fmt %in% c("csv", "xlsx", "parquet") && "dataverse" %in% extension) ||
      fmt %in% extension
  }

  exported_files <- character()

  ## CSV ----
  if (wants("csv")) {
    is_large <- nrow(dat) > 2e6
    csv_path <- file.path(
      out_dir,
      paste0(clean_name, if (is_large) ".csv.gz" else ".csv")
    )
    written <- write_with_check(csv_path, "csv", overwrite, function() {
      if (is_large) {
        data.table::fwrite(dat, csv_path, compress = "gzip")
      } else {
        data.table::fwrite(dat, csv_path)
      }
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  ## XLSX ----
  if (wants("xlsx")) {
    xlsx_path <- file.path(out_dir, paste0(clean_name, ".xlsx"))
    if (nrow(dat) > 1e6) {
      cli::cli_alert_danger(
        "The dataset exceeds 1 million rows; consider an alternate format due to Excel's row limit."
      )
    }
    written <- write_with_check(xlsx_path, "xlsx", overwrite, function() {
      writexl::write_xlsx(dat, xlsx_path)
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  ## Parquet ----
  if (wants("parquet")) {
    parquet_path <- file.path(out_dir, paste0(clean_name, ".parquet"))
    written <- write_with_check(parquet_path, "parquet", overwrite, function() {
      arrow::write_parquet(dat, parquet_path)
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  ## RDS ----
  if (wants("rds")) {
    rds_path <- file.path(out_dir, paste0(clean_name, ".rds"))
    written <- write_with_check(rds_path, "rds", overwrite, function() {
      readr::write_rds(dat, rds_path, compress = "gz")
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  ## Feather ----
  if (wants("feather")) {
    feather_path <- file.path(out_dir, paste0(clean_name, ".feather"))
    written <- write_with_check(feather_path, "feather", overwrite, function() {
      arrow::write_feather(dat, feather_path)
    })
    if (!is.null(written)) exported_files <- c(exported_files, written)
  }

  summarise_exports(exported_files, out_dir)
  invisible(exported_files)
}
