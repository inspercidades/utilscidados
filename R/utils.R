# Internal helpers ----

#' Sanitize a file name
#'
#' Lower-cases the name, replaces runs of non-alphanumerics with a single
#' underscore, and trims leading/trailing underscores.
#'
#' @param file_name A character scalar.
#' @return A cleaned character scalar.
#' @keywords internal
#' @noRd
clean_file_name <- function(file_name) {
  file_name |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9_]+", "_") |>
    stringr::str_replace_all("_{2,}", "_") |>
    stringr::str_remove("^_+|_+$")
}

#' Resolve an output directory, creating it if needed
#'
#' If `out_dir` is empty, returns the current working directory. Otherwise
#' creates the directory (recursively) if it does not exist.
#'
#' @param out_dir Character scalar with a directory path, or `""`.
#' @return Normalized directory path (character scalar).
#' @keywords internal
#' @noRd
resolve_out_dir <- function(out_dir) {
  if (identical(out_dir, "") || is.null(out_dir)) {
    out_dir <- getwd()
    cli::cli_inform(
      "No output directory specified. Using current working directory: {.path {out_dir}}"
    )
    return(out_dir)
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_inform("Created output directory: {.path {out_dir}}")
  }

  out_dir
}

#' Write a file with overwrite-protection and standard logging
#'
#' Wraps a writer function with a consistent existence check, try/catch, and
#' cli logging. Returns the path on success, `NULL` on skip or failure.
#'
#' @param path Destination file path.
#' @param label Human-readable format label (e.g. `"csv"`, `"GeoJSON"`).
#' @param overwrite Logical; if `FALSE` and `path` exists, the write is skipped.
#' @param writer A function of no arguments that performs the write.
#' @return The path (invisibly) on success, otherwise `NULL`.
#' @keywords internal
#' @noRd
write_with_check <- function(path, label, overwrite, writer) {
  if (file.exists(path) && !overwrite) {
    cli::cli_warn(
      "{label} file already exists: {.file {basename(path)}}. Use {.arg overwrite = TRUE} to replace it."
    )
    return(NULL)
  }

  existed <- file.exists(path)

  tryCatch(
    {
      writer()
      action <- if (existed) "Overwritten" else "Exported"
      cli::cli_inform("{action} {label}: {.file {basename(path)}}")
      invisible(path)
    },
    error = function(e) {
      cli::cli_warn("Failed to export {label}: {e$message}")
      NULL
    }
  )
}

#' Validate that `extension` is a subset of `valid`
#'
#' @param extension Character vector of requested extensions.
#' @param valid Character vector of allowed values.
#' @keywords internal
#' @noRd
check_extension <- function(extension, valid) {
  if (!is.character(extension) || length(extension) == 0) {
    cli::cli_abort(
      "Argument {.arg extension} must be a non-empty character vector."
    )
  }
  bad <- setdiff(extension, valid)
  if (length(bad) > 0) {
    cli::cli_abort(
      "Argument {.arg extension} must be one or more of: {.val {valid}}.
       Invalid value{?s}: {.val {bad}}."
    )
  }
  invisible(TRUE)
}

#' Summarise a set of exported files for the user
#'
#' @param exported_files Character vector of paths.
#' @param out_dir Output directory.
#' @param epsg Optional EPSG code to display (for spatial exports).
#' @keywords internal
#' @noRd
summarise_exports <- function(exported_files, out_dir, epsg = NULL) {
  if (length(exported_files) == 0) {
    cli::cli_alert_danger("No files were successfully exported!")
    return(invisible(NULL))
  }

  if (!is.null(epsg)) {
    cli::cli_alert_info("File{?s} exported using EPSG: {epsg}.")
  }
  cli::cli_alert_success(
    "Successfully exported {length(exported_files)} file{?s} to: {.path {out_dir}}"
  )

  total_size <- sum(file.size(exported_files), na.rm = TRUE)
  if (total_size > 0) {
    size_mb <- round(total_size / 1024^2, 2)
    cli::cli_inform("Total size: {size_mb} MB")
  }
  invisible(NULL)
}
