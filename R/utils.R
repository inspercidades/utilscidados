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
#' Creates `out_dir` (recursively) if it does not exist.
#'
#' @param out_dir Character scalar with a directory path.
#' @return The directory path (unchanged).
#' @keywords internal
#' @noRd
resolve_out_dir <- function(out_dir) {
  if (!is.character(out_dir) || length(out_dir) != 1) {
    cli::cli_abort("{.arg out_dir} must be a single character path.")
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

#' Resolve a vector of requested extensions to a vector of format keys
#'
#' Handles the `"all"` and `"dataverse"` aliases against a registry of
#' format specs (each of which may set `dataverse = TRUE`).
#'
#' @keywords internal
#' @noRd
resolve_formats <- function(extension, registry) {
  all_fmts <- names(registry)
  if ("all" %in% extension) {
    return(all_fmts)
  }
  fmts <- intersect(extension, all_fmts)
  if ("dataverse" %in% extension) {
    dv <- names(registry)[
      vapply(registry, \(s) isTRUE(s$dataverse), logical(1))
    ]
    fmts <- union(fmts, dv)
  }
  fmts
}

#' Write one format from a registry spec
#'
#' Builds the path (via `spec$path_fn` if present), runs `spec$pre`, then
#' calls `spec$writer` under [write_with_check()] semantics.
#'
#' @return Character vector of paths actually written (length 0 on
#'   skip or failure).
#' @keywords internal
#' @noRd
write_via_spec <- function(spec, dat, out_dir, clean_name, overwrite) {
  path <- if (is.null(spec$path_fn)) {
    file.path(out_dir, paste0(clean_name, spec$ext))
  } else {
    spec$path_fn(dat, out_dir, clean_name)
  }

  if (!is.null(spec$pre)) spec$pre(dat)

  if (file.exists(path) && !overwrite) {
    cli::cli_warn(
      "{spec$label} file already exists: {.file {basename(path)}}. Use {.arg overwrite = TRUE} to replace it."
    )
    return(character(0))
  }
  existed <- file.exists(path)

  tryCatch(
    {
      spec$writer(dat, path, overwrite = overwrite)
      written <- if (!is.null(spec$enumerate)) spec$enumerate(path) else path
      action <- if (existed) "Overwritten" else "Exported"
      cli::cli_inform("{action} {spec$label}: {.file {basename(written[1])}}")
      if (length(written) > 1) {
        cli::cli_inform("Components: {.file {basename(written)}}")
      }
      written
    },
    error = function(e) {
      cli::cli_warn("Failed to export {spec$label}: {e$message}")
      character(0)
    }
  )
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
