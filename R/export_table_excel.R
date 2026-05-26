# export_tables_to_excel ----

#' Export a List of Data Frames to a Multi-sheet Excel Workbook
#'
#' Writes a named list of data frames to a single `.xlsx` file, one sheet
#' per element, with a styled header row and auto-sized column widths.
#'
#' @param tables A non-empty list of data frames. If the list has names,
#'   they are used as sheet names; otherwise generic names (`"Sheet1"`,
#'   `"Sheet2"`, ...) are assigned. Sheet names longer than 31 characters
#'   (Excel's limit) are truncated, and duplicates are made unique.
#' @param file_name Character scalar with the full path of the `.xlsx`
#'   file to write. Required.
#' @param font_name Character scalar with the font family applied to the
#'   workbook (default `"Arial"`).
#' @param font_size Numeric body font size (default `11`).
#' @param header_font_size Numeric header font size (default `12`).
#'
#' @details
#' Sheets where the corresponding list element is not a data frame are
#' skipped with a warning. The file is always written with
#' `overwrite = TRUE` at the [openxlsx::saveWorkbook()] step; if the path
#' already exists a warning is emitted before writing.
#'
#' @return Invisibly returns `file_name`.
#'
#' @examples
#' \dontrun{
#' tables <- list(
#'   iris = iris,
#'   mtcars = mtcars
#' )
#' export_tables_to_excel(tables, file_name = file.path(tempdir(), "out.xlsx"))
#' }
#'
#' @seealso [export_table()] for single-table multi-format export.
#' @export
export_tables_to_excel <- function(
  tables,
  file_name,
  font_name = "Arial",
  font_size = 11,
  header_font_size = 12
) {
  ## Input validation ----
  if (!is.list(tables) || length(tables) == 0) {
    cli::cli_abort(
      "Argument {.arg tables} must be a non-empty list of data frames."
    )
  }
  if (!is.character(file_name) || length(file_name) != 1) {
    cli::cli_abort(
      "Argument {.arg file_name} must be a single character string."
    )
  }

  cli::cli_alert_info(
    "Starting Excel export with {.val {length(tables)}} table{?s}"
  )
  cli::cli_alert_info("Font: {.val {font_name}} (size {.val {font_size}})")

  if (file.exists(file_name)) {
    cli::cli_alert_warning(
      "File {.file {file_name}} already exists and will be overwritten"
    )
  }

  ## Workbook setup ----
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = font_size, fontName = font_name)

  header_style <- openxlsx::createStyle(
    fontName = font_name,
    fontSize = header_font_size,
    textDecoration = "bold"
  )

  ## Sheet names ----
  sheet_names <- resolve_sheet_names(tables)

  ## Per-table write ----
  cli::cli_progress_bar("Processing tables", total = length(tables))

  for (i in seq_along(tables)) {
    sheet_name <- sheet_names[i]
    table_data <- tables[[i]]

    if (!is.data.frame(table_data)) {
      cli::cli_alert_danger(
        "Table {.val {i}} ({.val {sheet_name}}) is not a data frame - skipping"
      )
      cli::cli_progress_update()
      next
    }
    if (nrow(table_data) == 0) {
      cli::cli_alert_warning("Table {.val {sheet_name}} is empty (0 rows)")
    }
    if (ncol(table_data) > 50) {
      cli::cli_alert_warning(
        "Table {.val {sheet_name}} has {.val {ncol(table_data)}} columns - Excel performance may be slow"
      )
    }

    tryCatch(
      {
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, table_data)
        if (nrow(table_data) > 0 && ncol(table_data) > 0) {
          openxlsx::addStyle(
            wb,
            sheet_name,
            header_style,
            rows = 1,
            cols = seq_len(ncol(table_data))
          )
          openxlsx::setColWidths(
            wb,
            sheet_name,
            cols = seq_len(ncol(table_data)),
            widths = "auto"
          )
        }
        cli::cli_progress_update()
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to process table {.val {sheet_name}}: {.val {e$message}}"
        )
        cli::cli_progress_update()
      }
    )
  }

  cli::cli_progress_done()

  ## Save ----
  cli::cli_alert_info("Saving Excel file...")
  tryCatch(
    {
      openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
      cli::cli_alert_success(
        "Excel file successfully saved as {.file {file_name}}"
      )
      file_size <- file.size(file_name)
      if (isTRUE(file_size > 1024^2)) {
        cli::cli_alert_info(
          "File size: {.val {round(file_size / 1024^2, 1)}} MB"
        )
      } else {
        cli::cli_alert_info(
          "File size: {.val {round(file_size / 1024, 1)}} KB"
        )
      }
    },
    error = function(e) {
      cli::cli_abort("Failed to save Excel file: {.val {e$message}}")
    }
  )

  invisible(file_name)
}

# Internal: derive Excel-safe, unique sheet names from a list ----
resolve_sheet_names <- function(tables) {
  if (is.null(names(tables))) {
    sheet_names <- paste0("Sheet", seq_along(tables))
    cli::cli_alert_warning(
      "No names found for tables, using generic sheet names: {.val {sheet_names}}"
    )
  } else {
    sheet_names <- names(tables)
    cli::cli_alert_info("Original sheet names: {.val {sheet_names}}")
  }

  original <- sheet_names
  sheet_names <- ifelse(
    nchar(sheet_names) > 31,
    substr(sheet_names, 1, 31),
    sheet_names
  )

  truncated <- which(nchar(original) > 31)
  if (length(truncated) > 0) {
    cli::cli_alert_warning(
      "Sheet name{?s} truncated to 31 characters (Excel limit):"
    )
    for (i in truncated) {
      cli::cli_alert_warning(
        "  {.val {original[i]}} -> {.val {sheet_names[i]}}"
      )
    }
  }

  if (any(duplicated(sheet_names))) {
    cli::cli_alert_warning(
      "Duplicate sheet names detected after truncation - adding suffixes"
    )
    sheet_names <- make.unique(sheet_names, sep = "_")
    sheet_names <- ifelse(
      nchar(sheet_names) > 31,
      substr(sheet_names, 1, 31),
      sheet_names
    )
  }

  sheet_names
}
