# metadata_sheet ----

#' Field name mapping (Portuguese keys -> English aliases)
#'
#' Standard mapping between English field keys and their Portuguese column
#' labels as used in the Dataverse metadata sheets.
#' @keywords internal
#' @noRd
field_map <- list(
  "title"               = "T\u00edtulo",
  "subtitle"            = "Subt\u00edtulo",
  "alternative_title"   = "T\u00edtulo Alternativo",
  "alternative_url"     = "URL alternativo",
  "doi"                 = "DOI",
  "author"              = "Autor",
  "contact"             = "Contato Autor/Respons\u00e1vel",
  "publisher"           = "Publicador",
  "description"         = "Descri\u00e7\u00e3o",
  "subject"             = "Assunto",
  "language"            = "Idioma",
  "producer"            = "Produtor - se se aplica",
  "contributor"         = "Contribui\u00e7\u00e3o - se se aplica",
  "distributor"         = "Distribuidor - se se aplica",
  "date_start"          = "Data inicio",
  "date_end"            = "Data fim",
  "type"                = "Tipo de dado/informa\u00e7\u00e3o",
  "related_material"    = "Materiais relacionados",
  "source"              = "Fonte",
  "geographic_coverage" = "Cobertura geogr\u00e1fica",
  "typology"            = "Tipologia - se se aplica",
  "temporal_coverage"   = "Cobertura temporal"
)

#' Update metadata values in an existing metadata sheet
#'
#' Writes values (title, author, keywords, etc.) into column 3 of a
#' Dataverse metadata sheet, looking up the correct row by the field
#' label in column 2.
#'
#' For multi-value fields (\code{subject}, \code{contact},
#' \code{geographic_coverage}) a vector of values populates consecutive
#' rows.
#'
#' @param file_path Path to the \code{.xlsx} workbook.
#' @param sheet_name Sheet name within the workbook.
#' @param updates A named list of values. Names must be keys from
#'   \code{field_map} (e.g. \code{"title"}, \code{"author"}).
#'   Values can be scalar character or a character vector for
#'   multi-row fields.
#'
#' @return Invisibly returns the modified data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' update_metadata(
#'   "Dataverse_-_Metadados.xlsx",
#'   "1-bilhetagem",
#'   list(title = "Novo T\u00edtulo", author = "Reginatto, Vinicius")
#' )
#' }
update_metadata <- function(file_path, sheet_name, updates) {
  wb <- openxlsx::loadWorkbook(file_path)
  df <- openxlsx::readWorkbook(wb, sheet = sheet_name, colNames = FALSE)

  for (field_key in names(updates)) {
    field_name <- field_map[[field_key]]
    new_value <- updates[[field_key]]

    if (is.null(field_name)) {
      cli::cli_warn("Field {.val {field_key}} not found in field_map.")
      next
    }

    row_idx <- which(df[, 2] == field_name)

    if (length(row_idx) == 0) {
      cli::cli_warn("Field {.val {field_name}} not found in sheet {.val {sheet_name}}.")
      next
    }

    if (length(row_idx) > 1) {
      if (field_key %in% c("subject", "contact", "geographic_coverage")) {
        n_vals <- length(new_value)
        for (i in seq_len(n_vals)) {
          if (i <= length(row_idx)) {
            df[row_idx[i], 3] <- new_value[i]
          }
        }
        next
      }
      row_idx <- row_idx[1]
    }

    df[row_idx, 3] <- new_value
  }

  openxlsx::writeData(wb, sheet = sheet_name, x = df, colNames = FALSE)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  cli::cli_inform("Updated {.val {sheet_name}} in {.file {file_path}}.")
  return(invisible(df))
}

#' Read metadata values from a metadata sheet
#'
#' Returns the current values stored in column 3 of a Dataverse
#' metadata sheet, optionally filtered to a subset of fields.
#'
#' @param file_path Path to the \code{.xlsx} workbook.
#' @param sheet_name Sheet name within the workbook.
#' @param fields Optional character vector of field keys (see
#'   \code{field_map}). When \code{NULL} (default), all fields are
#'   returned.
#'
#' @return A named list of values. Multi-row fields return character
#'   vectors.
#' @export
#'
#' @examples
#' \dontrun{
#' read_metadata("Dataverse_-_Metadados.xlsx", "1-bilhetagem")
#' read_metadata("Dataverse_-_Metadados.xlsx", "1-bilhetagem",
#'               fields = c("title", "author"))
#' }
read_metadata <- function(file_path, sheet_name, fields = NULL) {
  df <- openxlsx::read.xlsx(file_path, sheet = sheet_name, colNames = FALSE)

  if (is.null(fields)) {
    all_vals <- stats::setNames(df[, 3], df[, 2])
    return(all_vals[!is.na(names(all_vals))])
  }

  result <- list()
  for (field_key in fields) {
    field_name <- field_map[[field_key]]
    if (is.null(field_name)) {
      cli::cli_warn("Field {.val {field_key}} not found.")
      next
    }
    row_idx <- which(df[, 2] == field_name)
    if (length(row_idx) > 0) {
      values <- df[row_idx, 3]
      values <- values[!is.na(values)]
      result[[field_key]] <- if (length(values) == 0) NA else values
    } else {
      result[[field_key]] <- NA
    }
  }
  return(result)
}

#' Create a new metadata sheet from a template
#'
#' Copies the structure of an existing template sheet into a new sheet
#' and optionally sets initial field values.
#'
#' @param file_path Path to the \code{.xlsx} workbook.
#' @param new_sheet_name Name for the new sheet.
#' @param template_sheet Name of the existing sheet to use as a
#'   template (default \code{"1-bilhetagem"}).
#' @param initial_values Optional named list of field values passed to
#'   \code{\link{update_metadata}}.
#'
#' @return Invisibly returns \code{TRUE} on success.
#' @export
#'
#' @examples
#' \dontrun{
#' create_metadata_sheet(
#'   "Dataverse_-_Metadados.xlsx",
#'   "3-gps-onibus",
#'   initial_values = list(
#'     title = "GPS \u00d4nibus",
#'     subject = c("Mobilidade", "GPS")
#'   )
#' )
#' }
create_metadata_sheet <- function(
  file_path,
  new_sheet_name,
  template_sheet = "1-bilhetagem",
  initial_values = NULL
) {
  wb <- openxlsx::loadWorkbook(file_path)

  if (new_sheet_name %in% names(wb)) {
    cli::cli_abort("Sheet {.val {new_sheet_name}} already exists.")
  }

  template_data <- openxlsx::readWorkbook(wb, sheet = template_sheet, colNames = FALSE)
  openxlsx::addWorksheet(wb, sheetName = new_sheet_name)
  openxlsx::writeData(wb, sheet = new_sheet_name, x = template_data, colNames = FALSE)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  if (!is.null(initial_values)) {
    update_metadata(file_path, new_sheet_name, initial_values)
  }

  cli::cli_inform("Created sheet {.val {new_sheet_name}} in {.file {file_path}}.")
  return(invisible(TRUE))
}

#' Validate a metadata sheet for completeness
#'
#' Checks a generated Metadados sheet for:
#' \itemize{
#'   \item Empty required fields (Autor, Produtor, Contato).
#'   \item Shall we have at least 3 keywords (Assunto)?
#'   \item Leftover \code{[CONFIRMAR...]} / TODO-style placeholder markers.
#' }
#'
#' When \code{strict = FALSE} (the default), missing required fields
#' produce a warning and placeholder markers produce a message. When
#' \code{strict = TRUE}, missing required fields abort with an error.
#'
#' @param file_path Path to the \code{.xlsx} workbook.
#' @param sheet_name Sheet name within the workbook.
#' @param strict Logical. When \code{TRUE}, missing required fields
#'   trigger \code{cli_abort} instead of \code{cli_warn}.
#'
#' @return Invisibly returns \code{TRUE} if all checks pass, \code{FALSE}
#'   otherwise (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' validate_metadata("Dataverse_-_Metadados.xlsx", "1-bilhetagem")
#' validate_metadata("Dataverse_-_Metadados.xlsx", "1-bilhetagem", strict = TRUE)
#' }
validate_metadata <- function(file_path, sheet_name, strict = FALSE) {
  all_keys <- names(field_map)
  meta <- read_metadata(file_path, sheet_name, fields = all_keys)

  signal <- if (strict) cli::cli_abort else cli::cli_warn
  ok <- TRUE

  required_fields <- c("author", "producer", "contact")
  for (f in required_fields) {
    val <- meta[[f]]
    if (is.null(val) || all(is.na(val)) || all(grepl("^\\s*$", val))) {
      signal("Required field {.field {field_map[[f]]}} is empty in sheet {.val {sheet_name}}.")
      ok <- FALSE
    }
  }

  keywords <- meta[["subject"]]
  if (is.null(keywords) || all(is.na(keywords)) || sum(!is.na(keywords) & nzchar(keywords)) < 3) {
    signal("Field {.field Assunto} must have at least 3 keywords in sheet {.val {sheet_name}}.")
    ok <- FALSE
  }

  for (nm in all_keys) {
    vals <- meta[[nm]]
    if (all(is.na(vals))) next
    for (v in vals) {
      if (is.na(v)) next
      if (grepl("\\[CONFIRMAR", v, ignore.case = TRUE) || grepl("TODO", v, ignore.case = TRUE)) {
        cli::cli_warn("Field {.field {field_map[[nm]]}} contains a placeholder marker: {.val {v}}.")
        ok <- FALSE
      }
    }
  }

  if (ok) {
    cli::cli_inform("All checks passed for sheet {.val {sheet_name}}.")
  }
  return(invisible(ok))
}

# Formatting helpers (internal) ----

#' Format a worksheet: zoom and column widths
#' @noRd
format_sheet <- function(
  file_path,
  sheet_name,
  zoom_scale = 100,
  col_widths = "auto",
  freeze_panes = NULL
) {
  wb <- openxlsx::loadWorkbook(file_path)

  if (!sheet_name %in% names(wb)) {
    cli::cli_abort("Sheet {.val {sheet_name}} not found in workbook.")
  }

  wb$worksheets[[sheet_name]]$sheetViews <- paste0(
    '<sheetView workbookViewId="0" zoomScale="',
    zoom_scale,
    '" zoomScaleNormal="',
    zoom_scale,
    '"/>'
  )

  if (identical(col_widths, "auto")) {
    df <- openxlsx::readWorkbook(wb, sheet = sheet_name, colNames = FALSE)
    for (col in seq_len(ncol(df))) {
      max_width <- max(nchar(as.character(df[, col])), na.rm = TRUE)
      openxlsx::setColWidths(wb, sheet = sheet_name, cols = col, widths = min(max_width + 2, 100))
    }
  } else if (is.numeric(col_widths)) {
    df <- openxlsx::readWorkbook(wb, sheet = sheet_name, colNames = FALSE)
    ncols <- ncol(df)
    if (length(col_widths) == 1) {
      openxlsx::setColWidths(wb, sheet = sheet_name, cols = seq_len(ncols), widths = col_widths)
    } else {
      openxlsx::setColWidths(wb, sheet = sheet_name, cols = seq_along(col_widths), widths = col_widths)
    }
  }

  if (!is.null(freeze_panes)) {
    openxlsx::freezePane(wb, sheet = sheet_name, firstRow = freeze_panes[1], firstCol = freeze_panes[2])
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  return(invisible(TRUE))
}

#' Format all sheets in a workbook
#' @noRd
format_all_sheets <- function(
  file_path,
  zoom_scale = 100,
  col_widths = "auto",
  freeze_panes = NULL,
  exclude_sheets = NULL
) {
  wb <- openxlsx::loadWorkbook(file_path)
  sheet_names <- names(wb)

  if (!is.null(exclude_sheets)) {
    sheet_names <- setdiff(sheet_names, exclude_sheets)
  }

  for (sheet in sheet_names) {
    format_sheet(file_path, sheet, zoom_scale, col_widths, freeze_panes)
  }

  cli::cli_inform("Formatted {.val {length(sheet_names)}} sheet{?s}.")
  return(invisible(TRUE))
}

#' Copy formatting from one sheet to another
#' @noRd
copy_sheet_formatting <- function(
  file_path,
  source_sheet,
  target_sheet,
  copy_col_widths = TRUE,
  copy_row_heights = TRUE,
  copy_zoom = TRUE,
  copy_freeze_panes = TRUE,
  copy_cell_styles = TRUE
) {
  wb <- openxlsx::loadWorkbook(file_path)

  if (!source_sheet %in% names(wb)) cli::cli_abort("Source sheet {.val {source_sheet}} not found.")
  if (!target_sheet %in% names(wb)) cli::cli_abort("Target sheet {.val {target_sheet}} not found.")

  source_idx <- which(names(wb) == source_sheet)
  target_idx <- which(names(wb) == target_sheet)
  source_data <- openxlsx::readWorkbook(wb, sheet = source_sheet, colNames = FALSE)
  target_data <- openxlsx::readWorkbook(wb, sheet = target_sheet, colNames = FALSE)

  if (copy_col_widths) {
    tryCatch({
      if (!is.null(wb$colWidths[[source_idx]])) {
        for (col_info in wb$colWidths[[source_idx]]) {
          if (!is.null(col_info$width)) {
            openxlsx::setColWidths(wb, sheet = target_sheet, cols = col_info$cols, widths = col_info$width)
          }
        }
      }
    }, error = function(e) NULL)
  }

  if (copy_row_heights) {
    tryCatch({
      if (!is.null(wb$rowHeights[[source_idx]])) {
        for (row_info in wb$rowHeights[[source_idx]]) {
          if (!is.null(row_info$height)) {
            openxlsx::setRowHeights(wb, sheet = target_sheet, rows = row_info$rows, heights = row_info$height)
          }
        }
      }
    }, error = function(e) NULL)
  }

  if (copy_zoom) {
    tryCatch({
      wb$worksheets[[target_idx]]$sheetViews <- wb$worksheets[[source_idx]]$sheetViews
    }, error = function(e) NULL)
  }

  if (copy_freeze_panes) {
    tryCatch({
      wb$worksheets[[target_idx]]$freezePane <- wb$worksheets[[source_idx]]$freezePane
    }, error = function(e) NULL)
  }

  if (copy_cell_styles) {
    tryCatch({
      n_rows <- min(nrow(source_data), nrow(target_data))
      n_cols <- min(ncol(source_data), ncol(target_data))
      for (row in seq_len(n_rows)) {
        for (col in seq_len(n_cols)) {
          style_obj <- tryCatch(
            wb$styleObjects[[wb$worksheets[[source_idx]]$sheet_data[[row]][[col]]$style]],
            error = function(e) NULL
          )
          if (!is.null(style_obj)) {
            openxlsx::addStyle(wb, sheet = target_sheet, style = style_obj, rows = row, cols = col, gridExpand = FALSE, stack = FALSE)
          }
        }
      }
    }, error = function(e) NULL)
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  cli::cli_inform("Copied formatting from {.val {source_sheet}} to {.val {target_sheet}}.")
  return(invisible(TRUE))
}

#' Copy formatting from one sheet to multiple target sheets
#' @noRd
copy_formatting_to_multiple <- function(
  file_path,
  source_sheet,
  target_sheets,
  ...
) {
  for (target in target_sheets) {
    copy_sheet_formatting(file_path, source_sheet, target, ...)
  }
  cli::cli_inform("Formatted {.val {length(target_sheets)}} sheet{?s} based on {.val {source_sheet}}.")
  return(invisible(TRUE))
}
