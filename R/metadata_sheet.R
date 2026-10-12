# metadata_sheet ----

#' Field name mapping (Portuguese keys -> English aliases)
#'
#' Standard mapping between English field keys and their Portuguese column
#' labels as used in the Dataverse metadata sheets.
#' @keywords internal
#' @noRd
field_map <- list(
  "title" = "T\u00edtulo",
  "subtitle" = "Subt\u00edtulo",
  "alternative_title" = "T\u00edtulo Alternativo",
  "alternative_url" = "URL alternativo",
  "doi" = "DOI",
  "author" = "Autor",
  "contact" = "Contato Autor/Respons\u00e1vel",
  "publisher" = "Publicador",
  "description" = "Descri\u00e7\u00e3o",
  "subject" = "Assunto",
  "language" = "Idioma",
  "producer" = "Produtor - se se aplica",
  "contributor" = "Contribui\u00e7\u00e3o - se se aplica",
  "distributor" = "Distribuidor - se se aplica",
  "date_start" = "Data inicio",
  "date_end" = "Data fim",
  "type" = "Tipo de dado/informa\u00e7\u00e3o",
  "related_material" = "Materiais relacionados",
  "source" = "Fonte",
  "geographic_coverage" = "Cobertura geogr\u00e1fica",
  "typology" = "Tipologia - se se aplica",
  "temporal_coverage" = "Cobertura temporal"
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
  df <- update_metadata_workbook(wb, sheet_name, updates)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  cli::cli_inform("Updated {.val {sheet_name}} in {.file {file_path}}.")
  return(invisible(df))
}

update_metadata_workbook <- function(wb, sheet_name, updates) {
  # shortcut: templates must start on row 1; parse cell coordinates if leading blank rows are needed.
  df <- openxlsx::readWorkbook(
    wb,
    sheet = sheet_name,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  changes <- list()

  for (field_key in names(updates)) {
    field_name <- field_map[[field_key]]
    new_value <- updates[[field_key]]

    if (is.null(field_name)) {
      cli::cli_warn("Field {.val {field_key}} not found in field_map.")
      next
    }

    row_idx <- which(df[, 2] == field_name)

    if (length(row_idx) == 0) {
      cli::cli_warn(
        "Field {.val {field_name}} not found in sheet {.val {sheet_name}}."
      )
      next
    }

    if (field_key %in% c("subject", "contact", "geographic_coverage")) {
      if (length(new_value) > length(row_idx)) {
        cli::cli_abort(
          "{.arg {field_key}} has {length(new_value)} values, but the template has only {length(row_idx)} rows."
        )
      }
      df[row_idx, 3] <- NA_character_
      if (length(new_value) > 0) {
        df[row_idx[seq_along(new_value)], 3] <- new_value
      }
      changes[[length(changes) + 1]] <- list(rows = row_idx, values = new_value)
      next
    }

    row_idx <- row_idx[1]
    df[row_idx, 3] <- new_value
    changes[[length(changes) + 1]] <- list(rows = row_idx, values = new_value)
  }

  for (change in changes) {
    openxlsx::deleteData(
      wb,
      sheet = sheet_name,
      cols = 3,
      rows = change$rows,
      gridExpand = TRUE
    )
    for (i in seq_along(change$values)) {
      if (!is.na(change$values[i])) {
        openxlsx::writeData(
          wb,
          sheet = sheet_name,
          x = change$values[i],
          startCol = 3,
          startRow = change$rows[i],
          colNames = FALSE
        )
      }
    }
  }

  return(df)
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

  openxlsx::cloneWorksheet(
    wb,
    sheetName = new_sheet_name,
    clonedSheet = template_sheet
  )

  if (!is.null(initial_values)) {
    update_metadata_workbook(wb, new_sheet_name, initial_values)
  }
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  cli::cli_inform(
    "Created sheet {.val {new_sheet_name}} in {.file {file_path}}."
  )
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
      signal(
        "Required field {.field {field_map[[f]]}} is empty in sheet {.val {sheet_name}}."
      )
      ok <- FALSE
    }
  }

  keywords <- meta[["subject"]]
  if (
    is.null(keywords) ||
      all(is.na(keywords)) ||
      sum(!is.na(keywords) & nzchar(keywords)) < 3
  ) {
    signal(
      "Field {.field Assunto} must have at least 3 keywords in sheet {.val {sheet_name}}."
    )
    ok <- FALSE
  }

  for (nm in all_keys) {
    vals <- meta[[nm]]
    if (all(is.na(vals))) {
      next
    }
    for (v in vals) {
      if (is.na(v)) {
        next
      }
      if (
        grepl("\\[CONFIRMAR", v, ignore.case = TRUE) ||
          grepl("TODO", v, ignore.case = TRUE)
      ) {
        cli::cli_warn(
          "Field {.field {field_map[[nm]]}} contains a placeholder marker: {.val {v}}."
        )
        ok <- FALSE
      }
    }
  }

  if (ok) {
    cli::cli_inform("All checks passed for sheet {.val {sheet_name}}.")
  }
  return(invisible(ok))
}
