# build_documentation ----

#' Build Column-level Documentation for a Dataset
#'
#' Produces a data dictionary (one row per column) with column name, R
#' type, sample values or range, optional manual description, and
#' missing-value statistics. Designed for datasets prepared for
#' publication (e.g. on Dataverse).
#'
#' @param dat A `data.frame`, `tibble`, or `sf` object. If `sf`, geometry
#'   is dropped before profiling.
#' @param tbl_description Optional `data.frame` with columns
#'   `col_names` (character) and `description` (character) giving manual
#'   descriptions for each column. Columns not listed receive an `NA`
#'   description.
#'
#' @return A tibble with columns:
#'   * `Nome da Coluna` - column name
#'   * `Tipo da Coluna` - Portuguese label of the column type
#'   * `Valores` - sample values (categorical), range (date), or
#'     `"Cont\u00ednua"` (numeric)
#'   * `Descri\u00e7\u00e3o` - manual description (or `NA`)
#'   * `Percentual de Obs. Ausentes` - share of missing observations (%)
#'   * `Percentual de Preenchimento` - share of non-missing observations (%)
#'
#' @examples
#' \dontrun{
#' descs <- tibble::tibble(
#'   col_names = c("Sepal.Length", "Species"),
#'   description = c("Sepal length in cm", "Iris species")
#' )
#' build_documentation(iris, tbl_description = descs)
#' }
#' @export
build_documentation <- function(dat, tbl_description = NULL) {
  if (inherits(dat, "sf")) {
    dat <- sf::st_drop_geometry(dat)
  }
  if (!is.data.frame(dat)) {
    cli::cli_abort(
      "Argument {.arg dat} must be a {.cls data.frame} or {.cls sf} object."
    )
  }

  col_names <- names(dat)
  coltypes <- vapply(
    lapply(dat, class),
    format_coltype,
    character(1),
    USE.NAMES = FALSE
  )
  unique_values <- get_unique_values(dat)
  tbl_na_values <- get_missing_values(dat)

  doc <- tibble::tibble(
    col_names = col_names,
    tipo_coluna = coltypes,
    valores = unlist(unique_values, use.names = FALSE)
  )

  if (is.null(tbl_description)) {
    tbl_description <- tibble::tibble(
      col_names = col_names,
      description = NA_character_
    )
  }

  doc <- dplyr::left_join(doc, tbl_description, by = "col_names")
  doc <- dplyr::left_join(doc, tbl_na_values, by = "col_names")
  format_documentation(doc)
}

#' Create an Index of Dataset Files
#'
#' Builds a small lookup tibble mapping original file names to the table
#' names used in published documentation.
#'
#' @param name_file Character vector of original file names (e.g. CSV or
#'   XLSX file names).
#' @param name_table Character vector of corresponding table / sheet
#'   names. Must be the same length as `name_file`.
#'
#' @return A tibble with two columns:
#'   `Nome da aba/tabela` and `Nome original do arquivo (csv/xlsx)`.
#'
#' @examples
#' \dontrun{
#' create_documentation_index(
#'   name_file = c("dados_2023.csv", "dados_2024.csv"),
#'   name_table = c("dados_2023", "dados_2024")
#' )
#' }
#' @export
create_documentation_index <- function(name_file, name_table) {
  if (length(name_file) != length(name_table)) {
    cli::cli_abort(
      "{.arg name_file} and {.arg name_table} must have the same length."
    )
  }
  if (length(name_file) < 1) {
    cli::cli_abort("{.arg name_file} must have at least one element.")
  }

  tbl_index_docs <- tibble::tibble(
    `Nome da aba/tabela` = name_table,
    `Nome original do arquivo (csv/xlsx)` = name_file
  )
  tbl_index_docs
}

# Internal helpers ----

#' Summarise sample values for each column of a data frame
#'
#' For character columns, lists the first few unique values (up to ~50
#' characters total). For Date columns, gives a min/max range. For
#' numeric columns, returns `"Cont\u00ednua"`.
#'
#' @param dat A data frame.
#' @return A one-row tibble whose names match `names(dat)`.
#' @keywords internal
#' @noRd
get_unique_values <- function(dat) {
  col_names <- names(dat)
  is_text <- vapply(dat, \(x) is.character(x) || is.factor(x), logical(1))
  txt_cols <- col_names[is_text]
  date_cols <- col_names[vapply(dat, \(x) inherits(x, "Date"), logical(1))]
  num_cols <- col_names[vapply(dat, is.numeric, logical(1))]

  result <- character(ncol(dat))
  result <- stats::setNames(result, col_names)

  if (length(txt_cols) > 0) {
    txt_unique_values <- vapply(
      dat[txt_cols],
      \(x) glimpse_text_values(as.character(x)),
      character(1)
    )
    result[txt_cols] <- txt_unique_values
  }
  if (length(num_cols) > 0) {
    result[num_cols] <- "Cont\u00ednua"
  }
  if (length(date_cols) > 0) {
    date_range <- vapply(
      dat[date_cols],
      \(x) stringr::str_c(min(x, na.rm = TRUE), " / ", max(x, na.rm = TRUE)),
      character(1)
    )
    result[date_cols] <- date_range
  }

  out <- tibble::as_tibble(as.list(result))
  out
}

#' Build a short comma-separated preview of unique text values
#'
#' @param x A character vector.
#' @keywords internal
#' @noRd
glimpse_text_values <- function(x) {
  x10 <- utils::head(unique(stats::na.omit(x)), 10)
  if (length(x10) == 0) {
    return("Primeiros valores: -")
  }
  tsl <- cumsum(nchar(x10))
  if (min(tsl) >= 50) {
    label_text <- substr(x10[1], 1, 50)
    return(stringr::str_c("Primeiros valores: ", label_text, ", ..."))
  }
  indmax <- max(which(tsl < 50))
  label_text <- paste(x10[seq_len(indmax)], collapse = ", ")
  if (max(tsl) < 50) {
    stringr::str_c("Primeiros valores: ", label_text)
  } else {
    stringr::str_c("Primeiros valores: ", label_text, ", ...")
  }
}

#' Translate an R class label to a Portuguese display name
#' @keywords internal
#' @noRd
format_coltype <- function(x) {
  y <- c(
    "numeric" = "Num\u00e9rico",
    "integer" = "Num\u00e9rico",
    "double" = "Num\u00e9rico",
    "character" = "Texto",
    "factor" = "Texto",
    "logical" = "L\u00f3gico",
    "Date" = "Data",
    "POSIXct" = "Data",
    "POSIXt" = "Data"
  )
  matched <- as.character(stats::na.omit(y[x]))
  if (length(matched) == 0) {
    NA_character_
  } else {
    matched[1]
  }
}

#' Compute missing-value statistics for each column
#' @keywords internal
#' @noRd
get_missing_values <- function(dat) {
  dat |>
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      list(
        total_obs = ~ length(.x),
        missing_count = ~ sum(is.na(.x)),
        missing_percent = ~ round(100 * sum(is.na(.x)) / length(.x), 2),
        non_na_percent = ~ round(100 * sum(!is.na(.x)) / length(.x), 2)
      )
    )) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("col_names", "metric"),
      names_sep = "_(?=total_obs|missing_count|missing_percent|non_na_percent)",
      values_to = "value"
    ) |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value") |>
    dplyr::select(
      dplyr::all_of(c("col_names", "missing_percent", "non_na_percent"))
    )
}

#' Rename the technical documentation columns to user-facing labels
#' @keywords internal
#' @noRd
format_documentation <- function(dat) {
  rename_cols <- c(
    "Nome da Coluna" = "col_names",
    "Tipo da Coluna" = "tipo_coluna",
    "Valores" = "valores",
    "Descri\u00e7\u00e3o" = "description",
    "Percentual de Obs. Ausentes" = "missing_percent",
    "Percentual de Preenchimento" = "non_na_percent"
  )

  dat <- dplyr::mutate(
    dat,
    description = dplyr::case_when(
      is.na(description) ~ NA_character_,
      stringr::str_detect(description, "\\.$") ~ description,
      TRUE ~ paste0(description, ".")
    )
  )
  dplyr::rename(dat, dplyr::any_of(rename_cols))
}

# Quiet R CMD check about NSE column references
utils::globalVariables(c("description"))
