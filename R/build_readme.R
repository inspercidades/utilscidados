# build_readme ----

#' Generate a Bilingual README.txt Skeleton
#'
#' Creates a bilingual (English/Portuguese) \code{README.txt} skeleton
#' from a table of dataset files and their join keys, leaving dataset
#' narrative sections for manual completion.
#'
#' @param files A data frame with columns:
#'   \code{file} (file name), \code{label} (short descriptor), and
#'   optionally \code{description} (one-line summary).
#' @param join_keys A data frame with columns \code{table} and
#'   \code{key} describing which columns serve as join keys across
#'   tables.
#' @param title Character scalar with the dataset collection title.
#' @param out_dir Directory where \code{README.txt} is written. Defaults
#'   to \code{getwd()}. Created if it does not exist.
#'
#' @return Invisibly returns the path to the written file.
#' @export
#'
#' @examples
#' \dontrun{
#' files <- data.frame(
#'   file = c("dados_2023.csv", "dados_2024.csv"),
#'   label = c("2023 data", "2024 data"),
#'   description = c("Annual records for 2023", "Annual records for 2024")
#' )
#' keys <- data.frame(
#'   table = c("dados_2023", "dados_2024"),
#'   key = c("id_municipio", "id_municipio")
#' )
#' build_readme(files, keys, title = "Municipal Data Collection")
#' }
build_readme <- function(files, join_keys, title, out_dir = getwd()) {
  if (!is.data.frame(files)) {
    cli::cli_abort("{.arg files} must be a data frame.")
  }
  required_cols <- c("file", "label")
  missing_cols <- setdiff(required_cols, names(files))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.arg files} is missing required column{?s}: {.val {missing_cols}}.")
  }
  if (!is.data.frame(join_keys)) {
    cli::cli_abort("{.arg join_keys} must be a data frame.")
  }
  if (missing(title) || !is.character(title) || length(title) != 1) {
    cli::cli_abort("{.arg title} must be a single character string.")
  }
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  has_desc <- "description" %in% names(files)

  lines <- c(
    "README",
    "=======",
    "",
    title,
    strrep("=", nchar(title)),
    "",
    "# English",
    "",
    "## Dataset Overview",
    "",
    "[TODO: Write a brief description of the dataset collection.]",
    "",
    "### Files",
    "",
    "| File | Description |",
    "|------|-------------|"
  )

  for (i in seq_len(nrow(files))) {
    desc <- if (has_desc && nzchar(files$description[i])) files$description[i] else "[TODO]"
    lines <- c(lines, paste0("| `", files$file[i], "` | ", desc, " |"))
  }

  lines <- c(lines,
    "",
    "### Join Keys",
    "",
    "| Table | Key column |",
    "|-------|------------|"
  )

  for (i in seq_len(nrow(join_keys))) {
    lines <- c(lines, paste0("| `", join_keys$table[i], "` | `", join_keys$key[i], "` |"))
  }

  lines <- c(lines,
    "",
    "## Notes",
    "",
    "[TODO: Add methodological notes, data sources, caveats.]",
    "",
    "---",
    "",
    "# Portugu\u00eas",
    "",
    "## Vis\u00e3o Geral do Conjunto de Dados",
    "",
    "[TODO: Escreva uma descri\u00e7\u00e3o breve do conjunto de dados.]",
    "",
    "### Arquivos",
    "",
    "| Arquivo | Descri\u00e7\u00e3o |",
    "|---------|-----------------|"
  )

  for (i in seq_len(nrow(files))) {
    desc <- if (has_desc && nzchar(files$description[i])) files$description[i] else "[TODO]"
    lines <- c(lines, paste0("| `", files$file[i], "` | ", desc, " |"))
  }

  lines <- c(lines,
    "",
    "### Chaves de Jun\u00e7\u00e3o",
    "",
    "| Tabela | Coluna chave |",
    "|--------|--------------|"
  )

  for (i in seq_len(nrow(join_keys))) {
    lines <- c(lines, paste0("| `", join_keys$table[i], "` | `", join_keys$key[i], "` |"))
  }

  lines <- c(lines,
    "",
    "## Notas",
    "",
    "[TODO: Adicione notas metodol\u00f3gicas, fontes dos dados, ressalvas.]",
    ""
  )

  path <- file.path(out_dir, "README.txt")
  writeLines(lines, path, useBytes = TRUE)
  cli::cli_inform("Generated README skeleton: {.file {path}}.")
  return(invisible(path))
}
