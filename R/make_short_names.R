# make_short_names ----

#' Shorten Column Names for Formats with a Name-Length Limit
#'
#' Shortens a vector of column names so each fits within `max_length`
#' bytes and all are unique, ignoring case. Shapefiles store attribute
#' names in a `.dbf` file that allows at most 10 bytes per name, so
#' [export_shapefile()] applies this function before writing a
#' Shapefile.
#'
#' @param x Character vector of column names, without `NA`.
#' @param max_length Positive integer with the maximum name length in
#'   bytes. Defaults to 10, the Shapefile limit.
#'
#' @details
#' Names that already fit are kept unchanged. Longer names are
#' abbreviated with [base::abbreviate()] and truncated to `max_length`
#' bytes. When two names collide, the later one gets a numeric suffix
#' such as `"_1"`. The result depends only on `x`, so calling the
#' function twice on the same names gives the same short names; this is
#' how [build_documentation()] records the names a Shapefile ships
#' with.
#'
#' @return A character vector the same length as `x`.
#'
#' @examples
#' make_short_names(c("id", "n_unidades_his_por_bloco", "n_unidades_hmp_por_bloco"))
#'
#' @seealso [export_shapefile()], [build_documentation()].
#' @export
make_short_names <- function(x, max_length = 10) {
  if (!is.character(x) || anyNA(x)) {
    cli::cli_abort(
      "Argument {.arg x} must be a character vector without {.val NA}."
    )
  }
  if (
    !is.numeric(max_length) ||
      length(max_length) != 1 ||
      is.na(max_length) ||
      max_length < 2
  ) {
    cli::cli_abort("Argument {.arg max_length} must be a single number >= 2.")
  }

  is_long <- nchar(x, type = "bytes") > max_length
  short <- x

  if (any(is_long)) {
    abbreviated <- suppressWarnings(
      abbreviate(x[is_long], minlength = max_length, named = FALSE)
    )
    short[is_long] <- truncate_bytes(abbreviated, max_length)
  }

  taken <- character(0)
  # Names that already fit are claimed first, so they are never renamed
  for (i in order(is_long)) {
    candidate <- short[i]
    suffix <- 1
    while (tolower(candidate) %in% taken) {
      suffix_chr <- paste0("_", suffix)
      candidate <- paste0(
        truncate_bytes(short[i], max_length - nchar(suffix_chr)),
        suffix_chr
      )
      suffix <- suffix + 1
    }
    short[i] <- candidate
    taken <- c(taken, tolower(candidate))
  }

  return(short)
}

# Internal helpers ----

#' Truncate strings to a maximum number of bytes
#'
#' Drops trailing characters until each string fits, so multibyte
#' characters are never split.
#'
#' @param x A character vector.
#' @param max_bytes A single positive integer.
#' @keywords internal
#' @noRd
truncate_bytes <- function(x, max_bytes) {
  truncated <- vapply(
    x,
    \(s) {
      s <- substr(s, 1, max_bytes)
      while (nchar(s, type = "bytes") > max_bytes) {
        s <- substr(s, 1, nchar(s) - 1)
      }
      return(s)
    },
    character(1),
    USE.NAMES = FALSE
  )
  return(truncated)
}
