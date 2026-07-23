test_that("glimpse_text_values handles a first value of exactly 50 characters", {
  # Regression test: a column whose first unique value has cumulative
  # nchar() of exactly 50 used to crash `build_documentation()` end-to-end
  # (seen in production against pemob_2020.csv, column "1.1.1"). The old
  # boundary check (`min(tsl) > 50` to early-return, `tsl < 50` to select)
  # left a gap at exactly 50: neither branch handled it, `which()` returned
  # `integer(0)`, `max(integer(0))` produced `-Inf`, and `seq_len(-Inf)`
  # errored.
  x50 <- strrep("a", 50)

  expect_no_error(utilscidados:::glimpse_text_values(x50))
  expect_match(
    utilscidados:::glimpse_text_values(x50),
    "^Primeiros valores: a{50}, \\.\\.\\.$"
  )
})

test_that("glimpse_text_values handles values just below/above the 50-char boundary", {
  expect_no_error(utilscidados:::glimpse_text_values(strrep("a", 49)))
  expect_no_error(utilscidados:::glimpse_text_values(strrep("a", 51)))
})

test_that("glimpse_text_values handles an all-NA character vector", {
  expect_equal(
    utilscidados:::glimpse_text_values(c(NA_character_, NA_character_)),
    "Primeiros valores: -"
  )
})

test_that("build_documentation() does not error on a 50-character text column", {
  dat <- data.frame(
    id = 1:2,
    label = c(strrep("a", 50), "b"),
    stringsAsFactors = FALSE
  )

  expect_no_error(doc <- build_documentation(dat))
  expect_equal(nrow(doc), 2)
  expect_true("Nome da Coluna" %in% names(doc))
})

test_that("format_documentation appends trailing period to descriptions missing one", {
  dat <- data.frame(
    col_names = c("x", "y"),
    tipo_coluna = c("Num\u00e9rico", "Texto"),
    valores = c("Cont\u00ednua", "a, b"),
    description = c("no period", "has period."),
    missing_percent = c(0, 0),
    non_na_percent = c(100, 100),
    stringsAsFactors = FALSE
  )
  out <- utilscidados:::format_documentation(dat)
  expect_match(out[[4]][1], "\\.$")
  expect_match(out[[4]][2], "\\.$")
})

test_that("format_documentation keeps NA descriptions as NA", {
  dat <- data.frame(
    col_names = "x",
    tipo_coluna = "Num\u00e9rico",
    valores = "Cont\u00ednua",
    description = NA_character_,
    missing_percent = 0,
    non_na_percent = 100,
    stringsAsFactors = FALSE
  )
  out <- utilscidados:::format_documentation(dat)
  expect_true(is.na(out[[4]][1]))
})
