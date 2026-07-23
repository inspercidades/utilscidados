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
