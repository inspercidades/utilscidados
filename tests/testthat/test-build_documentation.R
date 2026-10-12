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

test_that("get_unique_values handles all-NA logical column", {
  dat <- data.frame(
    id = 1:3,
    flag = c(NA, NA, NA)
  )
  dat$flag <- as.logical(dat$flag)
  out <- utilscidados:::get_unique_values(dat)
  expect_equal(out$flag, "Primeiros valores: -")
})

test_that("get_unique_values handles mixed logical column", {
  dat <- data.frame(
    id = 1:3,
    flag = c(TRUE, FALSE, TRUE)
  )
  out <- utilscidados:::get_unique_values(dat)
  expect_equal(out$flag, "Primeiros valores: TRUE, FALSE")
})

test_that("build_documentation() adds shapefile names when asked", {
  dat <- data.frame(
    id = 1:2,
    n_unidades_his_por_bloco = c(1, 2),
    n_unidades_hmp_por_bloco = c(3, 4)
  )

  doc <- build_documentation(dat, short_names = TRUE)

  expect_equal(names(doc)[1:2], c("Nome da Coluna", "Nome no Shapefile"))
  expect_equal(doc[["Nome no Shapefile"]], make_short_names(names(dat)))
})

test_that("build_documentation() omits shapefile names by default", {
  doc <- build_documentation(data.frame(id = 1:2))
  expect_false("Nome no Shapefile" %in% names(doc))
})

test_that("build_documentation validates description schema and keys", {
  dat <- data.frame(a = 1:2, b = c("x", "y"))

  expect_error(build_documentation(dat, tbl_description = 1), "tbl_description")
  expect_error(
    build_documentation(dat, tbl_description = data.frame(col_names = "a")),
    "tbl_description"
  )
  expect_error(
    build_documentation(
      dat,
      tbl_description = data.frame(col_names = 1, description = "A")
    ),
    "col_names"
  )
  expect_error(
    build_documentation(
      dat,
      tbl_description = data.frame(col_names = "a", description = 1)
    ),
    "description"
  )
  expect_error(
    build_documentation(
      dat,
      tbl_description = data.frame(
        col_names = c("a", "a"),
        description = c("First", "Second")
      )
    ),
    "col_names"
  )

  doc <- build_documentation(
    dat,
    tbl_description = data.frame(col_names = "a", description = "First")
  )
  expect_equal(nrow(doc), ncol(dat))
  expect_equal(doc[["Nome da Coluna"]], names(dat))
  expect_equal(doc[["Descrição"]][1], "First.")
  expect_true(is.na(doc[["Descrição"]][2]))
})

test_that("build_documentation handles missing and date-time ranges", {
  dat <- data.frame(
    date = as.Date(c(NA, NA)),
    timestamp = as.POSIXct(
      c("2024-01-01 12:34:56", "2024-01-02 14:00:00"),
      tz = "UTC"
    )
  )

  expect_no_warning(doc <- build_documentation(dat))
  expect_equal(doc[["Valores"]][1], "-")
  expect_match(doc[["Valores"]][2], "2024-01-01 12:34:56")
  expect_match(doc[["Valores"]][2], "2024-01-02 14:00:00")
})

test_that("build_documentation handles zero-row columns", {
  dat <- data.frame(
    date = as.Date(character()),
    timestamp = as.POSIXct(character(), tz = "UTC"),
    amount = numeric()
  )

  expect_no_warning(doc <- build_documentation(dat))
  expect_equal(nrow(doc), ncol(dat))
  expect_equal(doc[["Valores"]][1:2], c("-", "-"))
  expect_type(doc[["Percentual de Obs. Ausentes"]], "double")
  expect_true(all(is.na(doc[["Percentual de Obs. Ausentes"]])))
  expect_true(all(is.na(doc[["Percentual de Preenchimento"]])))
  expect_false(any(is.nan(doc[["Percentual de Obs. Ausentes"]])))
})
