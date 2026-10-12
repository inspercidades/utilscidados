test_that("RDS export keeps the object and gzip compression", {
  out_dir <- withr::local_tempdir()
  dat <- data.frame(
    label = c("São Paulo", NA_character_),
    date = as.Date(c("2024-01-01", "2024-01-02"))
  )

  paths <- export_table(dat, "Example", out_dir, extension = "rds")
  path <- file.path(out_dir, "example.rds")

  expect_identical(paths, path)
  expect_identical(readRDS(path), dat)
  expect_identical(readBin(path, "raw", n = 2), as.raw(c(0x1f, 0x8b)))

  # Serialization version 2 keeps files readable by R < 3.5, as readr did.
  con <- gzfile(path, "rb")
  withr::defer(close(con))
  expect_identical(readChar(con, 2, useBytes = TRUE), "X\n")
  expect_identical(readBin(con, "integer", endian = "big"), 2L)
})

test_that("XLSX export keeps columns, values, and overwrite behavior", {
  out_dir <- withr::local_tempdir()
  dat <- data.frame(
    label = c("São Paulo", "Rio"),
    value = c(1.5, NA_real_),
    date = as.Date(c("2024-01-01", NA)),
    flag = c(TRUE, FALSE)
  )
  path <- file.path(out_dir, "example.xlsx")

  paths <- export_table(dat, "Example", out_dir, extension = "xlsx")
  expect_identical(paths, path)
  expect_identical(names(openxlsx::loadWorkbook(path)), "Sheet1")
  expect_equal(openxlsx::readWorkbook(path, 1, detectDates = TRUE), dat)

  original <- unname(tools::md5sum(path))
  expect_warning(
    skipped <- export_table(dat, "Example", out_dir, extension = "xlsx"),
    "already exists"
  )
  expect_length(skipped, 0)
  expect_identical(unname(tools::md5sum(path)), original)

  revised <- data.frame(label = "Brasília", value = 3.5)
  replaced <- export_table(
    revised,
    "Example",
    out_dir,
    extension = "xlsx",
    overwrite = TRUE
  )
  expect_identical(replaced, path)
  expect_equal(openxlsx::readWorkbook(path, 1), revised)
})
