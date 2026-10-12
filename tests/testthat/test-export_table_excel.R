test_that("export_tables_to_excel protects an existing workbook", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  tables <- list(data = data.frame(value = 1L))
  export_tables_to_excel(tables, path)
  original <- unname(tools::md5sum(path))

  expect_error(
    export_tables_to_excel(list(data = data.frame(value = 2L)), path),
    "overwrite"
  )
  expect_identical(unname(tools::md5sum(path)), original)

  export_tables_to_excel(
    list(data = data.frame(value = 2L)),
    path,
    overwrite = TRUE
  )
  expect_equal(openxlsx::readWorkbook(path, "data")$value, 2L)
})

test_that("Excel sheet names remain unique after truncation", {
  long_name <- paste(rep("a", 31), collapse = "")
  names <- c(long_name, long_name, paste0(long_name, "_extra"), "Data", "data")
  tables <- rep(list(data.frame(value = 1)), length(names))
  names(tables) <- names

  sheets <- utilscidados:::resolve_sheet_names(tables)
  expect_length(sheets, length(tables))
  expect_true(all(nchar(sheets) <= 31))
  expect_equal(anyDuplicated(tolower(sheets)), 0)

  path <- tempfile(fileext = ".xlsx")
  suppressMessages(export_tables_to_excel(tables, path))
  expect_setequal(openxlsx::getSheetNames(path), sheets)
})
