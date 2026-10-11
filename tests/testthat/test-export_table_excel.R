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
