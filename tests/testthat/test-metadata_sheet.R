test_that("read_metadata returns all fields when fields=NULL", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", ""),
      V2 = c("T\u00edtulo", "Autor"),
      V3 = c("Meu Estudo", "Fulano"),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  result <- read_metadata(tmp, "Metadados")
  expect_type(result, "character")
  expect_equal(result[["T\u00edtulo"]], "Meu Estudo")
  expect_equal(result[["Autor"]], "Fulano")
})

test_that("read_metadata filters by fields when specified", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", ""),
      V2 = c("T\u00edtulo", "Autor"),
      V3 = c("Meu Estudo", "Fulano"),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  result <- read_metadata(tmp, "Metadados", fields = c("title", "author"))
  expect_named(result, c("title", "author"))
  expect_equal(result$title, "Meu Estudo")
  expect_equal(result$author, "Fulano")
})

test_that("read_metadata returns NA for missing fields", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = "",
      V2 = "T\u00edtulo",
      V3 = "Meu Estudo",
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  result <- read_metadata(tmp, "Metadados", fields = c("title", "author"))
  expect_equal(result$title, "Meu Estudo")
  expect_true(is.na(result$author))
})

test_that("read_metadata warns (not errors) on unknown field key", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = "",
      V2 = "T\u00edtulo",
      V3 = "Meu Estudo",
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  expect_warning(
    result <- read_metadata(tmp, "Metadados", fields = c("title", "bogus_key"))
  )
  expect_equal(result$title, "Meu Estudo")
  expect_null(result$bogus_key)
})

test_that("update_metadata warns (not errors) on unknown field key", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = "",
      V2 = "T\u00edtulo",
      V3 = "Old Title",
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  expect_warning(update_metadata(tmp, "Metadados", list(bogus_key = "v")))
})

test_that("update_metadata modifies values in-place", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", ""),
      V2 = c("T\u00edtulo", "Autor"),
      V3 = c("Old Title", "Old Author"),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  update_metadata(tmp, "Metadados", list(title = "New Title"))

  result <- read_metadata(tmp, "Metadados", fields = "title")
  expect_equal(result$title, "New Title")
})

test_that("update_metadata updates multiple subjects (keywords)", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", "", "", ""),
      V2 = c("T\u00edtulo", "Assunto", "Assunto", "Assunto"),
      V3 = c("Title", "", "", ""),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  update_metadata(tmp, "Metadados", list(subject = c("Mobilidade", "GPS", "Transporte")))

  result <- read_metadata(tmp, "Metadados", fields = "subject")
  expect_equal(result$subject, c("Mobilidade", "GPS", "Transporte"))
})

test_that("create_metadata_sheet creates new sheet from template", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1-bilhetagem")
  openxlsx::writeData(wb, "1-bilhetagem",
    x = data.frame(
      V1 = "",
      V2 = "T\u00edtulo",
      V3 = "",
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  create_metadata_sheet(tmp, "3-gps-onibus")
  sheet_names <- names(openxlsx::loadWorkbook(tmp))
  expect_true("3-gps-onibus" %in% sheet_names)
})

test_that("create_metadata_sheet aborts if sheet already exists", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(V1 = "", V2 = "T\u00edtulo", V3 = "", stringsAsFactors = FALSE),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  expect_error(create_metadata_sheet(tmp, "Metadados"))
})

test_that("create_metadata_sheet accepts initial_values", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1-bilhetagem")
  openxlsx::writeData(wb, "1-bilhetagem",
    x = data.frame(
      V1 = "",
      V2 = "T\u00edtulo",
      V3 = "",
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  create_metadata_sheet(tmp, "3-gps", initial_values = list(title = "GPS Data"))
  result <- read_metadata(tmp, "3-gps", fields = "title")
  expect_equal(result$title, "GPS Data")
})

test_that("validate_metadata passes on complete sheet", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", "", "", "", "", ""),
      V2 = c("Autor", "Produtor - se se aplica", "Contato Autor/Respons\u00e1vel",
             "Assunto", "Assunto", "Assunto"),
      V3 = c("Author Name", "Producer Name", "contact@email.com",
             "Mobilidade", "Transporte", "GPS"),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  expect_true(validate_metadata(tmp, "Metadados"))
})

test_that("validate_metadata warns on missing required fields", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", "", ""),
      V2 = c("Autor", "Produtor - se se aplica", "Contato Autor/Respons\u00e1vel"),
      V3 = c("", "", ""),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  expect_warning(validate_metadata(tmp, "Metadados"))
})

test_that("validate_metadata warns on placeholders", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(wb, "Metadados",
    x = data.frame(
      V1 = c("", "", "", "", "", ""),
      V2 = c("Autor", "Produtor - se se aplica", "Contato Autor/Respons\u00e1vel",
             "Assunto", "Assunto", "Assunto"),
      V3 = c("Author", "Producer", "contact@email.com",
             "Mobilidade", "Transporte", "[CONFIRMAR...]"),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  expect_warning(validate_metadata(tmp, "Metadados"))
})
