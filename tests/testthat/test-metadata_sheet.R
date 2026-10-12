test_that("read_metadata returns all fields when fields=NULL", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(
    wb,
    "Metadados",
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
  openxlsx::writeData(
    wb,
    "Metadados",
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
  openxlsx::writeData(
    wb,
    "Metadados",
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
  openxlsx::writeData(
    wb,
    "Metadados",
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
  openxlsx::writeData(
    wb,
    "Metadados",
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
  openxlsx::writeData(
    wb,
    "Metadados",
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
  openxlsx::writeData(
    wb,
    "Metadados",
    x = data.frame(
      V1 = c("", "", "", ""),
      V2 = c("T\u00edtulo", "Assunto", "Assunto", "Assunto"),
      V3 = c("Title", "", "", ""),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  update_metadata(
    tmp,
    "Metadados",
    list(subject = c("Mobilidade", "GPS", "Transporte"))
  )

  result <- read_metadata(tmp, "Metadados", fields = "subject")
  expect_equal(result$subject, c("Mobilidade", "GPS", "Transporte"))
})

test_that("update_metadata clears unused repeated rows", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(
    wb,
    "Metadados",
    data.frame(
      V1 = rep("", 3),
      V2 = rep("Assunto", 3),
      V3 = c("old1", "old2", "old3")
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  update_metadata(tmp, "Metadados", list(subject = "new"))

  expect_equal(read_metadata(tmp, "Metadados", "subject")$subject, "new")
})

test_that("update_metadata rejects repeated values beyond template capacity", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(
    wb,
    "Metadados",
    data.frame(V1 = c("", ""), V2 = rep("Assunto", 2), V3 = c("old1", "old2")),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  original <- unname(tools::md5sum(tmp))

  expect_error(
    update_metadata(tmp, "Metadados", list(subject = c("one", "two", "three"))),
    "subject"
  )
  expect_identical(unname(tools::md5sum(tmp)), original)
})

test_that("one repeated-field row also has a one-value capacity", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(
    wb,
    "Metadados",
    data.frame(V1 = "", V2 = "Assunto", V3 = "old"),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  original <- unname(tools::md5sum(tmp))

  expect_error(
    update_metadata(tmp, "Metadados", list(subject = c("one", "two"))),
    "subject"
  )
  expect_identical(unname(tools::md5sum(tmp)), original)
})

test_that("update_metadata writes to the labelled row after an empty row", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(
    wb,
    "Metadados",
    data.frame(V1 = "", V2 = "T\u00edtulo", V3 = "old title"),
    colNames = FALSE
  )
  openxlsx::writeData(
    wb,
    "Metadados",
    data.frame(V1 = "", V2 = "Autor", V3 = "old"),
    colNames = FALSE,
    startRow = 3
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  update_metadata(tmp, "Metadados", list(author = "new"))

  expect_equal(
    openxlsx::readWorkbook(
      tmp,
      "Metadados",
      cols = 3,
      rows = 3,
      colNames = FALSE
    )[1, 1],
    "new"
  )
})

test_that("create_metadata_sheet creates new sheet from template", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1-bilhetagem")
  openxlsx::writeData(
    wb,
    "1-bilhetagem",
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
  openxlsx::writeData(
    wb,
    "Metadados",
    x = data.frame(
      V1 = "",
      V2 = "T\u00edtulo",
      V3 = "",
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  expect_error(create_metadata_sheet(tmp, "Metadados"))
})

test_that("create_metadata_sheet accepts initial_values", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "1-bilhetagem")
  openxlsx::writeData(
    wb,
    "1-bilhetagem",
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

test_that("create_metadata_sheet preserves merged cells and styles", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  extraction_dir <- withr::local_tempdir()
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Template")
  openxlsx::writeData(
    wb,
    "Template",
    data.frame(
      V1 = c("", "", "", "Merged"),
      V2 = c("T\u00edtulo", "", "", ""),
      V3 = c("old", "", "", "")
    ),
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb,
    "Template",
    openxlsx::createStyle(fgFill = "#FF0000"),
    rows = 1,
    cols = 2
  )
  openxlsx::mergeCells(wb, "Template", cols = 1:2, rows = 4)
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  create_metadata_sheet(
    tmp,
    "New",
    template_sheet = "Template",
    initial_values = list(title = "new")
  )

  expect_equal(read_metadata(tmp, "New", "title")$title, "new")
  utils::unzip(tmp, files = "xl/worksheets/sheet2.xml", exdir = extraction_dir)
  sheet_xml <- paste(
    readLines(
      file.path(extraction_dir, "xl/worksheets/sheet2.xml"),
      warn = FALSE
    ),
    collapse = ""
  )
  expect_match(sheet_xml, 'mergeCell ref="A4:B4"', fixed = TRUE)
  expect_match(sheet_xml, '<c r="B1" s="[1-9]', perl = TRUE)
})

test_that("create_metadata_sheet does not save a failed initial update", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Template")
  openxlsx::writeData(
    wb,
    "Template",
    data.frame(V1 = c("", ""), V2 = rep("Assunto", 2), V3 = c("old1", "old2")),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  original <- unname(tools::md5sum(tmp))

  expect_error(
    create_metadata_sheet(
      tmp,
      "New",
      template_sheet = "Template",
      initial_values = list(subject = c("one", "two", "three"))
    ),
    "subject"
  )
  expect_identical(unname(tools::md5sum(tmp)), original)
  expect_identical(names(openxlsx::loadWorkbook(tmp)), "Template")
})

test_that("validate_metadata passes on complete sheet", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metadados")
  openxlsx::writeData(
    wb,
    "Metadados",
    x = data.frame(
      V1 = c("", "", "", "", "", ""),
      V2 = c(
        "Autor",
        "Produtor - se se aplica",
        "Contato Autor/Respons\u00e1vel",
        "Assunto",
        "Assunto",
        "Assunto"
      ),
      V3 = c(
        "Author Name",
        "Producer Name",
        "contact@email.com",
        "Mobilidade",
        "Transporte",
        "GPS"
      ),
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
  openxlsx::writeData(
    wb,
    "Metadados",
    x = data.frame(
      V1 = c("", "", ""),
      V2 = c(
        "Autor",
        "Produtor - se se aplica",
        "Contato Autor/Respons\u00e1vel"
      ),
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
  openxlsx::writeData(
    wb,
    "Metadados",
    x = data.frame(
      V1 = c("", "", "", "", "", ""),
      V2 = c(
        "Autor",
        "Produtor - se se aplica",
        "Contato Autor/Respons\u00e1vel",
        "Assunto",
        "Assunto",
        "Assunto"
      ),
      V3 = c(
        "Author",
        "Producer",
        "contact@email.com",
        "Mobilidade",
        "Transporte",
        "[CONFIRMAR...]"
      ),
      stringsAsFactors = FALSE
    ),
    colNames = FALSE
  )
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  expect_warning(validate_metadata(tmp, "Metadados"))
})
