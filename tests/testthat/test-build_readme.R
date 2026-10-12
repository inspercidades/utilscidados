test_that("build_readme protects an existing file unless overwrite is requested", {
  out_dir <- withr::local_tempdir()
  path <- file.path(out_dir, "README.txt")
  writeLines("original", path)
  files <- data.frame(file = "data.csv", label = "Data")
  keys <- data.frame(table = "data", key = "id")

  expect_error(
    build_readme(files, keys, "Dataset", out_dir = out_dir),
    "overwrite"
  )
  expect_identical(readLines(path), "original")

  build_readme(files, keys, "Dataset", out_dir = out_dir, overwrite = TRUE)
  expect_match(readLines(path)[1], "README")
})

test_that("build_readme writes [TODO] for missing descriptions", {
  keys <- data.frame(table = "a", key = "id")
  files <- data.frame(
    file = c("a.csv", "b.csv", "c.csv"),
    label = c("A", "B", "C"),
    description = c(NA, "", "Annual records")
  )

  path <- suppressMessages(
    build_readme(files, keys, title = "Test", out_dir = withr::local_tempdir())
  )
  lines <- readLines(path, encoding = "UTF-8")

  expect_equal(sum(lines == "| `a.csv` | [TODO] |"), 2)
  expect_equal(sum(lines == "| `b.csv` | [TODO] |"), 2)
  expect_equal(sum(lines == "| `c.csv` | Annual records |"), 2)
  expect_false(any(grepl("| NA |", lines, fixed = TRUE)))
})

test_that("build_readme writes [TODO] without a description column", {
  keys <- data.frame(table = "a", key = "id")
  files <- data.frame(file = "a.csv", label = "A")

  path <- suppressMessages(
    build_readme(files, keys, title = "Test", out_dir = withr::local_tempdir())
  )

  expect_equal(sum(readLines(path) == "| `a.csv` | [TODO] |"), 2)
})
