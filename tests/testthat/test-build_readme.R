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
