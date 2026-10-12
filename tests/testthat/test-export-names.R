test_that("exports reject names that clean to empty before creating output", {
  out_dir <- file.path(withr::local_tempdir(), "new-output")
  point <- sf::st_as_sf(
    data.frame(id = 1, x = 0, y = 0),
    coords = c("x", "y"),
    crs = 4326
  )

  expect_error(
    export_table(data.frame(id = 1), "...", out_dir, extension = "rds"),
    "file_name"
  )
  expect_false(dir.exists(out_dir))

  expect_error(
    export_shapefile(point, "...", out_dir, extension = "gpkg"),
    "file_name"
  )
  expect_false(dir.exists(out_dir))
})

test_that("Mapbox rejects an empty cleaned tileset ID before upload", {
  expect_error(
    mapbox_upload(NULL, "user", "...", access_token = "dummy"),
    "tileset_id"
  )
})

test_that("exports reject missing names with argument errors", {
  out_dir <- file.path(withr::local_tempdir(), "new-output")

  expect_error(
    export_table(data.frame(id = 1), NA_character_, out_dir, extension = "rds"),
    "file_name"
  )
  expect_false(dir.exists(out_dir))
  expect_error(
    mapbox_upload(NULL, "user", NA_character_, access_token = "dummy"),
    "tileset_id"
  )
})
