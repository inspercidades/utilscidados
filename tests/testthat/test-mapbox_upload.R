test_that("poll settings are validated before submission", {
  local_mocked_bindings(
    upload_tiles = function(...) stop("Upload should not run"),
    .package = "mapboxapi"
  )
  args <- list(
    shp = "layer.geojson",
    username = "user",
    tileset_id = "layer",
    access_token = "token",
    wait = TRUE
  )
  expect_error(
    do.call(mapbox_upload, c(args, list(poll_interval = 0))),
    "poll_interval"
  )
  expect_error(
    do.call(mapbox_upload, c(args, list(poll_interval = Inf))),
    "poll_interval"
  )
  expect_error(do.call(mapbox_upload, c(args, list(max_wait = -1))), "max_wait")
})

test_that("polling returns completion, reports API errors, and times out", {
  local_mocked_bindings(
    upload_tiles = function(...) list(id = "upload-1"),
    check_upload_status = function(...) list(complete = TRUE),
    .package = "mapboxapi"
  )
  args <- list(
    shp = "layer.geojson",
    username = "user",
    tileset_id = "layer",
    access_token = "token",
    wait = TRUE
  )
  expect_true(isTRUE(suppressMessages(do.call(mapbox_upload, args))$complete))

  local_mocked_bindings(
    check_upload_status = function(...) list(error = "invalid upload"),
    .package = "mapboxapi"
  )
  expect_error(suppressMessages(do.call(mapbox_upload, args)), "invalid upload")

  local_mocked_bindings(
    check_upload_status = function(...) list(complete = FALSE),
    .package = "mapboxapi"
  )
  expect_error(
    suppressMessages(do.call(
      mapbox_upload,
      c(args, list(max_wait = 0, poll_interval = 0.001))
    )),
    "timed out"
  )
})

test_that("polling does not start a status request after the deadline", {
  calls <- 0
  local_mocked_bindings(
    upload_tiles = function(...) list(id = "upload-1"),
    check_upload_status = function(...) {
      calls <<- calls + 1
      list(complete = FALSE)
    },
    .package = "mapboxapi"
  )
  expect_error(
    suppressMessages(mapbox_upload(
      shp = "layer.geojson",
      username = "user",
      tileset_id = "layer",
      access_token = "token",
      wait = TRUE,
      poll_interval = 1,
      max_wait = 0.05
    )),
    "timed out"
  )
  expect_equal(calls, 1)
})
