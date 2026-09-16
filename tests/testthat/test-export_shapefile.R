local_long_names_sf <- function() {
  dat <- data.frame(
    id = 1:3,
    n_unidades_his_por_bloco = c(10, 20, 30),
    n_unidades_hmp_por_bloco = c(1, 2, 3)
  )
  dat$geometry <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 2)),
    crs = 4326
  )
  return(sf::st_as_sf(dat))
}

# make_short_names() ----

test_that("make_short_names() keeps names that already fit", {
  expect_equal(
    make_short_names(c("id", "ano", "abcdefghij")),
    c("id", "ano", "abcdefghij")
  )
})

test_that("make_short_names() returns unique names within the limit", {
  x <- c("n_unidades_his_por_bloco", "n_unidades_hmp_por_bloco", "id")
  short <- make_short_names(x)

  expect_length(short, length(x))
  expect_true(all(nchar(short, type = "bytes") <= 10))
  expect_false(anyDuplicated(tolower(short)) > 0)
  expect_equal(short[3], "id")
})

test_that("make_short_names() resolves truncation collisions with a suffix", {
  x <- c("valor_venal_2020", "valor_venal_2021", "valor_venal_2022")
  short <- make_short_names(x)

  expect_false(anyDuplicated(tolower(short)) > 0)
  expect_true(all(nchar(short, type = "bytes") <= 10))
})

test_that("make_short_names() uses all available suffixes within the limit", {
  short <- make_short_names(rep("a", 10), max_length = 2)

  expect_length(unique(tolower(short)), 10)
  expect_lte(max(nchar(short, type = "bytes")), 2)
})

test_that("make_short_names() errors when the namespace is exhausted", {
  expect_snapshot(
    make_short_names(rep("a", 11), max_length = 2),
    error = TRUE
  )
})

test_that("make_short_names() never renames a name that already fits", {
  x <- c("area_terreno_total", "ar_trrn_tt")
  short <- make_short_names(x)

  expect_equal(short[2], "ar_trrn_tt")
  expect_false(tolower(short[1]) == tolower(short[2]))
})

test_that("make_short_names() counts bytes, not characters", {
  short <- make_short_names(c("área_construída", "situação"))

  expect_true(all(nchar(short, type = "bytes") <= 10))
})

test_that("make_short_names() is deterministic", {
  x <- c("n_unidades_his_por_bloco", "n_unidades_hmp_por_bloco")
  expect_identical(make_short_names(x), make_short_names(x))
})

test_that("make_short_names() validates its arguments", {
  expect_error(make_short_names(1:3), class = "rlang_error")
  expect_error(make_short_names("abc", max_length = 0), class = "rlang_error")
  expect_error(make_short_names("abc", max_length = 2.5), class = "rlang_error")
  expect_error(make_short_names("abc", max_length = Inf), class = "rlang_error")
  expect_error(make_short_names(c("a", NA)), class = "rlang_error")
})

# export_shapefile() shapefile output ----

test_that("shapefile export keeps every row when long names collide", {
  out_dir <- withr::local_tempdir()
  dat <- local_long_names_sf()

  suppressMessages(
    export_shapefile(dat, "pontos", out_dir = out_dir, extension = "shp")
  )

  shp <- sf::st_read(file.path(out_dir, "pontos", "pontos.shp"), quiet = TRUE)
  shp_names <- setdiff(names(shp), attr(shp, "sf_column"))

  expect_equal(nrow(shp), nrow(dat))
  expect_equal(shp_names, make_short_names(names(sf::st_drop_geometry(dat))))
  expect_equal(shp[[shp_names[2]]], dat$n_unidades_his_por_bloco)
  expect_equal(shp[[shp_names[3]]], dat$n_unidades_hmp_por_bloco)
})

test_that("shapefile export does not rename columns in other formats", {
  out_dir <- withr::local_tempdir()
  dat <- local_long_names_sf()

  suppressMessages(
    export_shapefile(
      dat,
      "pontos",
      out_dir = out_dir,
      extension = c("shp", "gpkg")
    )
  )

  gpkg <- sf::st_read(file.path(out_dir, "pontos.gpkg"), quiet = TRUE)
  expect_true(all(
    c("n_unidades_his_por_bloco", "n_unidades_hmp_por_bloco") %in% names(gpkg)
  ))
})

test_that("a failed shapefile write removes only shapefile components", {
  out_dir <- withr::local_tempdir()
  shp_dir <- file.path(out_dir, "pontos")
  dir.create(shp_dir)
  unrelated <- file.path(shp_dir, c("pontos.csv", "pontos.md"))
  file.create(unrelated)
  local_mocked_bindings(
    st_write = function(obj, dsn, ...) {
      stem <- tools::file_path_sans_ext(dsn)
      file.create(paste0(stem, c(".shp", ".shx", ".dbf")))
      stop("Write error")
    },
    .package = "sf"
  )

  expect_error(
    suppressMessages(
      export_shapefile(
        local_long_names_sf(),
        "pontos",
        out_dir = out_dir,
        extension = "shp"
      )
    ),
    "Shapefile"
  )
  expect_setequal(list.files(shp_dir), basename(unrelated))
})

test_that("check_shapefile() flags a shapefile with missing rows", {
  out_dir <- withr::local_tempdir()
  dat <- local_long_names_sf()
  names(dat)[2:3] <- c("n_his", "n_hmp")
  path <- file.path(out_dir, "vazio.shp")
  sf::st_write(dat[0, ], path, quiet = TRUE)

  expect_error(
    utilscidados:::check_shapefile(path, n_rows = 3, n_fields = 3),
    "0 features"
  )
})

test_that("check_shapefile() passes a complete shapefile", {
  out_dir <- withr::local_tempdir()
  dat <- local_long_names_sf()
  names(dat)[2:3] <- c("n_his", "n_hmp")
  path <- file.path(out_dir, "pontos.shp")
  sf::st_write(dat, path, quiet = TRUE)

  expect_no_error(utilscidados:::check_shapefile(
    path,
    n_rows = 3,
    n_fields = 3
  ))
})
