test_that("clean_file_name lower-cases and replaces non-alnum runs", {
  expect_equal(utilscidados:::clean_file_name("My File Name"), "my_file_name")
  expect_equal(utilscidados:::clean_file_name("Dados 2023"), "dados_2023")
  expect_equal(utilscidados:::clean_file_name("  espaços  "), "espa_os")
})

test_that("clean_file_name removes leading/trailing underscores", {
  expect_equal(utilscidados:::clean_file_name("_leading"), "leading")
  expect_equal(utilscidados:::clean_file_name("trailing_"), "trailing")
  expect_equal(utilscidados:::clean_file_name("_both_"), "both")
})

test_that("clean_file_name collapses multiple underscores", {
  expect_equal(utilscidados:::clean_file_name("weird___name"), "weird_name")
  expect_equal(utilscidados:::clean_file_name("a__b__c"), "a_b_c")
})

test_that("check_extension aborts on bad values", {
  expect_error(utilscidados:::check_extension("bad", c("csv", "xlsx")))
  expect_error(utilscidados:::check_extension(
    c("csv", "bad"),
    c("csv", "xlsx")
  ))
})

test_that("check_extension passes on valid values", {
  expect_invisible(utilscidados:::check_extension("csv", c("csv", "xlsx")))
  expect_true(utilscidados:::check_extension(
    c("csv", "xlsx"),
    c("csv", "xlsx")
  ))
})

test_that("check_extension aborts on empty input", {
  expect_error(utilscidados:::check_extension(character(0), c("csv", "xlsx")))
  expect_error(utilscidados:::check_extension(1, c("csv", "xlsx")))
})

test_that("resolve_formats expands 'all' alias", {
  registry <- list(
    csv = list(dataverse = TRUE),
    xlsx = list(dataverse = TRUE),
    rds = list(dataverse = FALSE)
  )
  expect_equal(
    utilscidados:::resolve_formats("all", registry),
    c("csv", "xlsx", "rds")
  )
})

test_that("resolve_formats expands 'dataverse' alias", {
  registry <- list(
    csv = list(dataverse = TRUE),
    xlsx = list(dataverse = TRUE),
    rds = list(dataverse = FALSE)
  )
  expect_equal(
    utilscidados:::resolve_formats("dataverse", registry),
    c("csv", "xlsx")
  )
})

test_that("resolve_formats handles 'all' combined with specific formats", {
  registry <- list(
    csv = list(dataverse = TRUE),
    xlsx = list(dataverse = TRUE),
    rds = list(dataverse = FALSE)
  )
  expect_equal(
    utilscidados:::resolve_formats(c("all", "xlsx"), registry),
    c("csv", "xlsx", "rds")
  )
})
