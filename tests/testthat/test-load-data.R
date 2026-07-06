test_that("statement data colnames are correct", {
  # NOTE: The working directory can't be set properly in `testthat`
  # I can't set this repo up like a proper package, because my Shiny deployment
  # (Posit Connect Cloud) seems to require a different structure than what a
  # real package has.
  setwd("../../")
  colnames <- c("tags", "statement", "var_name", "year")
  data_en <- readRDS("data/statements_en.rds")
  data_fr <- readRDS("data/statements_fr.rds")
  test_colnames_en <- data_en |> colnames()
  test_colnames_fr <- data_fr |> colnames()
  expect_equal(sort(test_colnames_en), sort(colnames))
  expect_equal(sort(test_colnames_fr), sort(colnames))
})
