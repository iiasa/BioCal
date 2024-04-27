library(testthat)
library(BioCal)

test_that("calculate_biomass returns correct biomass value", {
  # Setup
  country <- "UK"
  species <- "Abies spp."
  compartment <- "CR"
  D <- 15
  H <- 10

  # Act
  result <- calculate_biomass(country, species, compartment, D, H)

  # Assert
  expect_is(result, "numeric")  # Check if result is numeric
  expect_true(length(result) > 0)  # Check that result is not empty
  expect_true(all(result >= 0))  # Check all results are non-negative (if applicable)
})
