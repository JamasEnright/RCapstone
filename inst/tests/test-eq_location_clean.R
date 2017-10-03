library(testthat)
library(RCapstone)

test_that("eq_location_clean() returns a dataset",{
  raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
  raw_clean<-eq_clean_data(raw_data)
  expect_is(eq_location_clean(raw_clean),"data.frame")
})
