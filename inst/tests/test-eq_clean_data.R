library(testthat)
library(RCapstone)

test_that("eq_clean_data() returns a dataset",{
  raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
  expect_is(eq_clean_data(raw_data),"data.frame")
})
