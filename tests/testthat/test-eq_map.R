library(testthat)
library(RCapstone)
library(dplyr)
library(lubridate)
library(leaflet)

test_that("eq_map() returns a leaflet",{
  raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
  raw_clean <- raw_data %>% eq_clean_data() %>% eq_location_clean() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
  expect_is(eq_map(raw_clean,annot_col = "DATE"),"leaflet")
})
