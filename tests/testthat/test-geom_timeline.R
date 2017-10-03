library(dplyr)
library(ggplot2)

test_that("geom_timeline() returns a ggplot",{
  raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
  outplot<-eq_clean_data(raw_data) %>% eq_location_clean() %>%
    dplyr::filter(!is.na(EQ_PRIMARY), !is.na(DEATHS)) %>%
    ggplot2::ggplot()+ggplot2::aes(x = DATE,size = EQ_PRIMARY,colour = DEATHS) + geom_timeline()
  expect_is(outplot,"ggplot")
})
