## ---- results='hide'-----------------------------------------------------
library(dplyr)
raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))

## ------------------------------------------------------------------------
clean_data<-eq_clean_data(raw_data)
clean_data<-eq_location_clean(clean_data)

## ------------------------------------------------------------------------
subset(clean_data,COUNTRY %in% c("USA","CHINA")) %>%
  dplyr::filter(!is.na(EQ_PRIMARY), !is.na(DEATHS)) %>%
  ggplot2::ggplot() +
  ggplot2::aes(
    x = DATE,
    size = EQ_PRIMARY,
    colour = DEATHS,
    group=COUNTRY
  ) +
  geom_timeline()

## ------------------------------------------------------------------------
subset(clean_data,COUNTRY %in% c("USA","CHINA")) %>%
  dplyr::filter(!is.na(EQ_PRIMARY), !is.na(DEATHS)) %>%
  ggplot2::ggplot() +
  ggplot2::aes(
    x = DATE,
    size = EQ_PRIMARY,
    colour = DEATHS,
    group=COUNTRY,
    label=LOCATION_NAME
  ) +
  geom_timeline() + 
  geom_timeline_label()

## ------------------------------------------------------------------------
clean_data %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")

## ------------------------------------------------------------------------
clean_data %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")

