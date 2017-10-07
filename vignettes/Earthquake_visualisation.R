## ---- results='hide'-----------------------------------------------------
raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))

## ------------------------------------------------------------------------
clean_data<-RCapstone::eq_clean_data(raw_data)
clean_data<-RCapstone::eq_location_clean(clean_data)

## ------------------------------------------------------------------------
c1<-subset(clean_data,COUNTRY %in% c("USA","CHINA"))
c2<-dplyr::filter(c1,!is.na(EQ_PRIMARY), !is.na(DEATHS))
ggplot2::ggplot(data=c2) +
  ggplot2::aes(
    x = DATE,
    size = EQ_PRIMARY,
    colour = DEATHS,
    group=COUNTRY
  ) +
  RCapstone::geom_timeline()

## ------------------------------------------------------------------------
c1<-subset(clean_data,COUNTRY %in% c("USA","CHINA"))
c2<-dplyr::filter(c1,!is.na(EQ_PRIMARY), !is.na(DEATHS))
ggplot2::ggplot(data=c2) +
  ggplot2::aes(
    x = DATE,
    size = EQ_PRIMARY,
    colour = DEATHS,
    group=COUNTRY,
    label=LOCATION_NAME
  ) +
  RCapstone::geom_timeline() + 
  RCapstone::geom_timeline_label()

## ------------------------------------------------------------------------
c1<-dplyr::filter(clean_data,COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
RCapstone::eq_map(c1,annot_col = "DATE")

## ------------------------------------------------------------------------
c1<-dplyr::filter(clean_data,COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
c2<-dplyr::mutate(c1,popup_text = RCapstone::eq_create_label(c1))
RCapstone::eq_map(c1,annot_col = "popup_text")

