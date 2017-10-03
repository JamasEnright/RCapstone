## ---- results='hide'-----------------------------------------------------
raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))

## ------------------------------------------------------------------------
clean_data<-eq_clean_data(raw_data)
clean_data<-eq_location_clean(clean_data)

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
  geom_timeline()

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
  geom_timeline() + 
  geom_timeline_label()

