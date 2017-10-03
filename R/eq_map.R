#' Earthquake Map
#'
#' This is a method to map the earthquake data. It passes the data to leaflet
#'
#' @param x Data set to display
#' @param annot_col Column that contained the annotated text that pops up.
#'
#' @return This function returns a leaflet to display
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
#' eq_clean_data(raw_data) %>% eq_location_clean() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  eq_map(annot_col = "DATE")
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addMarkers
#'
#' @export
eq_map<-function(x,annot_col){
  m <- leaflet::leaflet()
  m <- leaflet::addTiles(m)
  m <- leaflet::addMarkers(m, lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ x[[annot_col]])
  m
}

#' Annotate Earthquake data
#'
#' This is a function to create a new annotated information label based on Location,
#' Size of Earthquake, and Number of Deaths.
#' If there is no information in those fields, the tag will not be displayed.
#'
#' @param x Data set containing the information
#'
#' @return This function returns the dataset with a new column.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
#' eq_clean_data(raw_data) %>% eq_location_clean() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @export
eq_create_label<-function(x){
  x<-eq_location_clean(x)
  x<-dplyr::mutate(x,y="",
           y=ifelse(!is.na(LOCATION_NAME),
                    paste0(y,"<b>Location: </b>",LOCATION_NAME,"<br />"),
                    y),
           y=ifelse(!is.na(EQ_PRIMARY),
                    paste0(y,"<b>Magnitude: </b>",EQ_PRIMARY,"<br />"),
                    y),
           y=ifelse(!is.na(TOTAL_DEATHS),
                    paste0(y,"<b>Total Deaths: </b>",TOTAL_DEATHS,"<br />"),
                    y)
    )
  x$y
}
