#' Clean Earthquake Data
#'
#' This is a simple function that cleans the Earthquake dataset.
#' It maps date to a proper date format.
#' It changes Latitutude and Longitude to numbers
#'
#' @param x Earthquake dataset
#'
#' @return This function returns the cleaned dataset
#'
#' @examples
#' raw_data<-system.file("extdata", "signif.txt", package = "RCapstone")
#' eq_clean_data(raw_data)
#'
#' @importFrom lubridate ymd
#' @importFrom lubridate years
#'
#' @export
eq_clean_data<-function(x){
  x$DATE=lubridate::ymd(paste("0000",x$MONTH,x$DAY,sep="-"),truncated=2)+lubridate::years(x$YEAR)
  x$LATITUDE=as.numeric(x$LATITUDE)
  x$LONGITUDE=as.numeric(x$LONGITUDE)
  x
}

#' Clean Earthquake Location
#'
#' This is a simple function that cleans the Earthquake dataset.
#' It removes country from the Location Name variable.
#' If there is no location then, then just Country is supplied.
#'
#' @param x Earthquake dataset
#'
#' @return This function returns the modified dataset
#'
#' @examples
#' library(magrittr)
#' raw_data<-system.file("extdata", "signif.txt", package = "RCapstone")
#' eq_clean_data(raw_data) %>% eq_location_clean()
#'
#' @export
eq_location_clean<-function(x){
  x$LOCATION_NAME=base::gsub("^[^:]+:\\s*", "", x$LOCATION_NAME)
  x[is.na(x$LOCATION_NAME),]$LOCATION_NAME=x[is.na(x$LOCATION_NAME),]$COUNTRY
  x
}
