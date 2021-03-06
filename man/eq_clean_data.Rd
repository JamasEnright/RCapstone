% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq_clean.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Clean Earthquake Data}
\usage{
eq_clean_data(x)
}
\arguments{
\item{x}{Earthquake dataset}
}
\value{
This function returns the cleaned dataset
}
\description{
This is a simple function that cleans the Earthquake dataset.
It maps date to a proper date format.
It changes Latitutude and Longitude to numbers
}
\examples{
library(readr)
raw_data<-readr::read_tsv(system.file("extdata", "signif.txt", package = "RCapstone"))
eq_clean_data(raw_data)

}
