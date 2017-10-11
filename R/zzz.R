#R CMD check causes problems with these variables so
if(getRversion() >= "2.15.1"){utils::globalVariables(c("y","LOCATION_NAME","EQ_PRIMARY","TOTAL_DEATHS"))}
