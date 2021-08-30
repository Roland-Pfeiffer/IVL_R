cat("\14")
rm(list=ls())
library(stringr)

# NOTES
# Use set() for the column names and check if they are similar.
# Maybe add an option to add 

extract_gmt_format <- function(input_str){
  dt_match <- str_extract(input_str, "GMT[+|-]\\d\\d:\\d\\d")
  return(dt_match)
}

gmt_hrs <- function(dtime_str){
  gmt_match <- extract_gmt_format(dtime_str)
  prefix <- substr(gmt_match, 4, 4) # Extract the fourth character (+ or -)
  hours <- as.numeric(substr(gmt_match, 5, 6)) # Extract the hour 
  if (prefix == "+"){
    multiplicator <- 1
  } else {
    multiplicator <- -1
  }
  return(hours * multiplicator)
}


adjust_datetime <- function(df) {
  for (cname in colnames(df)){
    if (startsWith(cname, "Date Time")) {
      time_format <- extract_gmt_format(cname)
      message("Encountered time format: ", time_format)
}}}

fname <-"/media/findux/DATA/Documents/IVL/Data/HOBO logger data 2020/25M_Temp_logger_20210324.csv"
data <- read.csv(fname, skip = 1, check.names = FALSE)

test_dtf <- colnames(data)[2]
dt_match <- str_extract(test_dtf, "GMT[+|-]\\d\\d:\\d\\d")
dt_match <- "GMT-02:00"
prefix <- substr(dt_match, 4, 4) # Select the fourth character

gmt_hrs(dt_match)
adjust_datetime(data)
