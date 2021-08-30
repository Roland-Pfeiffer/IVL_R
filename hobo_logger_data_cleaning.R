rm(list=ls())
cat("\014") 

# install.packages("tidyverse")
# install.packages("stringi")
library(stringr)  # Check if this also works on WIN. Otherwise use "stringi"

path_to_files <- "/media/findux/DATA/Documents/IVL/Data/HOBO logger data 2020"

extract_gmt_format <- function(df){
  for (cname in colnames(df)){
    if (startsWith(cname, "Date Time")){
      dt_match <- str_extract(cname, "GMT[+|-]\\d\\d:\\d\\d")
  }}
  return(dt_match)
}


missing_conductivity <- function(df){
  conductivity_missing <- TRUE
  for (cname in colnames(df)){
    if (startsWith(cname, "High Range")){
      conductivity_missing <- FALSE
}}}


fname_contains_depth <- function(fname) {
  re_match <- str_extract(fname, "\\d{1,2}(m|M)")  # TODO: Check if this 1,2 is still ok
  if (is.na(re_match)) {
    return(FALSE)
  } else{
    return(TRUE)
}}


extract_depth <- function(fname) {
  re_match <- str_extract(fname, "\\d{1,2}(m|M)")
  re_match_lower <- str_to_lower(re_match)
  return(re_match_lower)
}

depth_as_number <- function(depth){
  return(as.numeric(gsub("([0-9]+).*$", "\\1", depth)))
}


# Initiate data frames for each depth
data_03_m <- data.frame(row.names = c("Datetime", "Time_format", "Temp_03_m", "Raw_conductivity"))
data_07_m <- data.frame(row.names = c("Datetime", "Time_format", "Temp_07_m", "Raw_conductivity"))
data_15_m <- data.frame(row.names = c("Datetime", "Time_format", "Temp_15_m", "Raw_conductivity"))
data_25_m <- data.frame(row.names = c("Datetime", "Time_format", "Temp_25_m", "Raw_conductivity"))
data_03_m <- data.frame()
data_07_m <- data.frame()
data_15_m <- data.frame()
data_25_m <- data.frame()
data_merged <- data.frame()
skipped_files = list()

# Read filenames
fnames <- list.files(path_to_files)

# Gobble up chaotically named data:
for (fname in fnames){
  if (endsWith(fname, ".csv")){
    if (fname_contains_depth(fname)){
      depth <- extract_depth(fname)
      fpath <- paste(path_to_files, fname, sep = "/")
      message('Depth:', depth, '\tFile: ', fname)
      possible_read_error <- tryCatch(data_tmp <- read.csv(fpath, skip = 1, check.names = FALSE),
                                      error = function(e) {skipped_files <<- c(skipped_files, fname)})  # TODO: this does not work yet...
      if (!inherits(possible_read_error, "error")){  # TODO: This does not seem to work
        tformat <- extract_gmt_format(data_tmp)
        
        if (depth == "3m"){
          data_03_m <- rbind(data_03_m, data_tmp)
        } else if (depth == "7m") {
          data_07_m <- rbind(data_07_m, data_tmp)
        } else if (depth == "15m") {
          data_15_m <- rbind(data_15_m, data_tmp)
        } else if (depth == "25m") {
          data_25_m <- rbind(data_25_m, data_tmp)
        }
        
        
      }
      
    } else {
      skipped_files <- c(skipped_files, fname)
    }
  }
}



# Print skipped files if there are any:
if (length(skipped_files) > 0){
  for (fname in skipped_files){
    message("Skipped csv file: ", fname)
  }
}

# testname <- "Abdaskjhfduz2m"
# fname_contains_depth(testname)
# extract_depth(testname)
# depth_as_number(extract_depth(testname))
# str_to_lower("25M")
