rm(list=ls())
cat("\014") 

# install.packages("tidyverse")
# install.packages("stringi")
library(stringr)  # Check if this also works on WIN. Otherwise use "stringi"

path_to_files <- "/media/findux/DATA/Documents/IVL/Data/HOBO logger data 2020"


fname_correct <- function(fname) {
  re_match <- str_extract(fname, "^\\d{8}_\\d{2}(\\w|\\W)_")
  if (is.na(re_match)) {
    return(FALSE)
  } else{
    return(TRUE)
  }
}

fname_contains_depth <- function(fname) {
  re_match <- str_extract(fname, "\\d{2}(m|M)_")
  if (is.na(re_match)) {
    return(FALSE)
  } else{
    return(TRUE)
  }
}



# Read filenames
fnames <- list.files(path_to_files)
skipped_files <- list()

for (fname in fnames){
  if (endsWith(fname, ".csv")){
    if (fname_correct(fname)){
      depth <- as.character(fname)[10:11]
      message("Processing: ", fname)
      fpath <- paste(path_to_files, fname, sep="/")
      data <- read.csv(fpath, skip = 1)  # TODO: Implement a try-catch for bad data.
      depth <- substr(fname, 10, 11)
      
      # TODO: processing
      
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


