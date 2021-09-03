rm(list=ls())
cat("\14")
library(tidyverse)

fill_cond_if_missing <- function(dframe){
  if ("conductivity_raw" %in% colnames(dframe)){
    # (Do nothing, conductivity column already exists)
  } else {
    # Create a conductivity column filled with NA.
    message('No conductivity data.')
    dframe$conductivity_raw <- NA
  }
  dframe
}

fpath_d1 <- "/media/findux/DATA/Documents/IVL/Data/test_data_1.csv"
fpath_d2 <- "/media/findux/DATA/Documents/IVL/Data/test_data_2.csv"

data_1 <- read.csv(fpath_d1)
data_2 <- read.csv(fpath_d2)

cnames <- colnames(data_1)
! "conductivity_raw" %in% cnames

cnames <- colnames(data_2)
! "conductivity_raw" %in% cnames

data_1 %>%
  fill_cond_if_missing() 
