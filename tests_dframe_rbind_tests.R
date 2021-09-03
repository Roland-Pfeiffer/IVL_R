rm(list=ls())
cat("\14")

fill_cond_if_missing <- function(dframe){
  # This function does not work when used in pipes, for whatever reason.
  if ("conductivity_raw" %in% colnames(dframe)){
    # (Do nothing, conductivity column already exists)
  } else {
    # Create a conductivity column filled with NA.
    dframe$conductivity_raw <- NA
  }
  dframe
}

fpath_d1 <- "/media/findux/DATA/Documents/IVL/Data/test_data_1.csv"
fpath_d2 <- "/media/findux/DATA/Documents/IVL/Data/test_data_2.csv"

data_1 <- read.csv(fpath_d1)
data_1$depth <- 3
data_1 <- fill_cond_if_missing(data_1)

data_2 <- read.csv(fpath_d2)
# data_2 <- fill_cond_if_missing(data_2)
data_2$depth <- 7

data_3 <- rbind(data_1, data_2)

data_3 <- select(data_3, temp, conductivity_raw)