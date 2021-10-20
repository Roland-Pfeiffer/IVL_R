cat("\014")
rm(list = ls())

library(purrr)
library(tidyr)

FPATH <- "file:///media/findux/DATA/Documents/IVL/heat_experiment/heat_treatment_data_2021-10-20.csv"
SAVE_DIR <- "/media/findux/DATA/Documents/IVL/heat_experiment/"
fname <- tail(strsplit(FPATH, "/")[[1]], n=1) # tail(..., n=1) is what [-1] is in python
save_name <- paste(SAVE_DIR, "processed_", fname, sep = "")

if (! "temp_change_logge" %in% names(data)){
  message("Calculating changes and avg. change.")
  data$temp_change_ysy <- data$temp_c_measured_stop_ysy - data$temp_c_measured_start_ysy
  data$temp_change_logger <- data$temp_c_measured_stop_logger - data$temp_c_measured_start_logger
  data$temp_change_avg <- apply(data[, c("temp_change_logger", "temp_change_ysy")], FUN=mean, MARGIN = 1)
}

write.csv(data, save_name, row.names = FALSE)