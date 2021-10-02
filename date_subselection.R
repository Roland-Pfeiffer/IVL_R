cat("\014")
rm(list = ls())

library(dplyr)
library(lubridate)

fpath_wide <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_wide_2021-10-01_16-48.csv"

wide_data <- read.csv(fpath_wide) %>%
  subset(select = c(datetime, temp_3m, temp_7m, temp_15m, temp_25m)) %>%
  dplyr::mutate(datetime = ymd_hms(datetime))


# Note that the original data has been aggregated, so there is not a data point
# Available for every hour.
asas_data <- wide_data[wide_data$datetime >= as.POSIXct("2020-09-22 23:59:00", tz = "GMT") &
                                     wide_data$datetime < as.POSIXct("2020-10-06 00:00:00", tz = "GMT"), ]
asas_data <- asas_data[c("datetime", "temp_3m", "temp_25m")]
asas_data <- asas_data[!(is.na(asas_data$temp_25m) & is.na(asas_data$temp_3m)),]
fpath_out <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/data_2020-09-23_to2020-10-05.csv"

write.csv(asas_data, fpath_out, row.names = FALSE)