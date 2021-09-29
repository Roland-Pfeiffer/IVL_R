cat("\014")
rm(list = ls())

library(lubridate)
library(stringr)

fpath_d1 <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_data_all/20190827_Logger15m.csv"
d1 <- read.csv(fpath_d1, skip = 1, check.names = FALSE)

names(d1)[startsWith(names(d1), "Date Time")] <- "datetime"
names(d1)[startsWith(names(d1), "High Range")] <- "conductivity_raw"
names(d1)[startsWith(names(d1), "Temp, ")] <- "temp"
d1$depth_m <- 15
d1$depth_m <- as.factor(d1$depth_m)
d1 <- d1[c("datetime", "conductivity_raw", "temp", "depth_m")]

fpath_d2 <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_data_all/20200903_Logger25m.csv"
d2 <- read.csv(fpath_d2, skip = 1, check.names = FALSE)
names(d2)[startsWith(names(d2), "Date Time")] <- "datetime"
names(d2)[startsWith(names(d2), "High Range")] <- "conductivity_raw"
names(d2)[startsWith(names(d2), "Temp, ")] <- "temp"
d2$depth_m <- 25
d2$depth_m <- as.factor(d2$depth_m)
d2 <- d2[c("datetime", "conductivity_raw", "temp", "depth_m")]

data_merged <- rbind(d1, d2)
data_merged$datetime <- str_replace(data_merged$datetime, "em", "PM")
data_merged$datetime <- str_replace(data_merged$datetime, "fm", "AM")
data_merged$datetime <- mdy_hms(data_merged$datetime)

data_merged$year <- year(data_merged$datetime)
data_merged$month <- month(data_merged$datetime)
data_merged$day <- day(data_merged$datetime)
data_merged$hour <- hour(data_merged$datetime)
data_merged$aggregate_category <- data_merged$year * 1000000 + 
  data_merged$month * 10000 + 
  data_merged$day * 100 + 
  (data_merged$hour %/% 3)


data_merged <- aggregate(data_merged, by = list(data_merged$aggregate_category,
                                                data_merged$depth_m), FUN = "mean", na.rm = TRUE)
