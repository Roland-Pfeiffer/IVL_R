library(lubridate)
library(stringr)

fpath <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_data_all/20190604_Logger25m.csv"

data <- read.csv(fpath, skip = 1, check.names = FALSE)

names(data)[startsWith(names(data), "Date Time")] <- "datetime"
names(data)[startsWith(names(data), "Temp, ")] <- "temp"

data <- data[c("datetime", "temp")]

data$datetime <- str_replace(data$datetime, "em", "PM")
data$datetime <- str_replace(data$datetime, "fm", "AM")
data$datetime <- mdy_hms(data$datetime)

data[data$datetime >= as.POSIXct("2019-05-01 00:00:00", tz = "UTC"), ][1:5,]
