library(stringr)
library(lubridate)


fm_pm_to_AM_FM <- function(df){
  df$datetime <- str_replace(df$datetime, "em", "PM")
  df$datetime <- str_replace(df$datetime, "fm", "AM")
  return(df)
}

fname <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_data_all/20200330_Logger7m.csv"
data <- read.csv(fname, check.names = FALSE, skip = 1)
data <- data[, 2:4]
colnames(data) <- c("datetime", "cond", "temp")

data$datetime <- str_replace(data$datetime, "em", "PM")
data$datetime <- str_replace(data$datetime, "fm", "AM")

data <- fm_pm_to_AM_FM(data)

data$datetime <- mdy_hms(data$datetime)
data$datetime <- data$datetime + 20
