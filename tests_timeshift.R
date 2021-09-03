fname_1 <- "/media/findux/DATA/Documents/IVL/Data/test_data_datetime_1.csv"
fname_2 <- "/media/findux/DATA/Documents/IVL/Data/test_data_datetime_2.csv"

data_1 <- read.csv(fname_1, check.names = FALSE)
data_2 <- read.csv(fname_2, check.names = FALSE)



data_1 <- add_gmt_offset(data_1)
data_1$tz <- "GMT+1"
data_1$data <- 1

data_2 <- add_gmt_offset(data_2)
data_2$tz <- "GMT+2"
data_2$data <- 2

colnames(data_1)[1:2] <- c("datetime", "temp")
colnames(data_2)[1:2] <- c("datetime", "temp")

data <- rbind(data_1, data_2)
data <- fm_pm_to_AM_FM(data)

data$datetime <- as.POSIXct(data$datetime,
                            format="%m/%d/%y %I:%M:%S %p")

data$gmt_correction_s <- (2 - data$gmt_offset) * 3600
data$datetime <- data$datetime + data$gmt_correction_s
