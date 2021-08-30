rm(list=ls())
cat("\14")

fpath_d1 <- "/media/findux/DATA/Documents/IVL/Data/test_data_1.csv"
fpath_d2 <- "/media/findux/DATA/Documents/IVL/Data/test_data_2.csv"

data_0 <- data.frame(row.names = c("datetime", "temp", "cond", "depth"))


data_1 <- read.csv(fpath_d1)
data_1$depth <- 3

data_2 <- read.csv(fpath_d2)
data_2$depth <- 7

data <- rbind(data_0, data_1, data_2)
data

testvar <- "temp"
data_new <- data[, testvar]