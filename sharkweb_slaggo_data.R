cat("\014")
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(lubridate)
library(reshape2)
library(stringr)
library(tidyr)

Sys.setlocale("LC_ALL", "C")

# Settings
tstamp <- as.character(format(now(), format = "%Y-%m-%d"))

FPATH_SLAGGO_DATA <- "/media/findux/DATA/Documents/IVL/Data/slaggo_data/originals/"
SAVE_PATH <- "/media/findux/DATA/Documents/IVL/Data/slaggo_data/"

fnames_slaggo <- list.files(FPATH_SLAGGO_DATA)
data_slaggo <- data.frame()
for (fname in fnames_slaggo){
  fpath <- paste(FPATH_SLAGGO_DATA, fname, sep = "/")
  data_slaggo <- rbind(data_slaggo, read.csv(fpath, sep = "\t"))
}

# Rename columns
names(data_slaggo)[startsWith(names(data_slaggo), "Provtagningsdjup")] <- "depth_m"
names(data_slaggo)[startsWith(names(data_slaggo), "Temperatur.CTD")] <- "temp"
names(data_slaggo)[startsWith(names(data_slaggo), "Salinitet.CTD")] <- "sal_PSU"
names(data_slaggo)[startsWith(names(data_slaggo), "Klorofyll")] <- "chla_ugl"
names(data_slaggo)[startsWith(names(data_slaggo), "Provtagningsdatum")] <- "date"
names(data_slaggo)[names(data_slaggo) == "Provtagningstid..start."] <- "time"

# Drop irrelevant columns
data_slaggo <- data_slaggo[c("date", "time", "depth_m", "temp", "sal_PSU", "chla_ugl")]

# Add timestamp
data_slaggo$datetime <- paste(data_slaggo$date, data_slaggo$time)
data_slaggo <- subset(data_slaggo, select = c(datetime, depth_m, temp, sal_PSU, chla_ugl))
data_slaggo$datetime <- ymd_hm(data_slaggo$datetime)

# Correct data types
data_slaggo$depth_m <- as.factor(data_slaggo$depth_m)
data_slaggo$temp <- as.numeric(str_replace(data_slaggo$temp, ",", "."))
data_slaggo$sal_PSU <- as.numeric(str_replace(data_slaggo$sal_PSU, ",", "."))
data_slaggo$chla_ugl <- as.numeric(str_replace(data_slaggo$chla_ugl, ",", "."))

data_3m <- data_slaggo[data_slaggo$depth_m == 3, ]
data_3m <- data_3m[!is.na(data_3m$sal_PSU), ]



# ----------------------- AVERAGE 7 AND 25 -------------------------------------


# Extract the salinity and average it
slaggo_sal_wide <- spread(data_slaggo[c("datetime", "sal_PSU", "depth_m")], depth_m, sal_PSU)
slaggo_sal_wide <- slaggo_sal_wide[c("datetime", "6", "8", "20", "30")]
names(slaggo_sal_wide)[which(names(slaggo_sal_wide) == "6")] <- "sal_6"
names(slaggo_sal_wide)[which(names(slaggo_sal_wide) == "8")] <- "sal_8"
names(slaggo_sal_wide)[which(names(slaggo_sal_wide) == "20")] <- "sal_20"
names(slaggo_sal_wide)[which(names(slaggo_sal_wide) == "30")] <- "sal_30"
slaggo_sal_wide$sal_avg_25 <- rowMeans(slaggo_sal_wide[c("sal_20", "sal_30")])
slaggo_sal_wide$sal_avg_7 <- rowMeans(slaggo_sal_wide[c("sal_6", "sal_8")])

# Extract the temperature and average it
slaggo_temp_wide <- spread(data_slaggo[c("datetime", "temp", "depth_m")], depth_m, temp)
slaggo_temp_wide <- slaggo_temp_wide[c("datetime", "6", "8", "20", "30")]
names(slaggo_temp_wide)[which(names(slaggo_temp_wide) == "6")] <- "temp_6"
names(slaggo_temp_wide)[which(names(slaggo_temp_wide) == "8")] <- "temp_8"
names(slaggo_temp_wide)[which(names(slaggo_temp_wide) == "20")] <- "temp_20"
names(slaggo_temp_wide)[which(names(slaggo_temp_wide) == "30")] <- "temp_30"
slaggo_temp_wide$temp_avg_25 <- rowMeans(slaggo_temp_wide[c("temp_20", "temp_30")])
slaggo_temp_wide$temp_avg_7 <- rowMeans(slaggo_temp_wide[c("temp_6", "temp_8")])


# Merge the two into one data frame, add depth info and adjust colnames
slaggo_wide <- merge(slaggo_sal_wide, slaggo_temp_wide, by = "datetime")

# missing_vals_for_7m_avg <- is.na(slaggo_wide$sal_6) & is.na(slaggo_wide$sal_8)
# sum(missing_vals_for_7m_avg)

# Separate 25 m averages
slaggo_25 <- slaggo_wide[c("datetime", "temp_avg_25", "sal_avg_25")]
names(slaggo_25)[names(slaggo_25) == "temp_avg_25"] <- "temp"
names(slaggo_25)[names(slaggo_25) == "sal_avg_25"] <- "sal_PSU"
slaggo_25$depth_m <- as.factor(25)
slaggo_25$chla_ugl <- as.numeric(NA)

# Separate 7 m averages
slaggo_7 <- slaggo_wide[c("datetime", "temp_avg_7", "sal_avg_7")]
names(slaggo_7)[which(names(slaggo_7) == "temp_avg_7")] <- "temp"
names(slaggo_7)[which(names(slaggo_7) == "sal_avg_7")] <- "sal_PSU"
slaggo_7$depth_m <- as.factor(7)
slaggo_7$chla_ugl <- as.numeric(NA)


# Add averaged data to the dataset:
names(data_slaggo)
names(slaggo_7)
names(slaggo_25)
data_slaggo <- rbind(data_slaggo, slaggo_7, slaggo_25)

# Remove everything older than 2019
data_slaggo <- data_slaggo[year(data_slaggo$datetime) >= 2019, ]

# Drop rows where temp & sal == NA
data_slaggo <- data_slaggo[!is.na(data_slaggo$temp) & !is.na(data_slaggo$sal_PSU),]


fpath_out <- paste(SAVE_PATH, "slaggo_data_merged_", tstamp, ".csv", sep = "")
write.csv(data_slaggo, fpath_out, row.names = FALSE)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

