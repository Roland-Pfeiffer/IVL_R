rm(list = ls())
cat("\014")

library(ggplot2)
library(gridExtra)
library(lubridate)
library(reshape2)
library(stringr)
library(tidyr)
# Sys.setlocale("LC_ALL", "C")

# ------------------------- FILE NAMES -----------------------------------------

fpath_hobo <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-09-28_22-45.csv"
fpath_kberg <- "file:///media/findux/DATA/Documents/IVL/Data/Kberg_weather_data/kberg_data_merged_2021-09-27.csv"
fpath_slaggo <- "file:///media/findux/DATA/Documents/IVL/Data/slaggo_data/slaggo_data_merged_2021-09-29.csv"
  
  
# ------------------------- KBERG ----------------------------------------------

# Load kberg data
data_kberg <- read.csv(fpath_kberg)
data_kberg$datetime <- ymd_hms(data_kberg$datetime)
data_kberg[data_kberg$depth_m == "3", "Source"] <- "K'berg 1m Sjoboden"
data_kberg[data_kberg$depth_m == "7", "Source"] <- "K'berg surf water lab"
data_kberg[data_kberg$depth_m == "25", "Source"] <- "K'berg deep water lab"


# ------------------------------- SLAGGO ---------------------------------------

# Settings
data_slaggo <- read.csv(fpath_slaggo)
data_slaggo$datetime <- ymd_hms(data_slaggo$datetime)
data_slaggo <- data_slaggo[data_slaggo$depth_m <= 30, ]

# Take avg. for 25 m:
# slaggo_fully_long <- melt(data_slaggo, id.vars = c("datetime", "depth_m"))
# slaggo_wide <- spread(slaggo_fully_long, variable+depth_m, value)
# slaggo_wide <- dcast(slaggo_fully_long, variable ~ depth_m)
# slaggo_wide <- slaggo_wide[c("datetime", "20", "30")]
# names(slaggo_wide)[names(slaggo_wide) == "20"] <- "sal_20"
# names(slaggo_wide)[names(slaggo_wide) == "30"] <- "sal_30"
# slaggo_wide$sal_25_avg <- rowMeans(slaggo_wide[c("sal_20", "sal_30")])

# Extract the salinity and average it
slaggo_sal_wide <- spread(data_slaggo[c("datetime", "sal_PSU", "depth_m")], depth_m, sal_PSU)
slaggo_sal_wide <- slaggo_sal_wide[c("datetime", "20", "30")]
names(slaggo_sal_wide)[names(slaggo_sal_wide) == "20"] <- "sal_20"
names(slaggo_sal_wide)[names(slaggo_sal_wide) == "30"] <- "sal_30"
slaggo_sal_wide$sal_25_avg <- rowMeans(slaggo_sal_wide[c("sal_20", "sal_30")])

# Extract the temperature and average it
slaggo_temp_wide <- spread(data_slaggo[c("datetime", "temp", "depth_m")], depth_m, temp)
slaggo_temp_wide <- slaggo_temp_wide[c("datetime", "20", "30")]
names(slaggo_temp_wide)[names(slaggo_temp_wide) == "20"] <- "temp_20"
names(slaggo_temp_wide)[names(slaggo_temp_wide) == "30"] <- "temp_30"
slaggo_temp_wide$temp_25_avg <- rowMeans(slaggo_temp_wide[c("temp_20", "temp_30")])

# Merge the two into one data frame, add depth info and adjust colnames
slaggo_wide <- merge(slaggo_sal_wide, slaggo_temp_wide, by = "datetime")
rm(list = c("slaggo_sal_wide", "slaggo_temp_wide"))
slaggo_wide$depth_m <- 25
names(slaggo_wide)[names(slaggo_wide) == "temp_25_avg"] <- "temp"
names(slaggo_wide)[names(slaggo_wide) == "sal_25_avg"] <- "sal_PSU"

# cleanup for rbind:
slaggo_wide <- slaggo_wide[c("datetime", "depth_m", "temp", "sal_PSU")]
slaggo_wide$chla_ugl <- as.numeric(NA)
names(data_slaggo) == names(slaggo_wide)
data_slaggo <- rbind(data_slaggo, slaggo_wide)

# Remove everything older than 2019
data_slaggo <- data_slaggo[year(data_slaggo$datetime) >= 2019, ]
data_slaggo$Source <- "Slaggo"

# TODO: Average for 7 m btw. 6 and 8


# --------------------------------- HOBO ---------------------------------------

data_hobo <- read.csv(fpath_hobo)
data_hobo$datetime <- ymd_hms(data_hobo$datetime)
data_hobo$Source <- "HOBO loggers"

# -------------------------- COMBINE DATA --------------------------------------

# merge kber & slaggo f. salinity comparison
data_sal <- rbind(data_slaggo[, -which(names(data_slaggo) == "chla_ugl")], data_kberg)
data_sal$depth_m <- as.factor(data_sal$depth_m)

# Cleanup so data is mergeable:
data_hobo <- subset(data_hobo, select = -c(year, month, day, conductivity_raw))
data_kberg <- subset(data_kberg, select = -c(sal_PSU))

# Merge data
data_all <- rbind(data_kberg, data_hobo, subset(data_slaggo, select = -c(chla_ugl, sal_PSU)))

data_all$source <- as.factor(data_all$source)
data_all$depth_m <- as.factor(data_all$depth_m)

# Change factor levels to include "m"
# for (i in seq_along(levels(data_all$depth_m))){
#   levels(data_all$depth_m)[i] <- paste(levels(data_all$depth_m)[i], "m")
# }

# ----------------------------- COMPARISON -------------------------------------


depth_values <- c(3, 7, 15, 20, 25, 30)

temp_comp <- ggplot(data = data_all[data_all$depth_m %in% depth_values, ], mapping = aes(x = datetime, y = temp, color = Source)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~data_all[data_all$depth_m %in% depth_values, "depth_m"], ncol=1) +
  ggtitle(label = "Temperature comparison btw. sites", subtitle = ("25 m data for Slaggo: avg. btw. 20 m & 30 m.")) +
  xlab("Date") + ylab("Temperature (\u00b0C)") + # Using unicode for degree symbol
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sal_comp <- ggplot(data = data_sal[data_sal$depth_m %in% depth_values, ], mapping = aes(x = datetime, y = sal_PSU, color = Source)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~depth_m, ncol = 1) +
  ggtitle(label = "Salinity comparison btw. sites", subtitle = ("25 m data for Slaggo: avg. btw. 20 m & 30 m.")) +
  xlab("Date") + ylab("Salinity (PSU)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

temp_comp
sal_comp
