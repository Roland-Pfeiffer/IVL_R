library(ggplot2)
library(gridExtra)
library(lubridate)
library(stringr)


PATH_TO_KBERG_FILES <- "/media/findux/DATA/Documents/IVL/Data/Kberg_weather_data/"
SALINITY_CUTOFF_TH <- 1

replace_999_with_NaN <- function(df){
  for (i in seq_along(colnames(df))){
    df[, i][df[,i] == -999] <- NaN
    df[, i][df[,i] == 999.99] <- NaN
    }
  df
}

fnames_kberg <- list.files(PATH_TO_KBERG_FILES, pattern = ".csv$")
kberg_paths <- c()
for (fname in fnames_kberg){
  kberg_paths[length(kberg_paths) + 1] <- paste(PATH_TO_KBERG_FILES, fname, sep = "")
}

data_kberg <- data.frame()
for (fpath in kberg_paths){
  message(fpath)
  tmp <- read.csv(fpath)
  tmp <- tmp[,2:8]
  data_kberg <- rbind(data_kberg, tmp)
}

# Adjust names
names(data_kberg)[startsWith(names(data_kberg), "Local.time..CET")] <- "datetime"
names(data_kberg)[startsWith(names(data_kberg), "Salinity.1m..Sj")] <- "sal_3"
names(data_kberg)[startsWith(names(data_kberg), "Temperature.1m..Sj")] <- "temp_3"
names(data_kberg)[startsWith(names(data_kberg), "Surface.Water.Salinity..Lab")] <- "sal_7"
names(data_kberg)[startsWith(names(data_kberg), "Surface.Water.Temperature...Lab")] <- "temp_7"
names(data_kberg)[startsWith(names(data_kberg), "Deep.Water.Salinity.")] <- "sal_25"
names(data_kberg)[startsWith(names(data_kberg), "Deep.Water.Temperature.")] <- "temp_25"

# Adjust format
data_kberg$datetime <- ymd_hms(data_kberg$datetime)
data_kberg$sal_3 <- as.numeric(data_kberg$sal_3)
data_kberg$temp_3 <- as.numeric(data_kberg$temp_3)
data_kberg$sal_7 <- as.numeric(data_kberg$sal_7)
data_kberg$temp_7 <- as.numeric(data_kberg$temp_7)
data_kberg$sal_25 <- as.numeric(data_kberg$sal_25)
data_kberg$temp_25 <- as.numeric(data_kberg$temp_25)

data_kberg$sal_25[data_kberg$sal_25 < SALINITY_CUTOFF_TH] <- NaN
data_kberg$sal_7[data_kberg$sal_7 < SALINITY_CUTOFF_TH] <- NaN
data_kberg$sal_3[data_kberg$sal_3 < SALINITY_CUTOFF_TH] <- NaN

data_kberg <- replace_999_with_NaN(data_kberg)

# Create an hour-based category column for aggregating
data_kberg$aggregate_value <- as.factor(paste(year(data_kberg$datetime),
                                    str_pad(month(data_kberg$datetime), 2, pad = "0"),
                                    str_pad(day(data_kberg$datetime), 2, pad = "0"),
                                    str_pad(hour(data_kberg$datetime), 2, pad = "0"),
                                    sep = ""))

message("Aggregating...")
data_kberg <- aggregate(data_kberg, by = list(data_kberg$aggregate_value),
                        FUN = "mean", na.rm = TRUE)
message("DONE")
data_kberg <- subset(data_kberg, select = -c(Group.1))
summary(data_kberg)

# Highly specific:
data_3m <- subset(data_kberg, select = c(datetime, sal_3, temp_3))
colnames(data_3m) <- c("datetime", "salinity_PSU", "temp")
data_3m$depth_m <- 3

data_7m <- subset(data_kberg, select = c(datetime, sal_7, temp_7))
colnames(data_7m) <- c("datetime", "salinity_PSU", "temp")
data_7m$depth_m <- 7

data_25m <- subset(data_kberg, select = c(datetime, sal_25, temp_25))
colnames(data_25m) <- c("datetime", "salinity_PSU", "temp")
data_25m$depth_m <- 25

# Replace the original dataframe with one that consists of these subsets:
data_kberg <- rbind(data_3m, data_7m, data_25m)
data_kberg$depth_m <- as.factor(data_kberg$depth_m)

# Remove the outliers
# data_kberg <- data_kberg[data_kberg$salinity_PSU > 0 | is.nan(data_kberg$salinity_PSU), ]


summary(data_kberg)

xlims <- c(min(data_kberg$datetime), max(data_kberg$datetime))
temp_jetty <- ggplot(data = data_kberg[data_kberg$depth_m == "3", ], mapping = aes(x = datetime, y=temp)) +
  geom_line() + 
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  ggtitle("Jetty") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sal_jetty <- ggplot(data = data_kberg[data_kberg$depth_m == "3", ], mapping = aes(x = datetime, y=salinity_PSU)) +
  geom_line() + 
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  ggtitle("Jetty") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


temp_7 <- ggplot(data = data_kberg[data_kberg$depth_m == "7", ], mapping = aes(x = datetime, y=temp)) +
  geom_line() + 
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  ggtitle("07 m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
sal_7 <- ggplot(data = data_kberg[data_kberg$depth_m == "7", ], mapping = aes(x = datetime, y=salinity_PSU)) +
  geom_line() + 
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  ggtitle("07 m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

temp_25 <- ggplot(data = data_kberg[data_kberg$depth_m == "25", ], mapping = aes(x = datetime, y=temp)) +
  geom_line() + 
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  ggtitle("25 m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sal_25 <- ggplot(data = data_kberg[data_kberg$depth_m == "25", ], mapping = aes(x = datetime, y=salinity_PSU)) +
  geom_line() + 
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  ggtitle("25 m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


grid.arrange(temp_jetty, sal_jetty,
             temp_7, sal_7,
             temp_25, sal_25,
             ncol = 1)


# summary(data_kberg)
# time_now <- format(now(), format = "%Y-%m-%d")
# write.csv(data_kberg,
#           file = paste(SAVE_LOCATION, "/kberg_data_", time_now, ".csv", sep = ""))
# rm(list = c("PATH_TO_KBERG_FILES", "fnames_kberg", "kberg_paths", "jetty",
#             "rig07", "rig25"))
# 
# a <- read.csv("file:///media/findux/DATA/Documents/IVL/Data/minus999test.csv")
# a <- replace_999_with_NaN(a)
