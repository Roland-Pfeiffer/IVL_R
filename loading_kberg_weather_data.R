library(lubridate)
library(gridExtra)


PATH_TO_KBERG_FILES <- "/media/findux/DATA/Documents/IVL/Data/Kberg_weather_data/"

remove_999 <- function(df){
  for (i in seq_along(colnames(df))){
    df[, i][df[,i] == -999] <- NaN
    df[, i][df[,i] == 999.99] <- NaN
    # df[df[,i] == -999,] <- NA
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


colnames(data_kberg) <- str_replace_all(colnames(data_kberg), "\\.+", "")

data_kberg$LocaltimeCET <- ymd_hms(data_kberg$LocaltimeCET)
data_kberg$Salinity1mSjöbodenPSU <- as.numeric(data_kberg$Salinity1mSjöbodenPSU)
data_kberg$Temperature1mSjöbodenC <- as.numeric(data_kberg$Temperature1mSjöbodenC)
data_kberg$DeepWaterSalinityLabPSU <- as.numeric(data_kberg$DeepWaterSalinityLabPSU)
data_kberg$DeepWaterTemperatureLabC <- as.numeric(data_kberg$DeepWaterTemperatureLabC)
data_kberg$SurfaceWaterSalinityLabPSU <- as.numeric(data_kberg$SurfaceWaterSalinityLabPSU)
data_kberg$SurfaceWaterTemperatureLabC <- as.numeric(data_kberg$SurfaceWaterTemperatureLabC)

data_kberg <- remove_999(data_kberg)


# Create category column for aggregating
data_kberg$aggregate_value <- as.factor(paste(year(data_kberg$LocaltimeCET),
                                    str_pad(month(data_kberg$LocaltimeCET), 2, pad = "0"),
                                    str_pad(day(data_kberg$LocaltimeCET), 2, pad = "0"),
                                    str_pad(hour(data_kberg$LocaltimeCET), 2, pad = "0"),
                                    sep = ""))

message("Aggregating...")
data_kberg <- aggregate(data_kberg, by = list(data_kberg$aggregate_value),
                        FUN = "mean", na.rm = TRUE)
data_kberg <- subset(data_kberg, select = -c(Group.1))
names(data_kberg)[names(data_kberg) == 'LocaltimeCET'] <- 'datetime'

# Highly specific:
jetty <- subset(data_kberg, select = c(datetime, Salinity1mSjöbodenPSU, Temperature1mSjöbodenC))
colnames(jetty) <- c("datetime", "salinity_PSU", "temp_C")
jetty$depth_m <- "jetty"
summary(jetty)

rig07 <- subset(data_kberg, select = c(datetime, SurfaceWaterSalinityLabPSU, SurfaceWaterTemperatureLabC))
colnames(rig07) <- c("datetime", "salinity_PSU", "temp_C")
rig07$depth_m <- 7
summary(rig07)

rig25 <- subset(data_kberg, select = c(datetime, DeepWaterSalinityLabPSU, DeepWaterTemperatureLabC))
colnames(rig25) <- c("datetime", "salinity_PSU", "temp_C")
rig25$depth_m <- 25
summary(rig25)

data_kberg <- rbind(jetty, rig07, rig25)
data_kberg$depth_m <- as.factor(data_kberg$depth_m)

xlims <- c(min(data_kberg$datetime), max(data_kberg$datetime))
temp_jetty <- ggplot(data = data_kberg[data_kberg$depth_m == "jetty", ], mapping = aes(x = datetime, y=temp_C)) +
  geom_line() + 
  xlim(xlims) +
  ggtitle("Jetty")
temp_7 <- ggplot(data = data_kberg[data_kberg$depth_m == "7", ], mapping = aes(x = datetime, y=temp_C)) +
  geom_line() + 
  xlim(xlims) + 
  ggtitle("07 m")
temp_25 <- ggplot(data = data_kberg[data_kberg$depth_m == "25", ], mapping = aes(x = datetime, y=temp_C)) +
  geom_line() + 
  xlim(xlims) +
  ggtitle("25 m")

sal_jetty <- ggplot(data = data_kberg[data_kberg$depth_m == "jetty", ], mapping = aes(x = datetime, y=salinity_PSU)) +
  geom_line() + 
  xlim(xlims) +
  ggtitle("Jetty")

sal_7 <- ggplot(data = data_kberg[data_kberg$depth_m == "7", ], mapping = aes(x = datetime, y=salinity_PSU)) +
  geom_line() + 
  xlim(xlims) +
  ggtitle("07 m")

sal_25 <- ggplot(data = data_kberg[data_kberg$depth_m == "25", ], mapping = aes(x = datetime, y=salinity_PSU)) +
  geom_line() + 
  xlim(xlims) +
  ggtitle("25 m")

grid.arrange(temp_jetty, sal_jetty,
             temp_7, sal_7,
             temp_25, sal_25,
             ncol = 1)


summary(data_kberg)
time_now <- format(now(), format = "%Y-%m-%d")
write.csv(data_kberg,
          file = paste(SAVE_LOCATION, "/kberg_data_", time_now, ".csv", sep = ""))
rm(list = c("PATH_TO_KBERG_FILES", "fnames_kberg", "kberg_paths", "jetty",
            "rig07", "rig25"))

a <- read.csv("file:///media/findux/DATA/Documents/IVL/Data/minus999test.csv")
a <- minus_999_to_NA(a)
