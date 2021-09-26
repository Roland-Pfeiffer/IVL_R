library(ggplot2)
library(stringr)
library(lubridate)
library(reshape)
Sys.setlocale("LC_ALL", "C")

# ------------------------- FILE NAMES -----------------------------------------

fnames_kberg <- c("/media/findux/DATA/Documents/IVL/Data/Kberg_weather_data/kristineberg_weather_data_2019.csv",
                  "/media/findux/DATA/Documents/IVL/Data/Kberg_weather_data/kristineberg_weather_data_2020.csv",
                  "/media/findux/DATA/Documents/IVL/Data/Kberg_weather_data/kristineberg_weather_data_2021-09-07.csv")
fnames_slaggo <- c("/media/findux/DATA/Documents/IVL/Data/sharkweb_data_2018-2019.txt",
                   "/media/findux/DATA/Documents/IVL/Data/sharkweb_data_2020-2021.txt")
fname_hobo <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-09-26_18-39.csv"

# ------------------------- KBERG --------_--------------------------------------

# Load kberg data
data_kberg <- data.frame()
for (fname in fnames_kberg){
  data_kberg <- rbind(read.csv(fname), data_kberg)
}

names(data_kberg)[names(data_kberg) == "Local.time..CET."] <- "datetime"
names(data_kberg)[names(data_kberg) == "Temperature.1m..Sj..boden.....C."] <- "temp_03"
names(data_kberg)[names(data_kberg) == "Salinity.1m..Sj..boden...PSU."] <- "sal_PSU_03"
names(data_kberg)[names(data_kberg) == "Deep.Water.Salinity..Lab...PSU."] <- "sal_PSU_25"
names(data_kberg)[names(data_kberg) == "Deep.Water.Temperature..Lab.....C."] <- "temp_25"
names(data_kberg)[names(data_kberg) == "Surface.Water.Salinity..Lab...PSU."] <- "sal_PSU_07"
names(data_kberg)[names(data_kberg) == "Surface.Water.Temperature...Lab.....C."] <- "temp_07"

data_kberg <- subset(data_kberg, select = c(datetime, temp_03, temp_07, temp_25,
                                            sal_PSU_03, sal_PSU_07, sal_PSU_25))
data_kberg <-  melt(data_kberg, id=c("datetime"))
data_kberg$variable <- as.character(data_kberg$variable)
data_kberg$value <- as.numeric(data_kberg$value)
data_kberg$sal[startsWith(data_kberg$variable, "sal")] <- data_kberg$value[startsWith(data_kberg$variable, "sal")]
data_kberg$temp[startsWith(data_kberg$variable, "temp")] <- data_kberg$value[startsWith(data_kberg$variable, "temp")]
data_kberg$depth_m <- str_extract(data_kberg$variable, "\\d{1,2}")
data_kberg$depth_m <- as.factor(as.numeric(data_kberg$depth_m))

# Drop where sal & temp == NA:
data_kberg <- data_kberg[!(is.na(data_kberg$sal) & is.na(data_kberg$temp)), ]

data_kberg$datetime <- ymd_hms(data_kberg$datetime)

data_kberg <- subset(data_kberg, select = -c(value, variable))

# ------------------------------- SLÄGGÖ ---------------------------------------

# Settings
DEPTH_LEVELS <- c(0, 3, 5, 8, 15, 30)
data_slaggo <- data.frame()
for (fname in fnames_slaggo){
  data_slaggo <- rbind(data_slaggo, read.csv(fname, sep = "\t"))
}

# Rename columns
names(data_slaggo)[names(data_slaggo) == "Provtagningsdjup..m."] <- "depth_m"
names(data_slaggo)[names(data_slaggo) == "Temperatur.CTD..C."] <- "temp"
names(data_slaggo)[names(data_slaggo) == "Salinitet.CTD..o.oo.psu."] <- "sal"
names(data_slaggo)[names(data_slaggo) == "Klorofyll.a.vattenh.mtare..ug.l."] <- "chla"
names(data_slaggo)[names(data_slaggo) == "Provtagningsdatum"] <- "date"
names(data_slaggo)[names(data_slaggo) == "Provtagningstid..start."] <- "time"

# Add timestamp
data_slaggo$datetime <- paste(data_slaggo$date, data_slaggo$time)
# Select only relevant columns
data_slaggo <- subset(data_slaggo, select = c(datetime, depth_m, temp, sal, chla))
# Use datetime format (POSIXct) for timestamp
data_slaggo$datetime <- ymd_hm(data_slaggo$datetime)

# --------------------------------- HOBO ---------------------------------------

data_hobo <- read.csv(fname_hobo)
data_hobo$datetime <- ymd_hms(data_hobo$datetime)

# ----------------------------- COMPARISON -------------------------------------

xmin <- min(min(data_kberg$datetime), min(data_slaggo$datetime), min(data_hobo$datetime))
xmax <- max(c(max(data_kberg$datetime), max(data_slaggo$datetime), max(data_hobo$datetime)))
xlims <- c(xmin, xmax)

# Generate plots
plot_kberg <- ggplot(data = data_kberg, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line()
plot_kberg

summary(data_kberg)
