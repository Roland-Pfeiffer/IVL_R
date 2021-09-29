
library(ggplot2)
library(lubridate)
library(gridExtra)
library(stringr)
Sys.setlocale("LC_ALL", "C")

# Settings
tstamp <- as.character(format(now(), format = "%Y-%m-%d"))
DEPTH_LEVELS <- c(0, 3, 5, 8, 15, 30)

FPATH_SLAGGO_DATA <- "/media/findux/DATA/Documents/IVL/Data/slaggo_data/originals/"
SAVE_PATH <- "/media/findux/DATA/Documents/IVL/Data/slaggo_data/"

fnames_slaggo <- list.files(FPATH_SLAGGO_DATA)
data_slaggo <- data.frame()
for (fname in fnames_slaggo){
  fpath <- paste(FPATH_SLAGGO_DATA, fname, sep = "/")
  data_slaggo <- rbind(data_slaggo, read.csv(fpath, sep = "\t"))
}

# Rename columns
names(data_slaggo)[names(data_slaggo) == "Provtagningsdjup..m."] <- "depth_m"
names(data_slaggo)[names(data_slaggo) == "Temperatur.CTD..C."] <- "temp"
names(data_slaggo)[names(data_slaggo) == "Salinitet.CTD..o.oo.psu."] <- "sal_PSU"
names(data_slaggo)[names(data_slaggo) == "Klorofyll.a.vattenh.mtare..ug.l."] <- "chla_ugl"
names(data_slaggo)[names(data_slaggo) == "Provtagningsdatum"] <- "date"
names(data_slaggo)[names(data_slaggo) == "Provtagningstid..start."] <- "time"

# Add timestamp
data_slaggo$datetime <- paste(data_slaggo$date, data_slaggo$time)
# Select only relevant columns
data_slaggo <- subset(data_slaggo, select = c(datetime, depth_m, temp, sal_PSU, chla_ugl))
# Use datetime format (POSIXct) for timestamp
data_slaggo$datetime <- ymd_hm(data_slaggo$datetime)

# Select specific upper depths:
# data_slaggo <- data_slaggo[data_slaggo$depth_m %in% DEPTH_LEVELS, ]

data_slaggo$depth_m <- as.factor(data_slaggo$depth_m)

data_slaggo$temp <- as.numeric(str_replace(data_slaggo$temp, ",", "."))
data_slaggo$sal_PSU <- as.numeric(str_replace(data_slaggo$sal_PSU, ",", "."))
data_slaggo$chla_ugl <- as.numeric(str_replace(data_slaggo$chla_ugl, ",", "."))


plot_temp <- ggplot(data=data_slaggo, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line()
plot_sal <- ggplot(data = data_slaggo, mapping = aes(x=datetime, y=sal_PSU, color = depth_m)) +
  geom_line()
grid.arrange(plot_temp, plot_sal, ncol = 1)


fpath_out <- paste(SAVE_PATH, "slaggo_data_merged_", tstamp, ".csv", sep = "")
write.csv(data_slaggo, fpath_out, row.names = FALSE)
