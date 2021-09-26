
library(ggplot2)
library(lubridate)
library(gridExtra)
library(stringr)
Sys.setlocale("LC_ALL", "C")

# Settings
DEPTH_LEVELS <- c(0, 3, 5, 8, 15, 30)
fnames_slaggo <- c("/media/findux/DATA/Documents/IVL/Data/sharkweb_data_2018-2019.txt",
                   "/media/findux/DATA/Documents/IVL/Data/sharkweb_data_2020-2021.txt")
data_slaggo <- data.frame()
for (fname in fnames_slaggo){
  data_slaggo <- rbind(data_slaggo, read.csv(fname, sep = "\t"))
}

# Rename columns
names(data_slaggo)[names(data_slaggo) == "Provtagningsdjup..m."] <- "depth_m"
names(data_slaggo)[names(data_slaggo) == "Temperatur.CTD..C."] <- "temp_C"
names(data_slaggo)[names(data_slaggo) == "Salinitet.CTD..o.oo.psu."] <- "sal_PSU"
names(data_slaggo)[names(data_slaggo) == "Klorofyll.a.vattenh.mtare..ug.l."] <- "chla_ugl"
names(data_slaggo)[names(data_slaggo) == "Provtagningsdatum"] <- "date"
names(data_slaggo)[names(data_slaggo) == "Provtagningstid..start."] <- "time"

# Add timestamp
data_slaggo$timestamp <- paste(data_slaggo$date, data_slaggo$time)
# Select only relevant columns
data_slaggo <- subset(data_slaggo, select = c(timestamp, depth_m, temp_C, sal_PSU, chla_ugl))
# Use datetime format (POSIXct) for timestamp
data_slaggo$timestamp <- ymd_hm(data_slaggo$timestamp)

# Select specific upper depths:
data_slaggo <- data_slaggo[data_slaggo$depth_m %in% DEPTH_LEVELS, ]

data_slaggo$depth_m <- as.factor(data_slaggo$depth_m)

data_slaggo$temp_C <- as.numeric(str_replace(data_slaggo$temp_C, ",", "."))
data_slaggo$sal_PSU <- as.numeric(str_replace(data_slaggo$sal_PSU, ",", "."))
data_slaggo$chla_ugl <- as.numeric(str_replace(data_slaggo$chla_ugl, ",", "."))




plot_temp <- ggplot(data=data_slaggo,
                    mapping = aes(x = timestamp, y = temp_C, color = depth_m)) +
  geom_line()

plot_sal <- ggplot(data = data_slaggo,
                   mapping = aes(x=timestamp, y=sal_PSU, color = depth_m)) +
  geom_line()

plot_temp
plot_sal

grid.arrange(plot_temp, plot_sal, ncol = 1)
