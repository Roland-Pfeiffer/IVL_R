rm(list = ls())
cat("\014")

library(lubridate)
library(ggplot2)
library(ggthemes)
library(gridExtra)


# --------------------- SETTINGS & FILEPATHS -----------------------------------

fpath_merged <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-09-29_19-20.csv"
fpath_raw <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_raw_2021-09-29_19-20.csv"
fpath_slaggo <- "file:///media/findux/DATA/Documents/IVL/Data/slaggo_data/slaggo_data_merged_2021-09-29.csv"

color_palette <- c("#33CCCC", "#009999", "#006666", "#336666")
transparency_value = 0.1


# ------------------------- PREPARE DATA  ----------------------------------

data_raw <- read.csv(fpath_raw)
data_raw$datetime <- ymd_hms(data_raw$datetime)
data_raw$depth_m <- as.factor(data_raw$depth_m)

data_merged <- read.csv(fpath_merged)
data_merged$datetime <- ymd_hms(data_merged$datetime)
data_merged$depth_m <- as.factor(data_merged$depth_m)
data_merged$depth_classes_for_faceting <- factor(paste(data_merged$depth_m, "m"), levels = c("3 m", "7 m", "15 m", "25 m"))

data_slaggo <- read.csv(fpath_slaggo)


# ------------------------- TEMPERATURE PLOT  ----------------------------------


data_nofacet <- subset(data_merged, select = -c(depth_classes_for_faceting))
plot_faceted <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp)) +
  theme_bw() +
  # Map the background data in grey (actually transparent black).
  geom_line(data = data_nofacet, mapping = aes(group=depth_m), color="black", 
            alpha=transparency_value) +
  # Plot the corresponding data in color on top:
  geom_line(mapping = aes(color = depth_m)) +
  facet_grid(depth_classes_for_faceting ~ .) +
  geom_line(mapping = aes(color = depth_m)) +
  scale_colour_manual(values = color_palette) +
  ggtitle(label = "Temperature data per depth over time") +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
plot_faceted

