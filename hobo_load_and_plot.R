rm(list = ls())
cat("\014")

library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lubridate)

stringi::stri_locale_list()[order(stringi::stri_locale_list())]
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

# --------------------- SETTINGS & FILEPATHS -----------------------------------

fpath_merged <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-10-01_16-48.csv"
fpath_raw <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_raw_2021-10-01_16-48.csv"
fpath_slaggo <- "file:///media/findux/DATA/Documents/IVL/Data/slaggo_data/Slaggo_re-merged_2021-10-02_02-32.csv"

COLOR_PALETTE <- c("#33CCCC", "#009999", "#006666", "#336666")
TRANSPARENCY_VALUE = 0.15


# ------------------------- PREPARE DATA  ----------------------------------


data_merged <- read.csv(fpath_merged)
data_merged$datetime <- ymd_hms(data_merged$datetime)
data_merged$depth_m <- as.factor(data_merged$depth_m)
data_merged$facet_groups <- factor(paste(data_merged$depth_m, "m"),
                                                 levels = c("3 m", "7 m", "15 m", "25 m"))


data_raw <- read.csv(fpath_raw) %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(depth_m = as.factor(depth_m))

slaggo_data <- read.csv(fpath_slaggo) %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(depth_m = as.factor(depth_m))
slaggo_data <- slaggo_data[slaggo_data$depth_m %in% c(3, 7, 15, 25), ]
slaggo_data$depth_m <- droplevels(slaggo_data$depth_m)

slaggo_data$facet_groups <- factor(paste(slaggo_data$depth_m, "m"),
                                   levels = c("3 m", "7 m", "15 m", "25 m"))
# Only select relevant depths:




# ------------------------- TEMPERATURE PLOT  ----------------------------------

# Create a dataset that does not have the facet categories so it will show up in
# all plots:
data_nofacet <- subset(data_merged, select = -c(facet_groups))
# Create a plot that will be faceted by depth category
temp_plot_faceted <- ggplot(data = data_merged, mapping = aes(x = datetime,
                                                              y = temp)) +
  theme_bw() +
  # Map the background data in grey (actually transparent black).
  geom_line(data = data_nofacet, mapping = aes(group=depth_m), color="black", 
            alpha=TRANSPARENCY_VALUE) +
  # Plot the corresponding data in color on top:
  facet_grid(facet_groups ~ .) +
  geom_line(mapping = aes(color = depth_m)) +
  scale_colour_manual(values = COLOR_PALETTE) +
  ggtitle(label = "Temperature data per depth over time",
          subtitle = "Source: Hobo loggers.") +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b.'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color=guide_legend(title="Depth (m)")) # Customize legend title
temp_plot_faceted


# ---------------------------- SALINITY PLOT  ----------------------------------

slaggo_nofacet <- subset(slaggo_data, select = -c(facet_groups))
sal_plot_faceted <- ggplot(data = slaggo_data, mapping = aes(x = datetime,
                                                             y = sal_PSU)) +
  theme_bw() +
  # Map the background data in grey (actually transparent black).
  geom_line(data = slaggo_nofacet, mapping = aes(group=depth_m), color="black", 
            alpha=TRANSPARENCY_VALUE) +
  # Plot the corresponding data in color on top:
  facet_grid(facet_groups ~ .) +
  geom_line(mapping = aes(color = depth_m)) +
  scale_colour_manual(values = COLOR_PALETTE) +
  ggtitle(label = "Salinity data per depth over time",
          subtitle = "Source: Measurements at Släggö.") +
  xlab("Date") + ylab("Salinity (PSU)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b.'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(color=guide_legend(title="Depth (m)")) # Customize legend title
sal_plot_faceted
