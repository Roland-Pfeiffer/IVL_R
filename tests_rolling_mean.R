cat("\014")

library(dplyr)
library(lubridate)
library(zoo)

fname <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-09-29_19-20.csv"
data <- read.csv(fname) %>% 
  mutate(datetime = ymd_hms(datetime)) %>%
  mutate(depth_m = as.factor(depth_m)) %>%
  group_by(depth_m) %>%
  mutate(rolling_avg = rollmean(temp, k=3, fill = NA)) %>%
  ungroup()

data_merged <- data

data_merged$facet_groups <- factor(paste(data_merged$depth_m, "m"),
                                   levels = c("3 m", "7 m", "15 m", "25 m"))
COLOR_PALETTE <- c("#33CCCC", "#009999", "#006666", "#336666")

# Create a dataset that does not have the facet categories so it will show up in
# all plots:
data_nofacet <- subset(data_merged, select = -c(facet_groups))
# Create a plot that will be faceted by depth category
temp_plot_faceted <- ggplot(data = data_merged, mapping = aes(x = datetime, y = rolling_avg)) +
  theme_bw() +
  # Map the background data in grey (actually transparent black).
  geom_line(data = data_nofacet, mapping = aes(group=depth_m), color="black", 
            alpha=0.1) +
  # Plot the corresponding data in color on top:
  facet_grid(facet_groups ~ .) +
  geom_line(mapping = aes(color = depth_m)) +
  scale_colour_manual(values = COLOR_PALETTE) +
  ggtitle(label = "Temperature data per depth over time", subtitle = "Source: Hobo loggers.") +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
temp_plot_faceted
