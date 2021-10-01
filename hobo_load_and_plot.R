rm(list = ls())
cat("\014")

library(lubridate)
library(ggplot2)
library(ggthemes)
library(gridExtra)

fpath_merged <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-09-29_19-20.csv"
fpath_raw <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_raw_2021-09-29_19-20.csv"

data_raw <-read.csv(fpath_raw)
data_raw$datetime <- ymd_hms(data_raw$datetime)
data_raw$depth_m <- as.factor(data_raw$depth_m)

data_merged <- read.csv(fpath_merged)
data_merged$datetime <- ymd_hms(data_merged$datetime)
data_merged$depth_m <- as.factor(data_merged$depth_m)

xlims <- c(min(data_merged$datetime), max(data_merged$datetime))
# Colors:
C03 <- "#33CCCC"
C07 <- "#009999"
C15 <- "#006666"
C25 <- "#336666"

color_palette <- c(C03, C07, C15, C25)


plot3 <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C03) +  xlim(xlims) + ggtitle("3 m") + xlab("Date") + ylab("Temp. (°C)") +
  theme_bw()
plot7 <- ggplot(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C07) +  xlim(xlims) + ggtitle("7 m") + xlab("Date") + ylab("Temp. (°C)") +
  theme_bw()

plot15 <- ggplot(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C15) +  xlim(xlims) + ggtitle("15 m") + xlab("Date") + ylab("Temp. (°C)") +
  theme_bw()
plot25 <- ggplot(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C25) +  xlim(xlims) + ggtitle("25 m") + xlab("Date") + ylab("Temp. (°C)") +
  theme_bw()

grid.arrange(plot3, plot7, plot15, plot25, ncol = 1)



# ------------------------- CREATE PLOTS  --------------------------------------

# Define axis limits for the plots
xlims <- c(min(data_raw$datetime), max(data_raw$datetime))
ylims <- c(min(data_raw$temp), max(data_raw$temp))
data_merged$depth_classes_for_faceting <- factor(paste(data_merged$depth_m, "m"), levels = c("3 m", "7 m", "15 m", "25 m"))

# Plotting raw data
plot_temp_03m_raw <- ggplot(data = data_raw[data_raw$depth_m == 3, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "03 m raw") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "3 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_07m_raw <- ggplot(data = data_raw[data_raw$depth_m == 7, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "07 m raw") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "3 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_15m_raw <- ggplot(data = data_raw[data_raw$depth_m == 15, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "15 m raw") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "3 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_25m_raw <- ggplot(data = data_raw[data_raw$depth_m == 25, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "25 m raw") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "3 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_25m_raw

# Plot all cleaned (temp)
plot_temp_all <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line() +
  ggtitle("All depths")

# Plot cleaned temp per depth
plot_temp_03m <- ggplot(data = data_merged[data_merged$depth_m == 3, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#33CCCC" ) +
  ggtitle(label = "03 m") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw()
plot_temp_07m <- ggplot(data = data_merged[data_merged$depth_m == 7, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#009999" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw()
plot_temp_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#006666" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw()
plot_temp_25m <- ggplot(data = data_merged[data_merged$depth_m == 25, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_bw()

# Plot conductivity (raw)
cond_data <- subset(data_raw, select = c(datetime, conductivity_raw, depth_m))
cond_data <- cond_data[!is.na(cond_data$conductivity_raw), ]

plot_cond_03m <- ggplot(data = cond_data[cond_data$depth_m == 3, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("03 m") +
  xlab("Time") + ylab("Raw conductivity (\u00b2S cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_07m <- ggplot(data = cond_data[cond_data$depth_m == 7, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Raw conductivity (\u00b2S cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_15m <- ggplot(data = cond_data[cond_data$depth_m == 15, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Raw conductivity (\u00b2S cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_25m <- ggplot(data = cond_data[cond_data$depth_m == 25, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Raw conductivity \u00b2S cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))


plot_temp_comparison_03m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 3, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 3, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "03 m") + 
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_comparison_07m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 7, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 7, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "07 m") + 
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_comparison_15m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 15, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 15, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "15 m") + 
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_comparison_25m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 25, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 25, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "25 m") + 
  xlab("Time") + ylab("Temp. (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

color_palette <- c("#33CCCC", "#009999", "#006666", "#336666")
data_nofacet <- subset(data_merged, select = -c(depth_classes_for_faceting))
plot_faceted <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp)) +
  theme_bw() +
  geom_line(data = data_nofacet[data_nofacet$depth_m == 3,], col="black", alpha=0.1) +
  geom_line(data = data_nofacet[data_nofacet$depth_m == 7,], col="black", alpha=0.1) +
  geom_line(data = data_nofacet[data_nofacet$depth_m == 15,], col="black", alpha=0.1) +
  geom_line(data = data_nofacet[data_nofacet$depth_m == 25,], col="black", alpha=0.1) +
  geom_line(mapping = aes(color = depth_m)) +
  facet_grid(depth_classes_for_faceting ~ .) +
  geom_line(mapping = aes(color = depth_m)) +
  scale_colour_manual(values = color_palette) +
  ggtitle(label = "Temperature data per depth over time")
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
plot_faceted

alpha_value <- 0.2
alpha_plot_03 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color = depth_m)) +
  theme_bw() +
  scale_color_manual(values = color_palette) +
  geom_line(alpha = alpha_value) +
  geom_line(data = data_merged[data_merged$depth_m == 3,], color = C03) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "3 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
alpha_plot_7 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color = depth_m)) +
  theme_bw() +
  scale_color_manual(values = color_palette) +
  geom_line(alpha = alpha_value) +
  geom_line(data = data_merged[data_merged$depth_m == 7,], color = C07) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "7 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
alpha_plot_15 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color = depth_m)) +
  theme_bw() +
  scale_color_manual(values = color_palette) +
  geom_line(alpha = alpha_value) +
  geom_line(data = data_merged[data_merged$depth_m == 15,], color = C15) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "15 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
alpha_plot_25 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color = depth_m)) +
  theme_bw() +
  scale_color_manual(values = color_palette) +
  geom_line(alpha = alpha_value) +
  geom_line(data = data_merged[data_merged$depth_m == 25,], color = C25) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "25 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
grid.arrange(alpha_plot_03, alpha_plot_7, alpha_plot_15, alpha_plot_25, ncol=1,
             heights=c(1, 1, 1, 1.4))


alpha_value <- 0.1
alpha_grey_plot_03 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color = depth_m)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  geom_line(alpha = alpha_value) +
  geom_line(data = data_merged[data_merged$depth_m == 3,], color = C03) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "3 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
alpha_grey_plot_7 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color = depth_m)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  geom_line(alpha = alpha_value) +
  geom_line(data = data_merged[data_merged$depth_m == 7,], color = C07) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "7 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
alpha_grey_plot_15 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp, color=depth_m)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  geom_line(alpha = alpha_value) + #, mapping = aes(color=depth_m)) +
  geom_line(data = data_merged[data_merged$depth_m == 15,], color = C15) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "15 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
alpha_grey_plot_25 <- ggplot(data = data_merged, mapping = aes(x=datetime, y=temp)) +
  theme_bw() +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  geom_line(alpha = alpha_value, mapping = aes(color=depth_m)) +
  geom_line(data = data_merged[data_merged$depth_m == 25,], color = C25) +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  ggtitle(label = "25 m") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
grid.arrange(alpha_grey_plot_03, alpha_grey_plot_7, alpha_grey_plot_15, alpha_grey_plot_25, ncol=1,
             heights=c(1, 1, 1, 1.4))


# -------------------------- SHOW PLOTS ----------------------------------------

# grid.arrange(plot_03m_raw, plot_07m_raw, plot_15m_raw, plot_25m_raw, ncol=1)
grid.arrange(plot_temp_comparison_03m, plot_temp_comparison_07m,
             plot_temp_comparison_15m, plot_temp_comparison_25m,
             ncol=1)
grid.arrange(plot_temp_03m_raw, plot_temp_07m_raw, plot_temp_15m_raw, plot_temp_25m_raw, ncol=1)
grid.arrange(plot_temp_03m, plot_temp_07m, plot_temp_15m, plot_temp_25m, ncol=1)
grid.arrange(alpha_plot_03, alpha_plot_7, alpha_plot_15, alpha_plot_25, ncol=1)

grid.arrange(alpha_plot_03, alpha_plot_7, alpha_plot_15, alpha_plot_25, ncol=1,
             heights=c(1, 1, 1, 1.4))
plot_temp_all

