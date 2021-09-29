library(ggplot2)
library(gridExtra)

fname <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_data_merged_2021-09-04.csv"
data <- read.csv(fname)
data$datetime <- as.POSIXct(data$datetime,
                            format = "%Y-%m-%d %H:%M:%S")


xlims <- c(min(data$datetime), max(data$datetime))
# Colors:
C03 <- "#33CCCC"
C07 <- "#009999"
C15 <- "#006666"
C25 <- "#336666"

plot3 <- ggplot(data = data[data$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C03) +  xlim(xlims) + ggtitle("3 m") + xlab("Date") + ylab("Temp. (°C)")
plot7 <- ggplot(data = data[data$depth_m == 7, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C07) +  xlim(xlims) + ggtitle("7 m") + xlab("Date") + ylab("Temp. (°C)")
plot15 <- ggplot(data = data[data$depth_m == 15, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C15) +  xlim(xlims) + ggtitle("15 m") + xlab("Date") + ylab("Temp. (°C)")
plot25 <- ggplot(data = data[data$depth_m == 25, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color = C25) +  xlim(xlims) + ggtitle("25 m") + xlab("Date") + ylab("Temp. (°C)")

grid.arrange(plot3, plot7, plot15, plot25, ncol = 1)



# ------------------------- CREATE PLOTS  --------------------------------------

# Define axis limits for the plots
xlims <- c(min(data_raw$datetime), max(data_raw$datetime))
ylims <- c(min(data_raw$temp), max(data_raw$temp))
data_merged$depth_classes <- factor(paste(data_merged$depth_m, "m"), levels = c("3 m", "7 m", "15 m", "25 m"))

# Plotting raw data
plot_temp_03m_raw <- ggplot(data = data_raw[data_raw$depth_m == 3, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "03 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_07m_raw <- ggplot(data = data_raw[data_raw$depth_m == 7, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "07 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_15m_raw <- ggplot(data = data_raw[data_raw$depth_m == 15, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "15 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_temp_25m_raw <- ggplot(data = data_raw[data_raw$depth_m == 25, ], mapping = aes(x = datetime, y = temp, color = "red")) +
  geom_line() +
  ggtitle(label = "25 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Plotting cleaned temperature data
plot_temp_all <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line() +
  ggtitle("All depths")

plot_temp_03m <- ggplot(data = data_merged[data_merged$depth_m == 3, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#33CCCC" ) +
  ggtitle(label = "03 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_07m <- ggplot(data = data_merged[data_merged$depth_m == 7, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#009999" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#006666" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_25m <- ggplot(data = data_merged[data_merged$depth_m == 25, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Plot conductivity (raw)
cond_data <- subset(data_raw, select = c(datetime, conductivity_raw, depth_m))
cond_data <- cond_data[!is.na(cond_data$conductivity_raw), ]

plot_cond_03m <- ggplot(data = cond_data[cond_data$depth_m == 3, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("03 m") +
  xlab("Time") + ylab("Raw conductivity (uS cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_07m <- ggplot(data = cond_data[cond_data$depth_m == 7, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Raw conductivity (uS cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_15m <- ggplot(data = cond_data[cond_data$depth_m == 15, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Raw conductivity (uS cm-1)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_25m <- ggplot(data = cond_data[cond_data$depth_m == 25, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Raw conductivity (uS cm-1)") +
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
  xlab("Time") + ylab("Temp. (°C)") +
  xlab("Time") + ylab("Temp. (°C)") +
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

plot_faceted <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp)) +
  theme_fivethirtyeight() +
  facet_wrap(~depth_classes, ncol=1) +
  geom_line(color = "seagreen") +
  xlab("Date") + ylab("Temperature (\u00b0C)")




# -------------------------- SHOW PLOTS ----------------------------------------

# grid.arrange(plot_03m_raw, plot_07m_raw, plot_15m_raw, plot_25m_raw, ncol=1)
grid.arrange(plot_temp_comparison_03m, plot_temp_comparison_07m,
             plot_temp_comparison_15m, plot_temp_comparison_25m,
             ncol=1)
grid.arrange(plot_temp_03m_raw, plot_temp_07m_raw, plot_temp_15m_raw, plot_temp_25m_raw, ncol=1)
grid.arrange(plot_temp_03m, plot_temp_07m, plot_temp_15m, plot_temp_25m, ncol=1)
plot_temp_all