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