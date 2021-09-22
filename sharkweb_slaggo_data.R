rm(list = ls())

library(ggplot2)
library(lubridate)
library(gridExtra)
library(stringr)

setwd("C:/Users/Roland/Desktop")

d1 <- read.csv("Sharkweb_data_2018-2019.txt", sep ="\t")
d2 <- read.csv("Sharkweb_data_2020-2021.txt", sep ="\t")

data <- rbind(d1, d2)
data$timestamp <- paste(data$Provtagningsdatum, data$Provtagningstid..start.)
data <- subset(data, select = c(timestamp,
                                Provtagningsdjup..m.,
                                Temperatur.CTD..C.,
                                Salinitet.CTD..o.oo.psu.,
                                Klorofyll.a.vattenhämtare..ug.l.))

data$Temperatur.CTD..C. <- str_replace(data$Temperatur.CTD..C., ",", ".")
data$Salinitet.CTD..o.oo.psu. <- str_replace(data$Salinitet.CTD..o.oo.psu., ",", ".")
data$Klorofyll.a.vattenhämtare..ug.l. <- str_replace(data$Klorofyll.a.vattenhämtare..ug.l., ",", ".")

data <- data[data$Provtagningsdjup..m. <= 30, ]

# data <- data[data$Provtagningsdjup..m. == 3 |
#                data$Provtagningsdjup..m. == 5 |
#                data$Provtagningsdjup..m. == 8 |
#                data$Provtagningsdjup..m. == 15 |
#                data$Provtagningsdjup..m. == 30 ,]


data <- data[data$Provtagningsdjup..m. == 0 |
               data$Provtagningsdjup..m. == 3 |
               data$Provtagningsdjup..m. == 5 |
               data$Provtagningsdjup..m. == 8 |
               data$Provtagningsdjup..m. == 15 |
               data$Provtagningsdjup..m. == 30,]

data$Provtagningsdjup..m. <- as.factor(data$Provtagningsdjup..m.)

data$Temperatur.CTD..C. <- as.numeric(data$Temperatur.CTD..C.)
data$Salinitet.CTD..o.oo.psu. <- as.numeric(data$Salinitet.CTD..o.oo.psu.)
data$Klorofyll.a.vattenhämtare..ug.l. <- as.numeric(data$Klorofyll.a.vattenhämtare..ug.l.)


data$timestamp <- ymd_hm(data$timestamp)


plot_temp <- ggplot(data=data, mapping = aes(x = timestamp, y = Temperatur.CTD..C., color = Provtagningsdjup..m.)) +
  geom_line()

plot_sal <- ggplot(data = data, mapping = aes(x=timestamp, y=Salinitet.CTD..o.oo.psu., color = Provtagningsdjup..m.)) +
  geom_line()

plot_temp
plot_sal

grid.arrange(plot_temp, plot_sal, ncol = 1)
