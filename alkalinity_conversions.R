rm(list = ls())

library(ggplot2)


data <- read.csv("file:///media/findux/DATA/Documents/IVL/Data/alkalinity_table.csv")
conversion_model <- lm(data = data, ppm ~ dKH)
model_coefficients <-summary(conversion_model)$coefficients
model_intercept <- model_coefficients[1, 1]
model_slope <- model_coefficients[2, 1]

ggplot(data=data, mapping=aes(x=dKH, y=ppm)) +
  geom_abline(slope = model_slope, intercept = model_intercept, lty = 2, color = "brown") +
  geom_point(alpha = 0.6) + 
  ggtitle("Alkalinity conversion (dKH/ppm)")

dkh_to_ppm <- function(dkh){
  dkh * model_slope + model_intercept
}

dkh_to_ppm(5.2)
dkh_to_ppm(5)
