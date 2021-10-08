cat("\014")
rm(list = ls())

library(ggplot2)
library(gridExtra)


# TODO: CI (shell) for OE


fpath_oe <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/OE/Maturation quality OE 2021_final_2021-10-06.csv"
fpath_me <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/ME/Maturation quality ME 2021_2021-10-06.csv"


# -------------------------- Data prep -----------------------------------------


oeq_data <- read.csv(fpath_oe, check.names = TRUE)


# Prepping OE
names(oeq_data)[names(oeq_data) == "Date.sampled"] <- "sampling_date"
names(oeq_data)[names(oeq_data) == "Depth"] <- "depth_m"
names(oeq_data)[names(oeq_data) == "CI..length."] <- "ci_len"
names(oeq_data)[names(oeq_data) == "DW.tissue.cup"] <- "dw_tissue"
names(oeq_data)[names(oeq_data) == "Depth..mm."] <- "depth_shell_mm"
names(oeq_data)[names(oeq_data) == "CI..Shell."] <- "ci_shell"
oeq_data <- subset(oeq_data, select = c(sampling_date, depth_m, ci_len, dw_tissue, depth_shell_mm, ci_shell))

oeq_data$depth_m <- as.factor(oeq_data$depth_m)
oeq_data$sampling_date <- as.factor(oeq_data$sampling_date)

ggplot(data = oeq_data, mapping = aes(x=sampling_date, y = ci_shell, fill=depth_m)) +
  geom_boxplot()