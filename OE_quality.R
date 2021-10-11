cat("\014")
rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(gridExtra)
library(lubridate)
library(RColorBrewer)
library(viridis)

FPATH_OE <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/OE/Maturation quality OE 2021_final_2021-10-06.csv"
PLOT_FOLDER <- "/media/findux/DATA/Documents/IVL/Data/quality_oe_me/plots/"
TSTAMP <- format(now(), format = "_%Y-%m-%d_%H-%M")
SAVE_PLOTS <- FALSE
IVL_PALETTE <- c("#64cbce", "#37aaae", "#257274", "#1f5f61")
fpath_me_plot <- paste(PLOT_FOLDER, "OE_plots", TSTAMP, ".pdf", sep = "")

oeq_data <- read.csv(FPATH_OE, check.names = TRUE)
names(oeq_data)

# Fix colnames
names(oeq_data)[names(oeq_data) == "Date.sampled"] <- "sampling_date"
names(oeq_data)[names(oeq_data) == "Depth"] <- "depth_m"
names(oeq_data)[names(oeq_data) == "CI..length."] <- "ci_len"
names(oeq_data)[names(oeq_data) == "DW.tissue.cup"] <- "dw_tissue"
names(oeq_data)[names(oeq_data) == "Dry.weight.shell..48.hrs...g."] <- "dw_shell"
names(oeq_data)[names(oeq_data) == "Depth..mm."] <- "depth_shell_mm"
names(oeq_data)[names(oeq_data) == "CI..Shell."] <- "ci_shell"


# Fix data types
oeq_data$dw_shell <- as.numeric(oeq_data$dw_shell)
oeq_data$depth_shell_mm <- as.numeric(oeq_data$depth_shell_mm)
oeq_data$depth_m <- as.factor(oeq_data$depth_m)
oeq_data$sampling_date <- as.factor(oeq_data$sampling_date)



# Correct sampling dates
start_batch <- oeq_data[oeq_data$sampling_date == "20210519", ]
for (depth in c(7, 15, 25)){
  new_batch <- start_batch
  new_batch$depth_m <- depth
  oeq_data <- rbind(oeq_data, new_batch)
}
oeq_data$sampling_date[oeq_data$sampling_date == "20210616"] <- "20210618"
oeq_data$sampling_date[oeq_data$sampling_date == "20210701"] <- "20210702"
# Add hyphens in the dates
oeq_data$sampling_date <- paste(substring(oeq_data$sampling_date, 1, 4),
                                substring(oeq_data$sampling_date, 5, 6),
                                substring(oeq_data$sampling_date, 7, 8),
                                sep = "-")

# Only keep relevant data
oeq_data <- subset(oeq_data, select = c(sampling_date, depth_m, dw_shell, dw_tissue,
                                        dw_shell, ci_shell))
# Add verbose depth
oeq_data$depth_verbose <- factor(paste(oeq_data$depth_m, "m"), levels = c("3 m", "7 m", "15 m", "25 m"))

# Drop NA
oeq_data <- na.omit(oeq_data)



plot_OE_dw_shell <- ggplot(data = oeq_data,
                           mapping = aes(x = sampling_date, y = dw_shell, fill = sampling_date)) +
  theme_bw()+
  facet_grid(.~depth_verbose) +
  geom_boxplot(outlier.shape = 21) +
  ggtitle(label = "*Ostrea edulis* dry weight (shell) per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("Sampling occasion") +
  ylab("Shell dry weight (g)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")
plot_OE_dw_shell


plot_OE_CI_dw_tissue_dw_shell <- ggplot(data = oeq_data,
                                        mapping = aes(x = sampling_date, y = dw_tissue / dw_shell * 100, fill = sampling_date)) +
  theme_bw()+
  facet_grid(.~depth_verbose) +
  geom_boxplot(outlier.shape = 21) +
  ggtitle(label = "*Ostrea edulis* CI (shell DW) per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("Sampling occasion") +
  ylab("CI (DW tissue / DW shell)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")
plot_OE_CI_dw_tissue_dw_shell


