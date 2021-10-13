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
names(oeq_data)[names(oeq_data) == "Metal.cup.weight..g."] <- "cup_weight"
names(oeq_data)[names(oeq_data) == "Dry.weight.tissue.cup..48.hrs...g."] <- "dw_cup_and_tissue"


# Only keep relevant data
oeq_data <- subset(oeq_data, select = c(sampling_date, depth_m, cup_weight, dw_cup_and_tissue, dw_shell, dw_tissue, ci_len, ci_shell))

# Fix data types
oeq_data$dw_shell <- as.numeric(oeq_data$dw_shell)
oeq_data$depth_m <- as.factor(oeq_data$depth_m)
oeq_data$sampling_date <- as.factor(oeq_data$sampling_date)
oeq_data$cup_weight <- as.numeric(oeq_data$cup_weight)
oeq_data$dw_cup_and_tissue <- as.numeric(oeq_data$dw_cup_and_tissue)

# Drop NAs
oeq_data <- na.omit(oeq_data)

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
oeq_data$sampling_date <- ymd(oeq_data$sampling_date)

# Add short dates
short_levels <- unique(oeq_data$sampling_date) %>%
  format(format = "%b. %d")
oeq_data$sampling_date_short <- factor(format(oeq_data$sampling_date, format = "%b. %d"),
                                       levels = short_levels)


# Add verbose depth
oeq_data$depth_verbose <- factor(paste(oeq_data$depth_m, "m"), levels = c("3 m", "7 m", "15 m", "25 m"))

plot_OE_cups <- ggplot(data = oeq_data, mapping = aes(x = sampling_date_short, y = cup_weight, fill = sampling_date_short)) +
  geom_boxplot(outlier.shape = 21, show.legend = FALSE) +
  facet_grid(.~depth_verbose) +
  theme_bw() +
  ggtitle(label = "Cup weights") +
  xlab("\nSampling occasion (2021)") +
  ylab("Cup weight (g)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")

  

plot_OE_DW_shell <- ggplot(data = oeq_data,
                           mapping = aes(x = sampling_date_short,
                                         y = dw_shell,
                                         fill = sampling_date_short)) +
  theme_bw()+
  facet_grid(.~depth_verbose) +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE,
               size = 0.2) +
  ggtitle(label = "*Ostrea edulis* shell dry weight per sampling occasion and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("\nSampling occasion (2021)") +
  ylab("\nShell dry weight (g)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")



plot_OE_CI_dw_tissue_dw_shell <- ggplot(data = oeq_data,
                                        mapping = aes(x = sampling_date_short,
                                                      y = dw_tissue / dw_shell * 100,
                                                      fill = sampling_date_short)) +
  theme_bw()+
  facet_grid(.~depth_verbose) +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE,
               size = 0.2) +
  ggtitle(label = "*Ostrea edulis* condition index per sampling occasion and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("\nSampling occasion (2021)") +
  ylab("Condition index\n((DW tissue (g) / DW shell (g)) * 100)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")



grid.arrange(plot_OE_DW_shell,
             plot_OE_CI_dw_tissue_dw_shell,
             ncol = 1)


