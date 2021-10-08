cat("\014")
rm(list = ls())

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)


# TODO: CI (shell) for OE


FPATH_OE <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/OE/Maturation quality OE 2021_final_2021-10-06.csv"
FPATH_ME <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/ME/Maturation quality ME 2021_2021-10-06.csv"
PLOT_FOLDER <- "/media/findux/DATA/Documents/IVL/Data/quality_oe_me/plots/"
TSTAMP <- format(now(), format = "_%Y-%m-%d_%H-%M")
SAVE_PLOTS <- FALSE

fpath_oe_plot <- paste(PLOT_FOLDER, "OE_plots", TSTAMP, ".pdf", sep = "")
fpath_me_plot <- paste(PLOT_FOLDER, "ME_plots", TSTAMP, ".pdf", sep = "")


# -------------------------- Data prep -----------------------------------------


oeq_data <- read.csv(FPATH_OE, check.names = TRUE)
meq_data <- read.csv(FPATH_ME, check.names = TRUE)


# Prepping OE
names(oeq_data)[names(oeq_data) == "Date.sampled"] <- "sampling_date"
names(oeq_data)[names(oeq_data) == "Depth"] <- "depth_m"
names(oeq_data)[names(oeq_data) == "CI..length."] <- "ci_len"
names(oeq_data)[names(oeq_data) == "DW.tissue.cup"] <- "dw_tissue"
names(oeq_data)[names(oeq_data) == "Depth..mm."] <- "depth_shell_mm"
names(oeq_data)[names(oeq_data) == "CI..Shell."] <- "ci_shell"
oeq_data <- subset(oeq_data, select = c(sampling_date, depth_m, dw_tissue, depth_shell_mm, ci_len, ci_shell))
# Reorder factor levels
oeq_data$species <- "OE"


# Prepping ME
names(meq_data)[names(meq_data) == "Date.sampled"] <- "sampling_date"
names(meq_data)[names(meq_data) == "Depth..m."] <- "depth_m"
names(meq_data)[names(meq_data) == "CI"] <- "ci_len"
names(meq_data)[names(meq_data) == "Tissue.DW.without.cup"] <- "dw_tissue"
names(meq_data)[names(meq_data) == "Depth..mm."] <- "depth_shell_mm"
meq_data <- subset(meq_data, select = c(sampling_date, depth_m, ci_len, dw_tissue, depth_shell_mm))
meq_data$ci_shell <- NA_real_
meq_data$species <- "ME"


# Merge data and fix classes
data <- rbind(oeq_data, meq_data)
data$depth_m <- as.numeric(data$depth_m)
data$depth_m <- as.factor(data$depth_m)
data$sampling_date<- as.factor(data$sampling_date)
data$ci_len <- as.numeric(data$ci_len)
data$ci_shell <- as.numeric(data$ci_shell)
data$depth_m <- factor(data$depth_m, levels = c(3, 7, 15, 25))
data$dw_tissue <- as.numeric(data$dw_tissue)
data <- data[!is.na(data$sampling_date), ]


# -------------------------- Plotting ------------------------------------------

oe_date_dw <- ggplot(data = data[data$species == "OE", ], mapping = aes(x = sampling_date, y = dw_tissue, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("OE DW") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
oe_depth_dw <- ggplot(data = data[data$species == "OE", ], mapping = aes(x = depth_m, y = dw_tissue, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("OE DW")
oe_date_cil <- ggplot(data = data[data$species == "OE", ], mapping = aes(x = sampling_date, y = ci_len, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("OE CI (length)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
oe_depth_cil <- ggplot(data = data[data$species == "OE", ], mapping = aes(x = depth_m, y = ci_len, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("OE CI (length)")
oe_date_cis <- ggplot(data = data[data$species == "OE", ], mapping = aes(x=sampling_date, y = ci_shell, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("OE CI (shell)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
oe_depth_cis <- ggplot(data = data[data$species == "OE", ], mapping = aes(x=depth_m, y = ci_shell, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("OE CI (shell)")



me_date_v_cil <- ggplot(data = data[data$species == "ME", ], mapping = aes(x = sampling_date, y = ci_len, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("ME CI (length)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
me_depth_v_cil <- ggplot(data = data[data$species == "ME", ], mapping = aes(x = depth_m, y = ci_len, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("ME CI (length)")
me_date_dw <- ggplot(data = data[data$species == "ME", ], mapping = aes(x = sampling_date, y = dw_tissue, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("ME DW") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
me_depth_dw <- ggplot(data = data[data$species == "ME", ], mapping = aes(x = depth_m, y = dw_tissue, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("ME DW")


oe_plots <- grid.arrange(oe_date_dw, oe_depth_dw,
                        oe_date_cil, oe_depth_cil,
                        oe_date_cis, oe_depth_cis,
                        ncol=2)
me_plots <- grid.arrange(me_date_dw, me_depth_dw,
                        me_date_v_cil, me_depth_v_cil,
                        ncol=2)
oe_plots
me_plots

if (SAVE_PLOTS){ggsave(fpath_oe_plot, oe_plots, device="pdf",
                       height=420, width=297, units="mm")
  ggsave(fpath_me_plot, me_plots, device="pdf",
         height=420, width=297, units="mm")
  }

medians <- data %>%
  na.omit() %>%
  group_by(depth_m, sampling_date, species) %>%
  summarise(avg = median(dw_tissue)) %>%
  ungroup()



