cat("\014")
rm(list = ls())

library(ggplot2)
library(gridExtra)

# TODO: CI (shell) for OE
# TODO: Check if the same start data for all is also applicable here?



# Settings and paths
FPATH_OE <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/OE/Maturation quality OE 2021_final_2021-10-06.csv"
PLOT_FOLDER <- "/media/findux/DATA/Documents/IVL/Data/quality_oe_me/plots/"
TSTAMP <- format(now(), format = "_%Y-%m-%d_%H-%M")
SAVE_PLOTS <- FALSE
fpath_oe_plot <- paste(PLOT_FOLDER, "OE_plots", TSTAMP, ".pdf", sep = "")

# Load data
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

# Only keep relevant data
oeq_data <- subset(oeq_data, select = c(sampling_date, depth_m, depth_shell_mm, dw_shell, dw_tissue, ci_len, ci_shell))

# Apply the starting data to all
# starting_batch <- oeq_data[oeq_data$sampling_date == "20210519", ]
# for (depth in c(7, 15, 25)){
#   # Set depth and rbind to data
# }




# Calculate the medians
medians <- oeq_data %>%
  na.omit() %>%
  group_by(depth_m, sampling_date) %>%
  summarise(median_dw_tissue = median(dw_tissue)) %>%
  ungroup()

# Calculate a CI based on meat dw / shell dw
oeq_data$ci_meat <- oeq_data$dw_tissue / oeq_data$dw_shell

# -------------------------- Plotting ------------------------------------------

oe_date_dw <- ggplot(data = oeq_data, mapping = aes(x = sampling_date, y = dw_tissue, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("OE DW") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
oe_depth_dw <- ggplot(data = oeq_data, mapping = aes(x = depth_m, y = dw_tissue, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("OE DW")
oe_date_cil <- ggplot(data = oeq_data, mapping = aes(x = sampling_date, y = ci_len, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("OE CI (length)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
oe_depth_cil <- ggplot(data = oeq_data, mapping = aes(x = depth_m, y = ci_len, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("OE CI (length)")
oe_date_cis <- ggplot(data = oeq_data, mapping = aes(x=sampling_date, y = ci_shell, fill=depth_m)) +
  geom_boxplot() +
  ggtitle("OE CI (shell)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
oe_depth_cis <- ggplot(data = oeq_data, mapping = aes(x=depth_m, y = ci_shell, fill=sampling_date)) +
  geom_boxplot() +
  ggtitle("OE CI (shell)")



oe_plots <- grid.arrange(oe_date_dw, oe_depth_dw,
                         oe_date_cil, oe_depth_cil,
                         oe_date_cis, oe_depth_cis,
                         ncol=2)
oe_plots
