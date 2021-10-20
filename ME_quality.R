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


# ------------------------------ SETTINGS --------------------------------------

Sys.setlocale("LC_ALL", "en_GB.UTF-8")

FPATH_ME <- "file:///media/findux/DATA/Documents/IVL/data/quality_oe_me/ME/Maturation quality ME 2021_2021-10-06_new.csv"
PLOT_FOLDER <- "/media/findux/DATA/Documents/IVL/data/quality_oe_me/plots/"
TSTAMP <- format(now(), format = "_%Y-%m-%d_%H-%M")
SAVE_PLOTS <- FALSE
COLOR_PALETTE <- c("#00e6e6", "#00CCCC", "#009999", "#006666")
COLOR_PALETTE <- c("#00e6e6", "#00cccc", "#00b3b3", "#008080")
IVL_PALETTE <- c("#64cbce", "#37aaae", "#257274", "#1f5f61")
fpath_me_plot <- paste(PLOT_FOLDER, "ME_plots", TSTAMP, ".pdf", sep = "")


# ---------------------- LOADING AND PREPPING DATA -----------------------------

meq_data <- read.csv(FPATH_ME, check.names = TRUE)
names(meq_data)


# Prepping ME
names(meq_data)[names(meq_data) == "Date.sampled"] <- "sampling_date"
names(meq_data)[names(meq_data) == "Depth..m."] <- "depth_m"
names(meq_data)[names(meq_data) == "Live.WW..g."] <- "ww_live"
names(meq_data)[names(meq_data) == "Metal.cup.weight..g."] <- "cup_weight"
names(meq_data)[names(meq_data) == "Tissue.DW.without.cup"] <- "dw_tissue"

names(meq_data)[names(meq_data) == "Dry.weight.tissue.cup..48.hrs...g."] <- "dw_tissue_and_cup"

names(meq_data)[names(meq_data) == "Length..mm."] <- "length_shell_mm"
names(meq_data)[names(meq_data) == "Width..mm."] <- "width_shell_mm"
names(meq_data)[names(meq_data) == "Depth..mm."] <- "depth_shell_mm"
names(meq_data)[names(meq_data) == "Sample.ID"] <- "sample_id"
names(meq_data)[names(meq_data) == "CI"] <- "ci_len"


meq_data <- subset(meq_data, select = c(sampling_date, sample_id,
                                        depth_m,
                                        dw_tissue,
                                        ci_len,
                                        cup_weight,
                                        dw_tissue_and_cup,
                                        length_shell_mm,
                                        width_shell_mm,
                                        depth_shell_mm))

# Add hyphens in the dates
meq_data$sampling_date <- paste(substring(meq_data$sampling_date, 1, 4),
                                substring(meq_data$sampling_date, 5, 6),
                                substring(meq_data$sampling_date, 7, 8),
                                sep = "-")
meq_data$sampling_date <- ymd(meq_data$sampling_date)


# Create a short date factor column
short_levels <- unique(meq_data$sampling_date) %>%
  format(format = "%b. %d")
meq_data$sampling_date_short <- factor(format(meq_data$sampling_date, format = "%b. %d"),
                                       levels = short_levels)


# Fix classes
meq_data$depth_m <- as.numeric(meq_data$depth_m)
meq_data$dw_tissue_and_cup <- as.numeric(meq_data$dw_tissue_and_cup)
meq_data$cup_weight <- as.numeric(meq_data$cup_weight)
meq_data$length_shell_mm <- as.numeric(meq_data$length_shell_mm)
meq_data$width_shell_mm <- as.numeric(meq_data$width_shell_mm)
meq_data$depth_shell_mm <- as.numeric(meq_data$depth_shell_mm)
meq_data$sampling_date<- as.factor(meq_data$sampling_date)
meq_data$ci_len <- as.numeric(meq_data$ci_len)
meq_data$depth_m <- factor(meq_data$depth_m, levels = c(3, 7, 15, 25))
meq_data$dw_tissue <- as.numeric(meq_data$dw_tissue)
meq_data <- na.omit(meq_data)


# Set starting values for all depths:
start_bulk <- meq_data[meq_data$sampling_date == "2021-03-22",]
for (depth in c(7, 15, 25)){
  new_bulk <- start_bulk
  new_bulk$depth_m <- depth
  meq_data <- rbind(meq_data, new_bulk)
}
rm(list = c("new_bulk", "start_bulk"))


# Add a "verbose" depth column
m <- paste(as.character(meq_data$depth_m), "m")

meq_data$depth_verbose <- factor(paste(meq_data$depth_m, "m"),
                                 levels = c("3 m", "7 m", "15 m", "25 m"))


Medians <- meq_data %>%
  group_by(depth_m, sampling_date) %>%
  summarise(median_value = median(dw_tissue)) %>%
  ungroup()


# ----------------------- CUP WEIGHT CORRECTIONS--------------------------------


# Use +1 where weight < 1
sel <- meq_data$depth_m == 15 & meq_data$cup_weight < 1
meq_data$cup_weight_corrected <- meq_data$cup_weight
message("Correcting ", sum(sel), " data points where cup weight was below 1.")
meq_data$cup_weight_corrected[sel] <- meq_data$cup_weight_corrected[sel] + 1
meq_data$dw_tissue_recalculated <- meq_data$dw_tissue_and_cup - meq_data$cup_weight_corrected
meq_data$ci_len_recalculated <- meq_data$dw_tissue_recalculated / meq_data$length_shell_mm * 100


# Use avg. cup weight
sel <- meq_data$sampling_date == "2021-06-01" & meq_data$depth_m == 15
meq_data$cup_weight_avg <- meq_data$cup_weight
meq_data$cup_weight_avg[sel] <- mean(meq_data$cup_weight[!sel]) # Avg everything except the values to replace
meq_data$dw_tissue_using_cup_avg <- meq_data$dw_tissue_and_cup - meq_data$cup_weight_avg
meq_data$ci_len_using_cup_avg <- meq_data$dw_tissue_using_cup_avg / meq_data$length_shell_mm * 100


# Using avg. weight around June 1 (previous and subsequent sampling date for 15m)
meq_data$cup_weight_adjacent_avg <- meq_data$cup_weight
sel_center <- meq_data$sampling_date_short == "Jun 01" & meq_data$depth_m == 15
sel_around <- meq_data$sampling_date_short %in% c("May 12", "Jun 14") & meq_data$depth_m == 15
short_avg <- mean(meq_data$cup_weight[sel_around])
meq_data$cup_weight_adjacent_avg[sel] <- short_avg
meq_data$dw_tissue_short_avg <- meq_data$dw_tissue_and_cup - meq_data$cup_weight_adjacent_avg
meq_data$ci_len_short_avg <- meq_data$dw_tissue_short_avg / meq_data$length_shell_mm * 100


# Using average from that same sampling occasion, just excluding the cups below 1
sel_june01 <- meq_data$depth_m != 15 & meq_data$sampling_date == "2021-06-01"
sel_center <- meq_data$sampling_date == "2021-06-01" & meq_data$depth_m == 15
meq_data$cup_weight_avg_june01 <- meq_data$cup_weight
meq_data$cup_weight_avg_june01[sel_center] <- mean(meq_data$cup_weight[sel_june01])
meq_data$dw_tissue_june01_avg <- meq_data$dw_tissue_and_cup - meq_data$cup_weight_avg_june01
meq_data$ci_len_june01_avg <- meq_data$dw_tissue_june01_avg / meq_data$length_shell_mm * 100


# # -------------------------------- DW PLOTS ------------------------------------
# 
# 
# plot_ME_DW_orig <- ggplot(data = meq_data, 
#                           mapping = aes(x = sampling_date_short, 
#                                         y = dw_tissue,
#                                         fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling occasion and depth. UNCORRECTED") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Dry weight tissue (g)") +
#   xlab("\nSampling occasion (2021)") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose, scales = "free_x") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_DW_recalculated <- ggplot(data = meq_data, 
#                                   mapping = aes(x = sampling_date_short,
#                                                 y = dw_tissue_recalculated,
#                                                 fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling occasion and depth.",
#           subtitle = "Using +1 correction where cup weight < 1.") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Dry weight tissue (g)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose, scales = "free_x") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_DW_w_cup_avg <- ggplot(data = meq_data,
#                                mapping = aes(x = sampling_date_short,
#                                              y = dw_tissue_using_cup_avg,
#                                              fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* dry weight over time per depth",
#           subtitle = "Using avg 15m cup weights (excluding June 1st).") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Tissue dry weight (g)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_DW_short_avg <- ggplot(data = meq_data,
#                                mapping = aes(x = sampling_date_short,
#                                              y = dw_tissue_short_avg,
#                                              fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* dry weight over time per depth",
#           subtitle = "Using avg 15m cup weights of May 12 and Jun 14.") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Tissue dry weight (g)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_DW_june01_avg <- ggplot(data = meq_data,
#                                 mapping = aes(x = sampling_date_short,
#                                               y = dw_tissue_june01_avg,
#                                               fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* dry weight over time per depth",
#           subtitle = "Using avg cup weights of 01 June (excl. 15 m).") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Tissue dry weight (g)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# # ----------------------------- CI PLOTS ---------------------------------------
# 
# 
# plot_ME_CI_orig <- ggplot(data = meq_data, 
#                       mapping = aes(x = sampling_date_short, y = ci_len,
#                                     fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE)+
#   ggtitle(label = "*Mytilus edulis* condition index per sampling occasion and depth. UNCORRECTED") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Condition index\n(DW tissue / shell length * 100)") +
#   xlab("Sampling occasion") +
#   facet_grid(.~depth_verbose) +
#   scale_fill_brewer(palette = "BrBG") +
#   guides(fill = guide_legend(title = "Sampling occasion")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_CI_recalculated <- ggplot(data = meq_data, 
#                                   mapping = aes(x = sampling_date_short,
#                                                 y = ci_len_recalculated,
#                                                 fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* condition index per sampling occasion and depth.",
#           subtitle = "Using +1 correction where cup weight < 1.") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Condition index\n(DW tissue / length)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# 
# plot_ME_CI_w_cup_avg <- ggplot(data = meq_data,
#                                mapping = aes(x = sampling_date_short,
#                                              y = ci_len_using_cup_avg,
#                                              fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* condition index over time per depth",
#           subtitle = "Using avg 15m cup weights (excluding June 1st).") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Condition index\n(DW tissue / length)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_CI_short_avg <- ggplot(data = meq_data, 
#                                mapping = aes(x = sampling_date_short,
#                                              y = ci_len_short_avg,
#                                              fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* condition index over time per depth",
#           subtitle = "Using avg 15m cup weights of May 12 and Jun 14.") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Condition index\n(DW tissue / length)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_ME_CI_june01_avg <- ggplot(data = meq_data,
#                                 mapping = aes(x = sampling_date_short,
#                                               y = ci_len_june01_avg,
#                                               fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "*Mytilus edulis* condition index over time per depth",
#           subtitle = "Using avg cup weights of 01 June (excl. 15 m).") +
#   theme(plot.title = ggtext::element_markdown()) +
#   ylab("Condition index\n(DW tissue / length)") +
#   xlab("Sampling occasion") + 
#   scale_fill_brewer(palette = "BrBG") +
#   facet_grid(.~depth_verbose) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# 
# # -------------------------- CUP PLOTS -----------------------------------------
# 
# 
# plot_ME_cups_orig <- ggplot(data = meq_data,
#                          mapping = aes(x = sampling_date_short,
#                                        y = cup_weight,
#                                        fill = sampling_date_short)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   facet_grid(.~depth_verbose) +
#   ggtitle("Cup weights (original)") +
#   ylab("Cup weight (g)") +
#   xlab("Sampling occasion") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   scale_fill_brewer(palette = "BrBG")
# 
# plot_ME_cups_corrected <- ggplot(data = meq_data,
#                          mapping = aes(x = sampling_date_short,
#                                        y = cup_weight_corrected,
#                                        fill = sampling_date_short)) +
#   theme_bw() +
#   facet_grid(.~depth_verbose) +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "Cup weights (corrected)", 
#           subtitle = "Using +1 correction where cup weight < 1.") +
#   ylab("Corrected cup weight (g)") +
#   xlab("Sampling occasion") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   scale_fill_brewer(palette = "BrBG")
# 
# plot_ME_cups_avg <- ggplot(data = meq_data,
#                                  mapping = aes(x = sampling_date_short,
#                                                y = cup_weight_avg,
#                                                fill = sampling_date_short)) +
#   theme_bw() +
#   facet_grid(.~depth_verbose) +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "Cup weights (corrected)", 
#           subtitle = "Using avg 15m cup weights (excluding June 1st).") +
#   ylab("Corrected cup weight (g)") +
#   xlab("Sampling occasion") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   scale_fill_brewer(palette = "BrBG")
# 
# plot_ME_cups_short_avg <- ggplot(data = meq_data,
#                            mapping = aes(x = sampling_date_short,
#                                          y = cup_weight_adjacent_avg,
#                                          fill = sampling_date_short)) +
#   theme_bw() +
#   facet_grid(.~depth_verbose) +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "Cup weights (corrected)", 
#           subtitle = "Using avg 15m cup weights of May 12 and Jun 14.") +
#   ylab("Corrected cup weight (g)") +
#   xlab("Sampling occasion") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   scale_fill_brewer(palette = "BrBG")
# 
# 
# plot_ME_cups_june01_avg <- ggplot(data = meq_data,
#                                   mapping = aes(x = sampling_date_short,
#                                                 y = cup_weight_avg_june01,
#                                                 fill = sampling_date_short)) +
#   theme_bw() +
#   facet_grid(.~depth_verbose) +
#   geom_boxplot(outlier.shape = 21,
#                show.legend = FALSE) +
#   ggtitle(label = "Cup weights (corrected)", 
#           subtitle = "Using avg cup weights of 01 June (excl. 15 m).") +
#   ylab("Corrected cup weight (g)") +
#   xlab("Sampling occasion") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   scale_fill_brewer(palette = "BrBG")
#
#
# grid.arrange(plot_ME_cups_orig,
#              plot_ME_cups_corrected,
#              plot_ME_cups_avg,
#              plot_ME_cups_short_avg,
#              plot_ME_cups_june01_avg,
#              ncol = 1)
# 
# grid.arrange(plot_ME_DW_orig,
#              plot_ME_DW_recalculated,
#              plot_ME_DW_w_cup_avg,
#              plot_ME_DW_short_avg,
#              plot_ME_DW_june01_avg,
#              ncol = 1)
# 
# grid.arrange(plot_ME_CI_orig,
#              plot_ME_CI_recalculated,
#              plot_ME_CI_w_cup_avg,
#              plot_ME_CI_short_avg,
#              plot_ME_CI_june01_avg,
#              ncol = 1)


# -------------------------- FINAL PLOTS ---------------------------------------



plot_ME_DW_final <- ggplot(data = meq_data,
                           mapping = aes(x = sampling_date_short,
                                         y = dw_tissue_june01_avg,
                                         fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE,
               lwd = 0.2) +
  facet_grid(.~depth_verbose) +
  ggtitle("*Mytilus edulis* dry weight over time per depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("\nSampling occasion (2021)") +
  ylab("\nTissue dry weight (g)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
  scale_fill_brewer(palette = "BrBG")


plot_ME_CI_final <- ggplot(data = meq_data,
                           mapping = aes(x = sampling_date_short,
                                         y = ci_len_june01_avg,
                                         fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE,
               size = 0.2) +
  facet_grid(.~depth_verbose) +
  ggtitle("*Mytilus edulis* condition index over time per depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("\nSampling occasion (2021)") +
  ylab("Condition index\n(DW tissue (g) / shell length (mm)) * 100 ") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
  scale_fill_brewer(palette = "BrBG")


plot_final <-grid.arrange(plot_ME_DW_final,
             plot_ME_CI_final,
             ncol = 1)

savename <- paste(PLOT_FOLDER, "MEQ_final.tiff", sep = "")
ggsave(savename, plot_final,
       width = 297, height = 210, units = "mm", dpi = 300)
