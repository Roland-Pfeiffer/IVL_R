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

Sys.setlocale("LC_ALL", "en_GB.UTF-8")

FPATH_ME <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/ME/Maturation quality ME 2021_2021-10-06.csv"
PLOT_FOLDER <- "/media/findux/DATA/Documents/IVL/Data/quality_oe_me/plots/"
TSTAMP <- format(now(), format = "_%Y-%m-%d_%H-%M")
SAVE_PLOTS <- FALSE
COLOR_PALETTE <- c("#00e6e6", "#00CCCC", "#009999", "#006666")
COLOR_PALETTE <- c("#00e6e6", "#00cccc", "#00b3b3", "#008080")
IVL_PALETTE <- c("#64cbce", "#37aaae", "#257274", "#1f5f61")
fpath_me_plot <- paste(PLOT_FOLDER, "ME_plots", TSTAMP, ".pdf", sep = "")

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
  format(format = "%b %d")
meq_data$sampling_date_short <- factor(format(meq_data$sampling_date, format = "%b %d"),
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

# Outlier tests
# meq_data <- meq_data[!meq_data$sample_id  %in% c(156, 159), ]


# Set starting values for all depths:
start_bulk <- meq_data[meq_data$sampling_date == "2021-03-22",]
for (depth in c(7, 15, 25)){
  new_bulk <- start_bulk
  new_bulk$depth_m <- depth
  meq_data <- rbind(meq_data, new_bulk)
}
rm(list = c("new_bulk", "start_bulk"))

# Add a "verbose" depth column
meq_data$depth_verbose <- factor(paste(meq_data$depth_m, "m"),
                                 levels = c("3 m", "7 m", "15 m", "25 m"))


Medians <- meq_data %>%
  group_by(depth_m, sampling_date) %>%
  summarise(median_value = median(dw_tissue)) %>%
  ungroup()


# Try "correcting" the cup weight
sel <- meq_data$depth_m == 15 & meq_data$cup_weight < 1
meq_data$cup_weight_corrected <- meq_data$cup_weight
message("Correcting ", sum(sel), " data points where cup weight was below 1.")
meq_data$cup_weight_corrected[sel] <- meq_data$cup_weight_corrected[sel] + 1
# Recalculate results
meq_data$dw_tissue_recalculated <- meq_data$dw_tissue_and_cup - meq_data$cup_weight_corrected
meq_data$ci_len_recalculated <- meq_data$dw_tissue_recalculated / meq_data$length_shell_mm * 100



# Use avg. cup weight
sel <- meq_data$sampling_date == "2021-06-01" & meq_data$depth_m == 15
meq_data$cup_weight_avg <- meq_data$cup_weight
meq_data$cup_weight_avg[sel] <- mean(meq_data$cup_weight[!sel])
meq_data$dw_tissue_using_cup_avg <- meq_data$dw_tissue_and_cup - meq_data$cup_weight_avg
meq_data$ci_len_using_cup_avg <- meq_data$dw_tissue_using_cup_avg / meq_data$length_shell_mm * 100


plot_ME_dw_tissue_nofacet <- ggplot(data = meq_data,
                              mapping = aes(x = depth_m, y = dw_tissue_recalculated, fill = sampling_date)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = sampling_date),
               outlier.shape = 21,
               outlier.colour = "black") +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("Depth (m)") + 
  ylab("Dry weight tissue (g)") +
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  guides(color=guide_legend(title="Sampling occasion"),
         fill=guide_legend(title="Sampling occasion")) # Customize legend title



plot_ME_dw_depth_faceted_IVL <- ggplot(data = meq_data,
                               mapping = aes(x = sampling_date, y = dw_tissue_recalculated, fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = depth_verbose),
               outlier.shape = 21,
               outlier.colour = "black",
               show.legend = TRUE) +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  # scale_fill_brewer(palette = "BrBG") +
  # scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7))) +
  scale_fill_manual(values = IVL_PALETTE) +
  guides(color=guide_legend(title="Depth (m)"),
         fill=guide_legend(title="Depth (m)")) # Customize legend title



plot_ME_dw_depth_faceted_monocol <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue_recalculated,
                                                           fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = depth_verbose),
               outlier.shape = 21,
               outlier.colour = "black") +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7))) +
  guides(color=guide_legend(title="Depth (m)"),
         fill=guide_legend(title="Depth (m)")) # Customize legend title



plot_ME_dw_depth_faceted_grey <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue_recalculated, fill = "#D9D9D9")) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7)))



plot_ME_dw_depth_faceted_nolegend <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue_recalculated,
                                                           fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = sampling_date),
               outlier.shape = 21,
               outlier.colour = "black",
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7)))
plot_ME_dw_depth_faceted_nolegend



plot_ME_DW_orig <- ggplot(data = meq_data, mapping = aes(x = sampling_date_short, y = dw_tissue,
                                                                   fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth. UNCORRECTED") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("\nSampling occasion (2021)") + 
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_ME_DW_depth_faceted


dodge_width <- 0.5
plot_ME_over_time <- ggplot(meq_data, aes(x = sampling_date_short, y = dw_tissue_recalculated, fill=depth_m)) +
  theme_bw() +
  geom_boxplot(width = 0.45, outlier.shape = 21, outlier.colour = "black",
               position = position_dodge(width = dodge_width)) +
  # geom_linerange(mapping = aes(colour = depth_m),
  #                stat = "summary",
  #                fun.min = function(z){quantile(z, 0.25)},
  #                fun.max = function(z){quantile(z, 0.75)},
  #                fun = median,
  #                size = 1.8, alpha = 0.8,
  #                position = position_dodge(width = dodge_width)) +
  geom_line(alpha = 0.2, size = 1.2, stat = "summary",
            mapping = aes(group = depth_m, colour = depth_m),
            fun = median,
            position = position_dodge(width = dodge_width)) +
  # geom_point(mapping = aes(colour=depth_m),
  #            stat = "summary", fun = median,
  #            position = position_dodge(width = dodge_width)) +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  # geom_smooth(mapping = aes(x = sampling_date, y = dw_tissue,
  #                           color=depth_m, group = depth_m),
  #             position = position_dodge(width = dodge_width)) +
  xlab("Sampling occasion") + 
  ylab("Dry weight tissue (g)") +
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  guides(color=guide_legend(title="Depth (m)"),
         fill=guide_legend(title="Depth (m)")) # Customize legend title


plot_ME_CI_orig <- ggplot(data = meq_data, 
                      mapping = aes(x = sampling_date_short, y = ci_len, fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE)+
  ggtitle(label = "*Mytilus edulis* condition index per sampling and depth. UNCORRECTED") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Condition index\n(DW tissue / shell length * 100)") +
  xlab("Sampling occasion") +
  facet_grid(.~depth_verbose) +
  scale_fill_brewer(palette = "BrBG") +
  guides(fill = guide_legend(title = "Sampling occasion")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_ME_CI_recalculated <- ggplot(data = meq_data, mapping = aes(x = sampling_date_short, y = ci_len_recalculated,
                                                                 fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* condition index per sampling and depth.",
          subtitle = "Using +1 correction where weight < 1.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Condition index\n(DW tissue / length)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

plot_ME_CI_w_cup_avg <- ggplot(data = meq_data, mapping = aes(x = sampling_date_short, y = ci_len_using_cup_avg,
                                                              fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* condition index over time per depth",
          subtitle = "Using avg 15m cup weights (excluding June 1st).") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Condition index\n(DW tissue / length)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# plot_ME_cup_weight <- ggplot(data = meq_data, 
#                       mapping = aes(x = sampling_date, y = cup_weight_corrected, fill = sampling_date)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21)+
#   ylab("Cup weight (g)") +
#   facet_grid(.~depth_verbose) +
#   scale_fill_brewer(palette = "BrBG") +
#   guides(fill = guide_legend(title = "Sampling occasion")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# plot_ME_cup_weight
# 
# 
# plot_corrected_cup_weight <- ggplot(data = meq_data, 
#                           mapping = aes(x = sampling_date, y = cup_weight_corrected, fill = sampling_date)) +
#   theme_bw() +
#   geom_boxplot(outlier.shape = 21)+
#   ylab("Cup weight (g)") +
#   facet_grid(.~depth_verbose) +
#   scale_fill_brewer(palette = "BrBG") +
#   guides(fill = guide_legend(title = "Sampling occasion")) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



plot_ME_DW_recalculated <- ggplot(data = meq_data, mapping = aes(x = sampling_date_short, y = dw_tissue_recalculated,
                                                                 fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = sampling_date),
               outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7)))

plot_ME_DW_w_cup_avg <- ggplot(data = meq_data, mapping = aes(x = sampling_date_short, y = dw_tissue_using_cup_avg, fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle(label = "*Mytilus edulis* dry weight over time per depth",
          subtitle = "Using avg 15m cup weights (excluding June 1st).") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Tissue dry weight (g)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot_ME_cups_orig <- ggplot(data = meq_data,
                         mapping = aes(x = sampling_date_short, y = cup_weight, fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  facet_grid(.~depth_verbose) +
  ggtitle("Cup weights (original)") +
  ylab("Cup weight (g)") +
  xlab("Sampling occasion") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")

plot_ME_cups_corrected <- ggplot(data = meq_data,
                         mapping = aes(x = sampling_date_short, y = cup_weight_corrected, fill = sampling_date_short)) +
  theme_bw() +
  facet_grid(.~depth_verbose) +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle("Cup weights (corrected)", "Corrected only where weight < 1.0 (i.e. only on June 1st)") +
  ylab("Corrected cup weight (g)") +
  xlab("Sampling occasion") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")

plot_ME_cups_avg <- ggplot(data = meq_data,
                                 mapping = aes(x = sampling_date_short, y = cup_weight_avg, fill = sampling_date_short)) +
  theme_bw() +
  facet_grid(.~depth_verbose) +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  ggtitle("Cup weights (corrected)", "Using avg 15m cup weights (excluding June 1st).") +
  ylab("Corrected cup weight (g)") +
  xlab("Sampling occasion") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_brewer(palette = "BrBG")




plot_ME_dw_final <- ggplot(data = meq_data,
                           mapping = aes(x = sampling_date_short, y = dw_tissue_recalculated, fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  facet_grid(.~depth_verbose) +
  ggtitle("*Mytilus edulis* dry weight over time per depth") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("\nSampling occasions (2021)") +
  ylab("Tissue dry weight (g)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
  scale_fill_brewer(palette = "BrBG")


plot_ME_CI_final <- ggplot(data = meq_data,
                           mapping = aes(x = sampling_date_short, y = ci_len_recalculated, fill = sampling_date_short)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               show.legend = FALSE) +
  facet_grid(.~depth_verbose) +
  ggtitle("*Mytilus edulis* condition index over time per depth") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("\nSampling occasions (2021)") +
  ylab("Condition index\n(DW tissue / shell length * 100)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
  scale_fill_brewer(palette = "BrBG")




grid.arrange(plot_ME_CI_orig,
             plot_ME_CI_recalculated,
             plot_ME_CI_w_cup_avg,
             ncol = 1)

grid.arrange(plot_ME_cups_orig,
             plot_ME_cups_corrected,
             plot_ME_cups_avg,
             ncol = 1)

grid.arrange(plot_ME_DW_orig,
             plot_ME_DW_recalculated,
             plot_ME_DW_w_cup_avg,
             ncol = 1)
