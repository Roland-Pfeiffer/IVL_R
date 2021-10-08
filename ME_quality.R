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

FPATH_ME <- "file:///media/findux/DATA/Documents/IVL/Data/quality_oe_me/ME/Maturation quality ME 2021_2021-10-06.csv"
PLOT_FOLDER <- "/media/findux/DATA/Documents/IVL/Data/quality_oe_me/plots/"
TSTAMP <- format(now(), format = "_%Y-%m-%d_%H-%M")
SAVE_PLOTS <- FALSE
COLOR_PALETTE <- c("#00e6e6", "#00CCCC", "#009999", "#006666")
COLOR_PALETTE <- c("#00e6e6", "#00cccc", "#00b3b3", "#008080")
IVL_PALETTE <- c("#64cbce", "#37aaae", "#257274", "#1f5f61")
fpath_me_plot <- paste(PLOT_FOLDER, "ME_plots", TSTAMP, ".pdf", sep = "")

meq_data <- read.csv(FPATH_ME, check.names = TRUE)

# names(meq_data)

# Prepping ME
names(meq_data)[names(meq_data) == "Date.sampled"] <- "sampling_date"
names(meq_data)[names(meq_data) == "Depth..m."] <- "depth_m"
names(meq_data)[names(meq_data) == "CI"] <- "ci_len"
names(meq_data)[names(meq_data) == "Tissue.DW.without.cup"] <- "dw_tissue"
names(meq_data)[names(meq_data) == "Depth..mm."] <- "depth_shell_mm"
names(meq_data)[names(meq_data) == "Sample.ID"] <- "sample_id"

# Add hyphens in the dates
meq_data$sampling_date <- paste(substring(meq_data$sampling_date, 1, 4),
                                substring(meq_data$sampling_date, 5, 6),
                                substring(meq_data$sampling_date, 7, 8),
                                sep = "-")

# Fix classes
meq_data$depth_m <- as.numeric(meq_data$depth_m)
meq_data$sampling_date<- as.factor(meq_data$sampling_date)
meq_data$ci_len <- as.numeric(meq_data$ci_len)
meq_data$depth_m <- factor(meq_data$depth_m, levels = c(3, 7, 15, 25))
meq_data$dw_tissue <- as.numeric(meq_data$dw_tissue)

meq_data <- subset(meq_data, select = c(sampling_date, sample_id, depth_m, dw_tissue))
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

# me_date_v_cil <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = ci_len, fill=depth_m)) +
#   geom_boxplot() +
#   ggtitle("ME CI (length)") + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# me_depth_v_cil <- ggplot(data = meq_data, mapping = aes(x = depth_m, y = ci_len, fill=sampling_date)) +
#   geom_boxplot() +
#   ggtitle("ME CI (length)")



over_depth_graph_beautified <- ggplot(data = meq_data,
                              mapping = aes(x = depth_m,
                                            y = dw_tissue,
                                            fill = sampling_date)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = sampling_date),
               outlier.shape = 21,
               outlier.colour = "black") +
  ggtitle(label = "*M. edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  xlab("Depth (m)") + 
  ylab("Dry weight tissue (g)") +
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  guides(color=guide_legend(title="Sampling occasion"),
         fill=guide_legend(title="Sampling occasion")) # Customize legend title
over_depth_graph_beautified



dw_depth_faceted_IVL <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue,
                                                           fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = depth_verbose),
               outlier.shape = 21,
               outlier.colour = "black",
               show.legend = FALSE) +
  ggtitle(label = "*M. edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  # scale_fill_brewer(palette = "BrBG") +
  # scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7))) +
  scale_fill_manual(values = IVL_PALETTE) +
  guides(color=guide_legend(title="Sampling occasion"),
         fill=guide_legend(title="Sampling occasion")) # Customize legend title
dw_depth_faceted_IVL


dw_depth_faceted_monocol <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue,
                                                           fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = depth_verbose),
               outlier.shape = 21,
               outlier.colour = "black",
               show.legend = FALSE) +
  ggtitle(label = "*M. edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7))) +
  guides(color=guide_legend(title="Sampling occasion"),
         fill=guide_legend(title="Sampling occasion")) # Customize legend title
dw_depth_faceted_monocol


dw_depth_faceted_monocol <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue)) +
  theme_bw() +
  geom_boxplot(outlier.shape = 21,
               outlier.colour = "black",
               show.legend = FALSE,
               fill = "#D9D9D9") +
  ggtitle(label = "*M. edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7)))
dw_depth_faceted_monocol


dw_depth_faceted_nolegend <- ggplot(data = meq_data, mapping = aes(x = sampling_date, y = dw_tissue,
                                                           fill = depth_verbose)) +
  theme_bw() +
  geom_boxplot(mapping = aes(fill = sampling_date),
               outlier.shape = 21,
               outlier.colour = "black",
               show.legend = FALSE) +
  ggtitle(label = "*M. edulis* tissue dry weight per sampling and depth.") +
  theme(plot.title = ggtext::element_markdown()) +
  ylab("Dry weight tissue (g)") +
  xlab("Sampling occasion") + 
  scale_fill_brewer(palette = "BrBG") +
  scale_colour_brewer(palette = "BrBG") +
  facet_grid(.~depth_verbose, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 7))) +
  guides(color=guide_legend(title="Sampling occasion"),
         fill=guide_legend(title="Sampling occasion")) # Customize legend title
dw_depth_faceted_nolegend


dodge_width <- 0.5
over_time_graph_beautified <- ggplot(meq_data, aes(x = sampling_date,
                                                   y = dw_tissue,
                                                   fill=depth_m)) +
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
  geom_point(mapping = aes(colour=depth_m),
             stat = "summary", fun = median,
             position = position_dodge(width = dodge_width)) +
  ggtitle(label = "*M. edulis* tissue dry weight per sampling and depth.") +
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
over_time_graph_beautified
