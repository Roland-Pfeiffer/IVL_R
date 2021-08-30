rm(list=ls())
cat("\014")

library(ggplot2)
library(reshape)
library(gridExtra)

fname <- "/media/findux/DATA/Documents/IVL/Data/disturbance_results.csv"

# Read data
  data <- read.csv(fname)
  data$Time <- as.factor(data$Time)

# Summarize data
  start <- aggregate(x=data[data$Time == "Start", ], by=list(data[data$Time == "Start", ]$Treatment), FUN=mean)
  stop <- aggregate(x=data[data$Time == "Stop", ], by=list(data[data$Time == "Stop", ]$Treatment), FUN=mean)

# Adjust column names
  colnames_start <- colnames(start)
  for (i in 5:length(colnames_start)){
    colnames_start[i] <- paste(colnames_start[i], "start", sep = "_")
  }
  
  colnames_stop <- colnames(stop)
  for (i in 5:length(colnames_stop)){
    colnames_stop[i] <- paste(colnames_stop[i], "stop", sep = "_")
  }
  
  colnames(start) <- colnames_start
  colnames(stop) <- colnames_stop

# Merge summarized data
  results_merged <- merge(start, stop)

# Calc differences
  results_merged$D_diff <-results_merged$D_stop - results_merged$D_start
  results_merged$L_diff <-results_merged$L_stop - results_merged$L_start
  results_merged$W_diff <-results_merged$W_stop - results_merged$W_start
  results_merged$Weight_diff <- results_merged$Weight_stop - results_merged$Weight_start
  
  results_merged$D_rel_diff <- (results_merged$D_stop - results_merged$D_start) * 100 / results_merged$D_start
  results_merged$L_rel_diff <- (results_merged$L_stop - results_merged$L_start) * 100 / results_merged$L_start
  results_merged$W_rel_diff <- (results_merged$W_stop - results_merged$W_start) * 100 / results_merged$W_start
  results_merged$Weight_rel_diff <- (results_merged$Weight_stop - results_merged$Weight_start) * 100 / results_merged$Weight_start

# Drop irrelevant data  
  results_merged <- subset(results_merged, select = -c(Time, Cage, Treatment,
                                                       Weight_start, Weight_stop,
                                                       D_start, D_stop,
                                                       L_start, L_stop,
                                                       W_start, W_stop))

# T-tests (not sure if this is the way to go)
  start_weight_ttest <- t.test(x=data$Weight[data$Time == "Start" & data$Treatment == 'Disturbed'],
                               y=data$Weight[data$Time == "Start" & data$Treatment == 'Undisturbed'])
  stop_weight_ttest <- t.test(x=data$Weight[data$Time == "Stop" & data$Treatment == 'Disturbed'],
                               y=data$Weight[data$Time == "Stop" & data$Treatment == 'Undisturbed'])

# Plot overall len/weight ratio
  L_v_weight <- ggplot(data=data, aes(x=L, y=Weight, color=Treatment)) +
    geom_point() +
    ggtitle('Length vs. weight') + xlab('Length (mm)') + ylab('Weight (g)')

# Plot comparing len/weight ratio between start and end
  LW_start <- ggplot(data = data[data$Time=='Start',],
                     mapping = aes(x = L, y = Weight, color = Treatment)) +
    geom_point() + ggtitle('Start measurements') +
    xlab('Length (mm)') + ylab('Weight (g)')
  LW_stop <- ggplot(data = data[data$Time=='Stop',],
                    mapping = aes(x = L, y = Weight, color = Treatment)) +
    geom_point() + ggtitle('End measurements') +
    xlab('Length (mm)') + ylab('Weight (g)')
  

# Change in weight comparison
  disturbance_difference_weight <- ggplot(data = data, mapping = aes(x=Time, y = Weight, fill = Treatment)) +
    geom_boxplot() + 
    ggtitle('Weight (disturbance experiments)', ) + xlab('Time') + ylab('Weight (g)')

# Change in weight/length ratio
  disturbance_difference_Lweight <- ggplot(data = data, mapping = aes(x=Time, y = (Weight / L), fill = Treatment)) +
    geom_boxplot() + ggtitle('Length/weight ratio (disturbance experiments)',
                             'Note: displays median, not mean.')

# Comparing mean values:
  mean_vals <- aggregate(data, by = list(data$Time, data$Treatment), FUN = mean)
  mean_vals <- mean_vals[, -(3:5)] # dropping unimportant columns
  colnames(mean_vals)[1:2] <- c('Time', 'Treatment')

# Summarizing plot weight
  mean_change_plot_weight <- ggplot(mapping = aes(x=Time,
                                           y=Weight,
                                           color=Treatment)) +
    geom_line(data=mean_vals[mean_vals$Treatment=="Disturbed", ],
              mapping=aes(x = Time, y = Weight,
                          group = 1, # Necessary to not have just two points
                          color = Treatment)) +
    geom_line(data=mean_vals[mean_vals$Treatment=="Undisturbed", ],
              mapping=aes(x = Time, y = Weight,
                          group = 1, # Necessary to not have just two points
                          color = Treatment)) +
    geom_boxplot(data=data, mapping = aes(x=Time, y=Weight,
                                          fill = Treatment),
                 alpha = 0.5, outlier.shape=NA) +
    geom_point(data = data, position=position_jitterdodge(jitter.width = 0.3)) + 
    #geom_violin(data = data, mapping = aes(x=Time, y=Weight, fill='grey'), alpha=0.2) +
    xlab('Time') + ylab('Weight (g)') + 
    ggtitle('Disturbance data comparison.', 'Note: Diagonal lines indicate change between group means, boxplots indicate group medians. ')

# Summarizing plot Length
  mean_change_plot_length <- ggplot(mapping = aes(x=Time,
                                                  y=L,
                                                  color=Treatment)) +
    geom_line(data=mean_vals[mean_vals$Treatment=="Disturbed", ],
              mapping=aes(x = Time, y = L,
                          group = 1, # Necessary to not have just two points
                          color = Treatment)) +
    geom_line(data=mean_vals[mean_vals$Treatment=="Undisturbed", ],
              mapping=aes(x = Time, y = L,
                          group = 1, # Necessary to not have just two points
                          color = Treatment)) +
    geom_boxplot(data=data, mapping = aes(x=Time, y=L,
                                          fill = Treatment),
                 alpha = 0.5, outlier.shape=NA) +
    geom_point(data = data, position=position_jitterdodge(jitter.width = 0.3)) + 
    #geom_violin(data = data, mapping = aes(x=Time, y=Weight, fill='grey'), alpha=0.2) +
    xlab('Time') + ylab('Length (mm)') + 
    ggtitle('Disturbance data comparison.', 'Note: Diagonal lines indicate change between group means, boxplots indicate group medians. ')

# Summarizing plot Weight/Length ratio
  mean_change_plot_weight_length_ratio <- ggplot(mapping = aes(x=Time,
                                                  y=Weight/L,
                                                  color=Treatment)) +
    geom_line(data=mean_vals[mean_vals$Treatment=="Disturbed", ],
              mapping=aes(x = Time, y = Weight/L,
                          group = 1, # Necessary to not have just two points
                          color = Treatment)) +
    geom_line(data=mean_vals[mean_vals$Treatment=="Undisturbed", ],
              mapping=aes(x = Time, y = Weight/L,
                          group = 1, # Necessary to not have just two points
                          color = Treatment)) +
    geom_boxplot(data=data, mapping = aes(x=Time, y=Weight/L,
                                          fill = Treatment),
                 alpha = 0.5, outlier.shape=NA) +
    geom_point(data = data, position=position_jitterdodge(jitter.width = 0.3)) + 
    #geom_violin(data = data, mapping = aes(x=Time, y=Weight, fill='grey'), alpha=0.2) +
    xlab('Time') + ylab('Weight (g) / Length (mm) ratio') + 
    ggtitle('Disturbance data comparison.', 'Note: Diagonal lines indicate change between group means, boxplots indicate group medians. ')
  
  
  # head(data)
  # results_merged
  # mean_vals
  
  # start_weight_ttestA
  # stop_weight_ttest
  # 
  L_v_weight
  grid.arrange(LW_start, LW_stop, nrow=1)
  # disturbance_difference_weight
  # disturbance_difference_Lweight
  mean_change_plot_weight
  mean_change_plot_length
  mean_change_plot_weight_length_ratio