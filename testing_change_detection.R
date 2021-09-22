# Mark rows where the temperature change is above the specified threshold:


# # data_merged <- read.csv("file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-09-13.csv")
# data_merged <- read.csv("file:///media/findux/DATA/Documents/IVL/Data/test_data_for_change_detection.csv")
# TEMP_CANGE_THRESHOLD <- 1.0
# data_merged$datetime <- ymd_hms(data_merged$datetime)
# data_merged$depth_m <- as.factor(data_merged$depth_m)

# data_merged$change_above_threshold <- 0
# for (depth in unique(data_merged$depth_m)){
#   message("Processing depth ", depth, ". Please wait.")
#   n <- length(data_merged$temp[data_merged$depth_m == depth])
#   selection <- data_merged$depth_m == depth
#   
#   for (i in 2:n){
#     t1 <- data_merged$datetime[selection][i]
#     t0 <- data_merged$datetime[selection][i - 1]
#     time_diff <- difftime(t1, t0, units = "hours")
#     # Make sure you only look at change within one hour:
#     if (time_diff <= 1){
#       temp_diff <- data_merged[selection, ]$temp[i] - data_merged[selection, ]$temp[i - 1]
#       temp_diff <- sqrt(temp_diff ^ 2)
#       if (temp_diff > TEMP_CANGE_THRESHOLD){
#         data_merged[selection, ]$change_above_threshold[i] <- 1
#       }
#     }
#     if (i %% 250 == 0){
#       message("Processed ", i/n * 100, "% of points for depth ", depth, ".", " (", i, " points.")
#     }
#   }
#   message("Depth ", depth, " done. (", i, " points processed.)")
# }


# a <- ymd_hms("2021-08-09 21:00:00")
# b <- ymd_hms("2021-08-09 22:00:00")
# c <- ymd_hms("2021-08-09 22:30:00")
# difftime(b, c, units = "hours")

for (depth in unique(data_merged$depth_m)){
  selection <- data_merged$depth_m == depth
  t0 <- subset(data_merged, selection, select = c(datetime, id))
}
[selection, ][1:n-1]
t1 <- data_merged$datetime[selection][2:n]

# TODO: this needs to carry over an id!

difftable <- tibble(t0, t1)
difftable$timedelta <- difftime(difftable$t1, difftable$t0, units = "hours")
