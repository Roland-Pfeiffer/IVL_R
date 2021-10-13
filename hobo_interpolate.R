dev.off()
cat("\014")
rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(imputeTS)

FPATH <- "file:///media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data_merged_2021-10-01_16-48.csv"
# FPATH <- "file:///media/findux/DATA/R/IVL/test_data_time_rounding.csv"
SAVE_LOCATION <- "/media/findux/DATA/Documents/IVL/Data/hobo_out/"

dtnow <- as.character(format(now(), format = "%Y-%m-%d_%H-%M"))
save_path_long <- paste(SAVE_LOCATION, "hobo_data_merged_interpolated_", dtnow, ".csv", sep = "")
save_path_wide <- paste(SAVE_LOCATION, "hobo_data_merged_interpolated_wide_", dtnow, ".csv", sep = "")

# Load and fix format
hobo_data <- read.csv(FPATH) %>%
  dplyr::mutate(datetime = ymd_hms(datetime)) %>%
  dplyr::mutate(depth_m = as.factor(depth_m)) %>%
  subset(select = -c(conductivity_raw)) %>%
  dplyr::mutate(datetime = ymd_hms(round(datetime, "hour"))) %>%
  dplyr::mutate(id = paste(datetime, depth_m)) %>%
  plyr::mutate(id = str_replace_all(id, " ", "_"))

# Drop possible duplicates introduced by the time rounding (unlikely because of
# the aggregation during data preparation though).
n0 <- dim(hobo_data)[1]
hobo_data <- dplyr::distinct(hobo_data, datetime, depth_m, .keep_all = TRUE)
n1 <- dim(hobo_data)[1]
message("Dropped ", n1 - n0, " rows.")


# -------------------- CREATE EMPTY CONTINUOUS DF ------------------------------

full_frame <- data.frame()
dtime_start <- ymd_hms(min(hobo_data$datetime))
dtime_stop <- ymd_hms(max(hobo_data$datetime))
# tdiff <- difftime(dtime_stop, dtime_start, units = "hours")[1]
# tdiff_hrs <- as.numeric(tdiff)
datetime <- seq(dtime_start, dtime_stop, by = "hours")
for (depth in c(3, 7, 15, 25)){
  df_tmp <- data.frame(datetime)
  df_tmp$depth_m <- depth
  df_tmp$temp <- NA_real_
  full_frame <- rbind(full_frame, df_tmp)
}

full_frame %<>%
  dplyr::mutate(id = paste(datetime, depth_m, sep = "_")) %>%
  dplyr::mutate(id = str_replace_all(id, " ", "_")) %>%
  dplyr::mutate(depth_m = as.factor(depth_m))

# Drop rows contained in hobo_data
n0 <- dim(full_frame)[1]
full_frame <- full_frame[!full_frame$id %in% hobo_data$id, ]
n1 <- dim(full_frame)[1]
message("Dropped ", n0 - n1, " rows.")

message("Same colnames: ", all(names(hobo_data) == names(full_frame)))


# ---------------------- MERGE DATA & INTERPOLATE ------------------------------

hobo_data_continuous <- rbind(hobo_data, full_frame)
hobo_data_continuous <- hobo_data_continuous[order(hobo_data_continuous$datetime), ]

# Interpolate
hobo_data_continuous %<>%
  group_by(depth_m) %>%
  dplyr::mutate(temp_interpolated = na_interpolation(temp, option = "linear"))%>%
  dplyr::ungroup()


# ---------------------------- PLOT RESULTS ------------------------------------

ggplot() +
  facet_grid(depth_m~.) +
  geom_line(data = na.omit(hobo_data_continuous),
            mapping = aes(x = datetime, y = temp, color = depth_m),
            size = 0.2) +
  geom_point(data = hobo_data_continuous,
             alpha = 0.2,
             mapping = aes(x = datetime, y = temp_interpolated, colour = depth_m))


# ----------------------------- SAVE RESULTS -----------------------------------

# data_out <- hobo_data_continuous
# data_out <- subset(data_out, select = -c(temp, id))
# data_out$datetime <- as.character(data_out$datetime)
# write.csv(data_out, save_path_long, row.names = FALSE)
# 
# # Save wide format
# data_out_wide <- reshape(data_out, idvar = "datetime", timevar = "depth_m",
#                      direction = "wide")
# names(data_out_wide) <- str_replace_all(names(data_out_wide), "\\.", "_")
# write.csv(data_out_wide, save_path_wide, row.names = FALSE)
