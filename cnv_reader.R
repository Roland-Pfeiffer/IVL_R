cat("\014")
rm(list = ls())

# install.packages("oce") # requires libudunits2-dev and libgdal-dev
library(ggplot2)
library(lubridate)
library(oce)
library(tidyr)


fpath_slaggo_raw <- "/media/findux/DATA/Documents/IVL/Data/slaggo_data/raw/"
save_path <- "/media/findux/DATA/Documents/IVL/Data/slaggo_data/"
depth_lvls <- c(3, 7, 15, 25)

t_now <- format(now(), format = "%Y-%m-%d_%H-%M")


# --------------- FUNCTIONS ----------------------------------------------------

extract_oce_data <- function(fpath){
  message(paste("Running on file", fpath))
  oce_object <- tryCatch(read.oce(fpath), error = function(e) e)
  data_out <- NA
  if (inherits(oce_object, "error")){
    message("READ ERROR")
  } else {
    salinity <- oce_object[["salinity"]]
    depth_m <- oce_object[["depth"]]
    t_start <- oce_object[["startTime"]]
    if (!is.na(t_start)){
      datetime <- seq.POSIXt(t_start, t_start + length(salinity) - 1, by = "sec")
      data_out <- data.frame(datetime,depth_m, salinity)
    }
  }
  data_out
}


get_closest_level <- function(d){
  lvls <- c(3, 7, 15, 25)
  lvl_out <- lvls[which.min(abs(lvls - d))]
  lvl_out
}





# -------------------------- SCRIPT --------------------------------------------

slaggo_temp <- data.frame()

for (fname in list.files(fpath_slaggo_raw)){
  full_path <- paste(fpath_slaggo_raw, fname, sep = "")
  tmp <- extract_oce_data(full_path)
  slaggo_temp <- rbind(slaggo_temp, tmp)
}

slaggo_temp$sampling_day <- as.character(format(slaggo_temp$datetime, format = "%Y-%m-%d"))
slaggo_temp <- slaggo_temp[order(slaggo_temp$datetime), ]
slaggo_temp <- drop_na(slaggo_temp)

slaggo_final <- data.frame()

# Extract the depth values that are closest to the depth levels used, and pool
# those in the final data frame.
for (sampling_date in unique(slaggo_temp$sampling_day)){
  for (lvl in depth_lvls){
    min_i <- which.min(abs(slaggo_temp$depth_m[slaggo_temp$sampling_day == sampling_date] - lvl))
    found_depth <- slaggo_temp$depth_m[slaggo_temp$sampling_day == sampling_date][min_i]
    if (abs(found_depth - lvl) < 1){
      tstamp <- slaggo_temp$datetime[slaggo_temp$sampling_day == sampling_date][min_i]
      sal_val <- slaggo_temp$salinity[slaggo_temp$sampling_day == sampling_date][min_i]
      line_out <- data.frame(tstamp, as.integer(found_depth), sal_val)
      names(line_out) <- c("datetime", "depth_m", "sal_PSU")
      slaggo_final <- rbind(slaggo_final, line_out)
    }
  }
}

# Reassign depth levels according to closest desired level.
slaggo_final$depth_m <- sapply(slaggo_final$depth_m, FUN = get_closest_level)
slaggo_final$depth_m <- as.factor(slaggo_final$depth_m)


write.csv(slaggo_final, paste(save_path, "Slaggo_re-merged_", t_now, ".csv", sep = ""),
          row.names = FALSE)
