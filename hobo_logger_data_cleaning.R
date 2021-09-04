rm(list=ls())
cat("\014") 

library(tidyverse)
library(stringr)  # Check if this also works on WIN. Otherwise use "stringi"
library(tcltk)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)


add_gmt_offset <- function(data_df){
  gmt_offset <- extract_gmt_format(data_df)
  offset_numstr <-substr(gmt_offset, 4,6)
  gmt_offset_num <- as.numeric(offset_numstr)
  data_df$gmt_offset <- gmt_offset_num
  data_df
}

adjust_datetime <- function(dframe){
  dframe$gmt_correction_sec <- (2 - dframe$gmt_offset) * 60 * 60
  dframe$datetime <- as.POSIXct(dframe$datetime, format="%m/%d/%y %I:%M:%S %p", tz = "Europe/Paris")
  dframe$datetime <- dframe$datetime + dframe$gmt_correction_sec
  dframe$datetime <- as.POSIXct(dframe$datetime, format="%m/%d/%y %H:%M:%S")
  dframe
}


convert_f_to_c <- function(deg_F){
  # Formula from:
  # https://www.metric-conversions.org/sv/temperatur/fahrenheit-till-celsius.htm
  deg_C <- (deg_F - 32) / 1.8
  deg_C
}


depth_as_number <- function(depth){
  return(as.numeric(gsub("([0-9]+).*$", "\\1", depth))) # Copied from the internet
}


extract_depth <- function(fname) {
  fname_lower <- str_to_lower(fname)
  re_match <- str_extract(fname_lower, "\\d{1,2}m")
  re_match_surface <- str_extract(fname_lower, "surface")
  if (!is.na(re_match_surface)){
    return("3m")
  } else{
    return(re_match)
  }}


extract_gmt_format <- function(data_df){
  for (cname in colnames(data_df)){
    if (startsWith(cname, "Date Time") || startsWith(cname, "datetime")){
      dt_match_str <- str_extract(cname, "GMT[+|-]\\d\\d:\\d\\d")
    }}
  dt_match_str
}


fill_cond_if_missing <- function(dframe){
  # This function does not work when used in pipes, for whatever reason.
  if ("conductivity_raw" %in% colnames(dframe)){
    # (Do nothing, conductivity column already exists)
  } else {
    # Create a conductivity column filled with NA.
    dframe$conductivity_raw <- as.numeric(NA)
  }
  dframe
}


fm_pm_to_AM_FM <- function(df){
  df$datetime <- str_replace(df$datetime, "em", "PM")
  df$datetime <- str_replace(df$datetime, "fm", "AM")
  return(df)
}


fname_contains_depth <- function(fname_str) {
  fname_lower <- str_to_lower(fname_str)
  re_match <- str_extract(fname_lower, "\\d{1,2}m")  # TODO: Check if this 1,2 is still ok
  re_match_surface <- str_extract(fname_lower, "surface")
  if (is.na(re_match) && is.na(re_match_surface)){
    FALSE
  } else{
    TRUE
  }}


get_temp_unit <- function(dframe){
  temp_cname <- get_colname(dframe, "Temp")
  temp_unit <- str_match(temp_cname, "Temp, °(F|C)")[,2]
  temp_unit
}


homogenize_colnames <- function(cnames){
  cond_name <- NA  # Necessary for later in the pipe below so cond. can be skipped
  for (cname in cnames){
    if (startsWith(cname, "Temp")){
      temp_name <- cname
    } else if (startsWith(cname, "High Range")){
      cond_name <- cname
    } else if (startsWith(cname, "Date Time")){
      datetime_name <- cname 
    }
  }
  
  # Replace column names
  cnames %>% 
    replace(which(cnames == temp_name), "temp") %>%
    replace(which(cnames == cond_name), "conductivity_raw") %>% # This skips if there's no cond.
    replace(which(cnames == datetime_name), "datetime") %>%
    return()  # works w/o argument because of pipe.
}


missing_conductivity <- function(df){
  conductivity_missing <- TRUE
  for (cname in colnames(df)){
    message(cname)
    if (startsWith(cname, "High Range") || cname == "conductivity_raw"){
      conductivity_missing <- FALSE
    }
  }
  conductivity_missing
}


get_colname <- function(dframe, name_start_str){
  out <- NA
  for (cname in colnames(dframe)){
    if (startsWith(cname, name_start_str)){
      out <- cname
    }
  }
  out
}





# -------------------------------- SCRIPT SECTION ------------------------------

# fpath_browse <- tk_choose.dir("Select folder with HOBO logger .csv files.")
PATH_TO_FILES <- "/media/findux/DATA/Documents/IVL/Data/HOBO logger data 2020"
SAVE_LOCATION <- "/media/findux/DATA/Documents/IVL/Data"
COND_THRESHOLD <- 400


DELETE_OUTLIERS <- TRUE
FNAME_OUTLIERS <- "file:///media/findux/DATA/Documents/IVL/Data/outliers.csv"

# Colors:
C03 <- "#33CCCC"
C07 <- "#009999"
C15 <- "#006666"
C25 <- "#336666"


# Initiate data frames
# data_03_m <- data.frame()
# data_07_m <- data.frame()
# data_15_m <- data.frame()
# data_25_m <- data.frame()
data_merged <- data.frame()
skipped_files <- list()
processed_files <- list()

# Read filenames
fnames <- list.files(PATH_TO_FILES)

# Gobble up chaotically named data:
for (fname in fnames){
  if (endsWith(fname, ".csv")){
    processed_files <- c(processed_files, fname)
    if (fname_contains_depth(fname)){
      depth <- extract_depth(fname)
      depth_num <- depth_as_number(depth)
      fpath <- paste(PATH_TO_FILES, fname, sep = "/")
      message('Depth: ', depth, '\tFile: ', fname)
      
      # TODO: fix the error treatment
      possible_read_error <- tryCatch(data_tmp <- read.csv(fpath, skip = 1, check.names = FALSE),
                                      error = function(e) {skipped_files <<- c(skipped_files, fname)})  # TODO: this does not work yet...
      if (!inherits(possible_read_error, "error")){  # TODO: this does not escape errors.
        temperature_units <- get_temp_unit(data_tmp)
        
        
        # Adjust the column names for datetime, temp, 
        data_tmp <- add_gmt_offset(data_tmp)
        colnames(data_tmp) <- homogenize_colnames(colnames(data_tmp))
        
        if (temperature_units == "F"){
          message("(Temperature is in Fahrenheit and will be converted to Celsius.)")
          data_tmp$temp <- convert_f_to_c(data_tmp$temp)
        }
        
        # print(head(data_tmp))
        data_tmp <- fill_cond_if_missing(data_tmp)
        data_tmp$conductivity_raw <- as.numeric(data_tmp$conductivity_raw)
        data_tmp <- select(data_tmp, datetime, temp, conductivity_raw, gmt_offset)
        data_tmp$depth_m <- depth_num
        data_tmp$file <- fname
        
        # if (depth == "3m"){
        #   data_03_m <- rbind(data_03_m, data_tmp)
        # } else if (depth == "7m") {
        #   data_07_m <- rbind(data_07_m, data_tmp)
        # } else if (depth == "15m") {
        #   data_15_m <- rbind(data_15_m, data_tmp)
        # } else if (depth == "25m") {
        #   data_25_m <- rbind(data_25_m, data_tmp)
        # }
        data_merged <- rbind(data_merged, data_tmp)
      } else {
        # Error case
      }
    } else {
      skipped_files <- c(skipped_files, fname)
    }}}
rm(list=c("data_tmp",
          "possible_read_error"))
# Adjust datetime
data_merged <- fm_pm_to_AM_FM(data_merged) # Turn fm/em into AM/PM
data_merged <- adjust_datetime(data_merged)


# Drop columns no longer needed
data_merged <- subset(data_merged, select = -c(gmt_offset, gmt_correction_sec))

# Treat depth as factor 
data_merged$depth_m <- as.factor(data_merged$depth_m)

# Remove duplicates
len_old <- dim(data_merged)[1]
data_merged <- distinct(data_merged, datetime, temp, conductivity_raw, depth_m,
                        .keep_all = TRUE)
len_new <- dim(data_merged)[1]
message("Removed ", len_old - len_new, " duplicate rows.")

# Plot raw data
plot_raw <- ggplot(data=data_merged, mapping = aes(x=datetime, y=temp, color=depth_m)) +
  geom_line() + 
  ggtitle("Raw data")

# Plot raw
plot_ylims <- c(min(data_merged$temp), max(data_merged$temp))
plot_03m_raw <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C03 ) +
  ggtitle("03 m raw") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_07m_raw <- ggplot(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C07 ) +
  ggtitle("07 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_15m_raw <- ggplot(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C15) +
  ggtitle("15 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_25m_raw <- ggplot(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C25 ) +
  ggtitle("25 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)

data_raw <- data_merged


# testplot_01 <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color=C03 ) +
#   ggtitle("03 m raw") + xlab("Time") + ylab("Temp. (°C)") +
#   xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(c(-1, 25))

# Remove conductivity below threshold rows, but keep the ones w/ NA
c0 <- dim(data_merged)[1]
cond_above_threshold <- data_merged[data_merged$conductivity_raw > COND_THRESHOLD, ]
cond_missing <- data_merged[is.na(data_merged$conductivity_raw), ]
data_merged <- rbind(cond_above_threshold, cond_missing)
c1 <- dim(data_merged)[1]
message("Removed ", c0 - c1, " rows where conductivity <= ", COND_THRESHOLD)

# testplot_02 <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color=C03 ) +
#   ggtitle("03 m raw") + xlab("Time") + ylab("Temp. (°C)") +
#   xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(c(-1, 25))
# # grid.arrange(testplot_01, testplot_02, ncol=1)



# Remove rows with NA in temperature:
temp_na_0 <- dim(data_merged)[1]
data_merged <- data_merged[!is.na(data_merged$temp), ]
temp_na_1 <- dim(data_merged)[1]
message("Removed ", temp_na_0 - temp_na_1, " rows where temp = NA.")


# Delete manually marked outliers
if (DELETE_OUTLIERS){
  data_merged$id <- paste(as.character(data_merged$datetime), data_merged$depth_m, sep = "_")
  d1 <- dim(data_merged)[1]
  outliers <- read.csv(FNAME_OUTLIERS)
  outlier_ids <- outliers$id
  outlier_indices <- c()
  for (id_o in outlier_ids){
    if (id_o %in% data_merged$id){
      cur_loc <- which(data_merged$id == id_o)
      outlier_indices <- c(outlier_indices, cur_loc)
    }
  }
  data_merged <- data_merged[-c(outlier_indices),]
  d2 <- dim(data_merged)[1]
  message("Dropped ", d1 - d2, " rows of outliers.")
}

# Sort the data by datetime
data_merged <- data_merged[order(data_merged$datetime),]

# Add a numerical timestamp
data_merged$month <- month(data_merged$datetime)
data_merged$year <- year(data_merged$datetime)
data_merged$day <- day(data_merged$datetime)
data_merged$hour <- hour(data_merged$datetime)
data_merged$timenum <- data_merged$year * 1000000 + data_merged$month * 10000 + data_merged$day * 100 + data_merged$hour


# # Aggregate by datetime
# conductivity <- subset(data_merged, select = c(conductivity_raw, timenum))
# conductivity$conductivity_raw[is.na(conductivity$conductivity_raw)] <- 0
# conductivity <- aggregate(conductivity, by = list(conductivity$timenum), FUN = "sum")
# 
# data_merged <- subset(data_merged, select = -c(file, id, year, month, day, hour))
# data_merged$depth_m <- as.numeric(data_merged$depth_m)
# data_merged$conductivity_raw[is.na(data_merged$conductivity_raw)] <- -99999
data_merged <- aggregate(data_merged, by = list(data_merged$datetime, data_merged$depth_m), FUN = "mean")
data_merged <- subset(data_merged, select = -c(depth_m, datetime))
names(data_merged)[names(data_merged) == 'Group.2'] <- 'depth_m'
names(data_merged)[names(data_merged) == 'Group.1'] <- 'datetime'
data_merged$datetime <- as.POSIXct(data_merged$datetime, format = "%Y-%m-%d %H:%M:%S")

data_wide <- reshape(data_merged, idvar = "datetime", timevar = "depth_m", direction = "wide")
colnames(data_wide) <- str_replace(colnames(data_wide), "\\.", "_")


# Plotting 
plot_ylims <- c(min(data_merged$temp), max(data_merged$temp))
plot_all <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line() + 
  ggtitle("All depths")
plot_03m <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C03) +
  ggtitle(label = "03 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_07m <- ggplot(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C07 ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C15) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_25m <- ggplot(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color=C25 ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)



# Show info about processed files and removed duplicates:
if (length(skipped_files) > 0){
  for (fname in skipped_files){
    message("Skipped csv file: ", fname)
  }
}
n_processed <- length(processed_files)
n_skipped <- length(skipped_files)
message("Processed ", length(processed_files), " files.\nMerged ", n_processed - n_skipped, " files (", n_skipped, " skipped).")
message("Removed ", len_old - len_new, " duplicate rows.")


plot_comparison_03 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 3, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "03 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_comparison_07 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 7, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "07 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_comparison_15 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 15, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "15 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)
plot_comparison_25 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 25, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "25 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(c(min(data_merged$datetime), max(data_merged$datetime))) + ylim(plot_ylims)


# Save files
data_merged$datetime <- as.character(data_merged$datetime)
data_wide$datetime <- as.character(data_wide$datetime)
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
write_csv(data_merged, paste(SAVE_LOCATION, "/hobo_data_merged_", date_now, ".csv", sep = ""))
write_csv(data_wide, paste(SAVE_LOCATION, "/hobo_data_wide_", date_now, ".csv", sep = ""))

# Save raw
data_raw$datetime <- as.character(data_raw$datetime)
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
write_csv(data_raw, paste(SAVE_LOCATION, "/hobo_data_raw_", date_now, ".csv", sep = ""))




#grid.arrange(plot_03m_raw, plot_07m_raw, plot_15m_raw, plot_25m_raw, ncol=1)
grid.arrange(plot_03m, plot_07m, plot_15m, plot_25m, ncol=1)
grid.arrange(plot_comparison_03, plot_comparison_07, plot_comparison_15, plot_comparison_25, ncol=1)
# plot_03m
# plot_07m
# plot_15m
# plot_25m
 # plot_raw
# plot_all
# ggsave(paste(SAVE_LOCATION, "plot_all.png", sep = "/"), plot_all)
# plot_stacked <- ggplot(data = data_merged, aes(x = datetime, y = temp)) + 
#   geom_line(aes(color = depth_m)) + 
#   facet_grid(depth_m ~ ., scales = "free_y") + theme(legend.position = "none")

# Export 25m Oct/Nov 2020 for Asa:
data_export <- data_merged[which(data_merged$depth_m == 25 &
                                    (month(data_merged$datetime) == 10 | month(data_merged$datetime) == 11)), ]
data_export <- subset(data_export, select = -c(id, day, hour, timenum, file))
savename <- paste(SAVE_LOCATION, "25_m_oct_nov_2020_data_for_Asa", sep = "/")
write.csv(data_export, file = savename, row.names = FALSE)
