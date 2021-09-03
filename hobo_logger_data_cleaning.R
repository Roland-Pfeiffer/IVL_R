rm(list=ls())
cat("\014") 

# install.packages("tidyverse")
# install.packages("stringi")#
library(tidyverse)
library(stringr)  # Check if this also works on WIN. Otherwise use "stringi"
library(tcltk)
library(dplyr)
library(ggplot2)

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
  # indexTZ(dframe$datetime) <- "Europe/Paris"
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


dtime_to_string <- function(dframe){
  dframe$datetime <- as.character(dframe$datetime)
  dframe
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
    if (startsWith(cname, "High Range") || startsWith(cname, "conductivity_raw")){
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
path_to_files <- "/media/findux/DATA/Documents/IVL/Data/HOBO logger data 2020"
save_location <- "/media/findux/DATA/Documents/IVL/Data"


# Initiate data frames
# data_03_m <- data.frame()
# data_07_m <- data.frame()
# data_15_m <- data.frame()
# data_25_m <- data.frame()
data_merged <- data.frame()
skipped_files <- list()
processed_files <- list()

# Read filenames
fnames <- list.files(path_to_files)

# Gobble up chaotically named data:
for (fname in fnames){
  if (endsWith(fname, ".csv")){
    processed_files <- c(processed_files, fname)
    if (fname_contains_depth(fname)){
      depth <- extract_depth(fname)
      depth_num <- depth_as_number(depth)
      fpath <- paste(path_to_files, fname, sep = "/")
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

# Only keep relevant columns
data_merged <- select(data_merged, datetime, temp, conductivity_raw, depth_m, file)

# Treat depth as factor 
data_merged$depth_m <- as.factor(data_merged$depth_m)

# Remove duplicates
len_old <- dim(data_merged)[1]
data_merged <- distinct(data_merged, datetime, temp, conductivity_raw, depth_m,
                        .keep_all = TRUE)
len_new <- dim(data_merged)[1]
message("Removed ", len_old - len_new, " duplicate rows.")

data_wide <- reshape(data_merged, idvar = "datetime", timevar = "depth_m", direction = "wide")
colnames(data_wide) <- str_replace(colnames(data_wide), "\\.", "_")

# Writing output
# data_merged <- dtime_to_string(data_merged)
# dtime <- as.character(strptime(now(), format = "%Y-%m-%d"))
# write_csv(data_merged, paste(save_location, "/hobo_data_merged_", dtime, ".csv", sep = ""))

plot_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line()
plot_15m

plot_all <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line()
plot_all


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

