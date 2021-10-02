rm(list=ls())# Clear workspace (aka work environment)
cat("\014") # Clear the console

library(dplyr)
library(ggplot2)
library(ggthemes)
library(glue)
library(gridExtra)
library(logger)
library(lubridate)
library(reshape2)
library(stringr)  # Check if this works on Windows. Otherwise perhaps use "stringi"
library(tcltk)
library(tidyverse)
library(zoo)

log_threshold()  # set to log_threshold(DEBUG) to see debugging output.

Sys.setlocale("LC_TIME", "en_GB.UTF-8")

# -------------------------------- SCRIPT SETTINGS ------------------------------

# Adjust these according to your system and where your files are stored.

PATH_TO_HOBO_FILES <- "/media/findux/DATA/Documents/IVL/Data/hobo_data_all/"
SAVE_LOCATION <- "/media/findux/DATA/Documents/IVL/Data/hobo_out/"

DO_AGGREGATION <- TRUE
AGGREGATE_OVER_MULTIPLE_HRS <- 3

USE_ROLLING_MEAN <- FALSE
WINDOW_SIZE <- 2000

# Outlier removal settings:
COND_THRESHOLD <- 400

SD_BASED_OUTLIER_DELETION <- TRUE
SD_THRESHOLD <- 2

DELETE_MANUAL_OUTLIERS <- TRUE
PATH_TO_OUTLIER_FILE <- "/media/findux/DATA/Documents/IVL/Data/outliers_latest.csv"

SAVE_FILES <- TRUE


# ----------------------- INTERNAL SETUPS --------------------------------------
# Colors:
C03 <- "#33CCCC"
C07 <- "#009999"
C15 <- "#006666"
C25 <- "#336666"
# Initiate logging
now_clean <- format(now(), format = "%Y-%m-%d_%H-%M")
log_var <- c(paste("Datetime:", now()))
log_var[length(log_var) + 1] <- paste("Hobo files:", PATH_TO_HOBO_FILES)
log_var[length(log_var) + 1] <- paste("Save directory: ", SAVE_LOCATION)
log_var[length(log_var) + 1] <- paste("Outlier file:", PATH_TO_OUTLIER_FILE)
log_var[length(log_var) + 1] <- paste("Aggregating:", DO_AGGREGATION)
log_var[length(log_var) + 1] <- paste("Aggregating over hrs:",
                                      AGGREGATE_OVER_MULTIPLE_HRS)
log_var[length(log_var) + 1] <- paste("Using rolling mean:", USE_ROLLING_MEAN)
log_var[length(log_var) + 1] <- paste("Window size:", WINDOW_SIZE)
log_var[length(log_var) + 1] <- paste("Conductivity threshold:", COND_THRESHOLD)
log_var[length(log_var) + 1] <- paste("Remove manually marked outliers:",
                                      DELETE_MANUAL_OUTLIERS)
log_var[length(log_var) + 1] <- paste("SD-based outlier removal:",
                                      SD_BASED_OUTLIER_DELETION)
log_var[length(log_var) + 1] <- paste("SD thgreshold:", SD_THRESHOLD)

# -------------------------- FUNCTIONS -----------------------------------------

add_gmt_offset <- function(data_df){
  # Adds a column to the dataset that contains the gmt offset information 
  # Contained in the filenames.
  # Note that this needs to be run before column names are homogenised and lose
  # this info.
  gmt_offset <- extract_gmt_format(data_df)
  offset_numstr <-substr(gmt_offset, 4,6)
  gmt_offset_num <- as.numeric(offset_numstr)
  data_df$gmt_offset <- gmt_offset_num
  data_df
}


adjust_gmt_offset <- function(dframe){
  # Adjusts the datetime based on the GMT offset column.
  dframe$gmt_correction_sec <- (2 - dframe$gmt_offset) * 60 * 60
  dframe$datetime <- dframe$datetime + dframe$gmt_correction_sec
  dframe <- subset(dframe, select = -c(gmt_correction_sec))
  dframe
}


convert_f_to_c <- function(deg_F){
  # Converts degrees Fahrenheit to degrees Celsius.
  # Formula from:
  # https://www.metric-conversions.org/sv/temperatur/fahrenheit-till-celsius.htm
  deg_C <- (deg_F - 32) / 1.8
  deg_C
}


datetime_col_exists <- function(df){
  # Checks if there is a column starting with "Date Time".
  dtc_exists <- FALSE
  for (fname in colnames(df)){
    if (startsWith(fname, "Date Time")){
      dtc_exists <- TRUE
    }
  }
  dtc_exists
}


depth_as_number <- function(depth_str){
  # Takes a character string, e.g. "5m" and returns it as a number: 5.
  return(as.numeric(gsub("([0-9]+).*$", "\\1", depth_str))) # Googled
}


extract_depth <- function(fname) {
  # Extracts depth information from the filename.
  # This includes the following patterns:
  #   0m - 99m
  #   surface
  fname_lower <- str_to_lower(fname)
  re_match <- str_extract(fname_lower, "\\d{1,2}m")
  re_match_surface <- str_extract(fname_lower, "surface")
  if (!is.na(re_match_surface)){
    "3m"
  } else{
    re_match
  }}


extract_gmt_format <- function(data_df){
  # Returns the GMT offset string, e.g. "GMT+02:00"
  dt_match_str <- NA
  for (cname in colnames(data_df)){
    if (startsWith(cname, "Date Time") || cname == "datetime"){
      dt_match_str <- str_extract(cname, "GMT[+|-]\\d\\d:\\d\\d")
    }
  }
  dt_match_str
}


fill_cond_if_missing <- function(dframe){
  # Creates a conductivity column filled with NA if there is no conductivity data.
  # NOTE: This function does not seem to work in pipes
  if ("conductivity_raw" %in% colnames(dframe)){
    # (Do nothing, conductivity column already exists)
  } else {
    # Create a conductivity column filled with NA.
    dframe$conductivity_raw <- as.numeric(NA)
  }
  dframe
}


fm_pm_to_AM_FM <- function(df){
  # Changes fm/em to AM/EM in df$datetime.
  # Requires a character variable named "datetime" to be in the dataframe.
  df$datetime <- str_replace(df$datetime, "em", "PM")
  df$datetime <- str_replace(df$datetime, "fm", "AM")
  df
}


fname_contains_depth <- function(fname_str) {
  # Returns TRUE if the filename contains depth info. Uses regexes to look for
  # 2 decimals followed by "m", or the string "surface".
  # Returns FALSE otherwise.
  fname_lower <- str_to_lower(fname_str)
  re_match <- str_extract(fname_lower, "\\d{1,2}m")
  re_match_surface <- str_extract(fname_lower, "surface")
  if (is.na(re_match) && is.na(re_match_surface)){
    FALSE
  } else{
    TRUE
  }}


get_colname <- function(dframe, name_start_str){
  # Returns the full column name based on a string fragment at the beginning.
  out <- NA
  for (cname in colnames(dframe)){
    if (startsWith(cname, name_start_str)){
      out <- cname
    }
  }
  if (is.na(out)){
    message("[WARNING]\tNo corresponding colname found.")
  }
  out
}


get_temp_unit <- function(dframe){
  # Returns the temperature units (F or C) from the column name.
  temp_cname <- get_colname(dframe, "Temp")  # get_colname() function defined above.
  log_debug(temp_cname)
  temp_unit <- str_match(temp_cname, "Temp, °(F|C)")[,2]
  temp_unit
}


gmt_offset_exists <- function(df){
  # Checks if there is a datetime column containing GMT info with the pattern:
  # GMT+HH:MM
  gmt_os_exists <- FALSE
  if (datetime_col_exists(df)){
    gmt_offset <- extract_gmt_format(df)
    if (!is.na(gmt_offset)){
      gmt_os_exists <- TRUE
    }
  }
  gmt_os_exists
}


homogenize_colnames <- function(cnames){ # ToDo: use df instad of cnames
  #  Takes a list of column names, and returns them in homogenized form:
  #   datetime, temp, conductivity_raw
  # conductivity_raw will be skipped if there is not conductivity data.
  
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
    # This skips if there's no cond:
    replace(which(cnames == cond_name), "conductivity_raw") %>% 
    replace(which(cnames == datetime_name), "datetime") %>%
    return()  # works w/o argument because of pipe.
}

missing_conductivity <- function(df){
  # Checks if a data frame has a conductivity column starting with "High Range"
  # or named "conductivity_raw".
  # Returns TRUE if one of the two was found, else FALSE.
  conductivity_missing <- TRUE
  for (cname in colnames(df)){
    if (startsWith(cname, "High Range") || cname == "conductivity_raw"){
      conductivity_missing <- FALSE
    }
  }
  conductivity_missing
}


# --------------------- LOADING LOOP -------------------------------------------

log_var[length(log_var) + 1] <- paste("[", now(), "]\tLoading files.", sep = "")


# Initiate empty data frames and lists for later use.
data_merged <- data.frame()
skipped_files <- c()
processed_files <- c()
merged_files <- c()

# Read filenames into a list called fnames.
# Only use the ones that have the pattern .csv at the end ($)
fnames <- list.files(PATH_TO_HOBO_FILES, pattern = ".csv$")
n_files_total <- length(fnames)

# Extract fnames w/ depth info:
fnames_with_depth <- c()
for (fname in fnames){
  processed_files[length(processed_files) + 1] <- fname
  if (fname_contains_depth(fname)){
    fnames_with_depth[length(fnames_with_depth) + 1] <- fname
  } else {
    log_var[length(log_var) + 1] <- paste("[", now(),
                                          "]\tNo info on depth in filename ",
                                          fname, ". File skipped.", sep = "")
    skipped_files[length(skipped_files) + 1] <- fname
  }
}

# Overwrite old fnames
n_files_with_depth <- length(fnames_with_depth)
message(paste("Skipping ", n_files_total - n_files_with_depth, " out of ",
              n_files_total, " files due to missing depth info.", sep = ""))
log_var[length(log_var) + 1] <- paste("[", now(), "]\tSkipping ",
                                      n_files_total - n_files_with_depth, 
                                      " out of ", n_files_total, 
                                      " files due to missing depth info.", sep = "")
fnames <- fnames_with_depth
log_var[length(log_var) + 1] <- "\n"

# Data extraction loop
for (fname in fnames){
  depth <- extract_depth(fname)
  depth_num <- depth_as_number(depth)
  full_path_to_file <- paste(PATH_TO_HOBO_FILES, fname, sep = "/")
  
  # Try to read the file, if it fails, store the error in the variable.
  data_tmp <-  tryCatch(read.csv(full_path_to_file, skip = 1, check.names = FALSE),
                        error = function(e) e)
  
  # Check if there is an error stored in the variable
  if (inherits(data_tmp, "error")){
    message("Encountered error when reading ", fname, ". Error: ", data_tmp, 
            ". File skipped.")
    log_var[length(log_var) + 1] <- paste("[", now(), 
                                          "]\tError when reading file [ ", fname,
                                          "]. File skipped.", sep = "")
    skipped_files[length(skipped_files) + 1] <- fname
    # Skip the rest of this for-loop and jump to the next iteration 
    # (if there was an error):
    next
  }
  
  # If no error encountered, continue as planned
  log_var[length(log_var) + 1] <- paste("[", now(), "]\tReading: ", depth,
                                        ":\t", fname, sep = "")
  message('Depth: ', depth, '\tFile: ', fname)
  
  # Get temperature units. Needs to be done before colnames are changed.
  # log_debug(names(data_tmp))
  temperature_units <- get_temp_unit(data_tmp)
  log_debug(temperature_units)
  
  # Add GMT offset.
  gmt_offset <- extract_gmt_format(data_tmp)
  if (is.na(gmt_offset)){
    message(paste("No GMT offset could be extracted for file", fname,
                  ". File skipped.", sep = ""))
    log_var[length(log_var) + 1] <- paste("[", now(),
                                          "]\tCould not extract GMT offset for file ",
                                          fname, ". File skipped.", sep = "")
    # Jump to the next iteration of there was no GMT offset stored in the column names:
    next  
  } else {
    data_tmp <- add_gmt_offset(data_tmp)
  }
  
  
  # Adjust the column names for datetime, temp, and (if available) conductivity
  colnames(data_tmp) <- homogenize_colnames(colnames(data_tmp))
  
  # Correct Fahrenheit to Celsius if necessary.
  if (temperature_units == "F"){
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tTemperature for file ",
                                          fname, " is in Fahrenheit and will be",
                                          " converted to Celsius.", sep = "")
    message("(Temperature is in Fahrenheit and will be converted to Celsius.)")
    data_tmp$temp <- convert_f_to_c(data_tmp$temp)
  }
  
  if (missing_conductivity(data_tmp)){
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tMissing conductivity",
                                          " replaced with NA in file: ",
                                          fname, sep = "")
  }
  data_tmp <- fill_cond_if_missing(data_tmp)
  data_tmp$conductivity_raw <- as.numeric(data_tmp$conductivity_raw)
  data_tmp <- select(data_tmp, datetime, temp, conductivity_raw, gmt_offset)
  data_tmp$depth_m <- depth_num
  
  # Add a column w/ filename to see where the data came from.
  data_tmp$file <- fname
  
  # Add currently processed data to the data_merged data frame
  data_merged <- rbind(data_merged, data_tmp)
  merged_files[length(merged_files) + 1] <- fname
}
log_var[length(log_var) + 1] <- paste("[", now(), "]\tDone loading data.\n",
                                      sep = "")



# Show info about skipped and processed files
n_processed <- length(processed_files)
n_skipped <- length(skipped_files)
log_var[length(log_var) + 1] <- paste("[", now(), "]\tProcessed ", n_processed,
                                      " files.", sep = "")
log_var[length(log_var) + 1] <- paste("[", now(), "]\tSkipped ", n_skipped,
                                      " files.", sep = "")

message("Processed ", length(processed_files), " files.\n",
        length(fnames_with_depth), " file names contained depth info.\n",
        "Merged ",length(merged_files), " files (", n_skipped, " skipped).")
if (length(skipped_files) > 0){
  for (fname in skipped_files){
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tSkipped: ",
                                          fname, sep = "")
    message("Skipped csv file: ", fname)
  }
}



# ------------------ PREPPING DATA ---------------------------------------------

log_var[length(log_var) + 1] <- paste("\n[", now(),
                                      "]\tPreparing data.", sep = "")

# Adjust datetime
data_merged <- fm_pm_to_AM_FM(data_merged) # Turn fm/em into AM/PM
data_merged$datetime <- mdy_hms(data_merged$datetime) 
data_merged <- adjust_gmt_offset(data_merged)

# Treat depth as factor 
data_merged$depth_m <- as.factor(data_merged$depth_m)

# Add unique identifier column
data_merged$id <- paste(as.character(data_merged$datetime),
                        data_merged$depth_m,
                        data_merged$temp,
                        data_merged$file, sep = "_")
# Make sure it's actually unique:
ids_are_unique <- length(unique(data_merged$id)) == dim(data_merged)[1]
message('Identifier column is unique for every row: ', ids_are_unique)
if (!ids_are_unique) message("[ERROR]\tIDs not unique!")
log_var[length(log_var) + 1] <- paste("[", now(), "]\tIdentifiers are unique: ",
                                      ids_are_unique, sep = "")


# ---------------------- REMOVE DUPLICATES -------------------------------------
len_old <- dim(data_merged)[1]
data_merged <- distinct(data_merged, datetime, temp, conductivity_raw, depth_m,
                        # If TRUE, keep all variables in .data. If a combination of
                        # ... is not distinct, this keeps the first row of values:
                        .keep_all = TRUE)  
len_new <- dim(data_merged)[1]
n_duplicates <- len_old - len_new
log_var[length(log_var) + 1] <- paste("[", now(), "]\tRemoved ", n_duplicates,
                                      " duplicate data points.", sep = "")

message("Removed ", n_duplicates, " duplicate rows.")



# Sort the data by depth and datetime
data_merged <- data_merged[with(data_merged, order(depth_m, datetime)), ]

data_raw <- data_merged



# ------------------------- SAVE RAW -------------------------------------------

data_raw_out <- data_raw
data_raw_out$datetime <- as.character(data_raw_out$datetime)
if (SAVE_FILES) write_csv(data_raw_out, paste(SAVE_LOCATION, "hobo_data_raw_",
                                              now_clean, ".csv", sep = ""))
rm(data_raw_out)


# -------------------- REMOVE COND BELOW THRESHOLD -----------------------------

# Remove rows with conductivity below threshold
c0 <- dim(data_merged)[1]
data_merged$low_cond <- data_merged$conductivity_raw <= COND_THRESHOLD 
data_merged$low_cond[is.na(data_merged$low_cond)] <- FALSE
data_merged <- data_merged[data_merged$low_cond == FALSE, ]
# data_merged <- data_merged[is.na(data_merged$conductivity_raw) | 
#                              data_merged$conductivity_raw > COND_THRESHOLD,]
c1 <- dim(data_merged)[1]
message("Removed ", c0 - c1, " rows where conductivity <= ", COND_THRESHOLD)
log_var[length(log_var) + 1] <- paste("[", now(), "]\tRemoved ", c0 - c1,
                                      " rows where conductivity <= ",
                                      COND_THRESHOLD, sep = "")



# -------------------- REMOVE TEMP NAs -----------------------------------------

# Remove rows with NA in temperature:
temp_na_0 <- dim(data_merged)[1]
data_merged <- data_merged[!is.na(data_merged$temp), ]
temp_na_1 <- dim(data_merged)[1]
message("Removed ", temp_na_0 - temp_na_1, " rows where temp = NA.")
log_var[length(log_var) + 1] <- paste("[", now(), "]\tRemoved ",
                                      temp_na_0 - temp_na_1,
                                      " rows where temp == NA.", sep = "")


# ---------------- DELETE OUTLIERS BASED ON SD THRESHOLD -----------------------

if (SD_BASED_OUTLIER_DELETION){
  data_merged$sd_category <- paste(year(data_merged$datetime),
                                   format(data_merged$datetime, format = "%m"),
                                   data_merged$depth_m,
                                   sep = "")
  for (sd_cat in unique(data_merged$sd_category)){
    
    data_merged$temp_sd[data_merged$sd_category == sd_cat] <- 
      sd(data_merged$temp[data_merged$sd_category == sd_cat])
    
    data_merged$temp_mean[data_merged$sd_category == sd_cat] <- 
      mean(data_merged$temp[data_merged$sd_category == sd_cat])
    
    data_merged$tdiff_from_sd <- 
      abs(data_merged$temp_mean - data_merged$temp)
    
    data_merged$tdiff_above_th <- 
      data_merged$tdiff_from_sd > (data_merged$temp_sd * SD_THRESHOLD)
  }
  
  t0 <- length(data_merged[,1])
  data_merged <- data_merged[!data_merged$tdiff_above_th, ]
  t1 <- length(data_merged[,1])
  message("Removed ", t0 - t1, " rows where temperature was above ",
          SD_THRESHOLD, " standard deviations above SD per", SD_THRESHOLD,
          " month(s).")
  log_var[length(log_var) + 1] <- paste("[", now(), "]\tRemoved ", t0 - t1,
                                        " outliers based on SD.",
                                        SD_THRESHOLD, " month(s).", sep = "")
  
}


# ---------------- DELETE MANUALLY SELECTED OUTLIERS ---------------------------


# Delete manually marked outliers
if (DELETE_MANUAL_OUTLIERS){
  d1 <- dim(data_merged)[1]
  outliers <- read.csv(PATH_TO_OUTLIER_FILE)
  outlier_ids <- outliers$id
  outlier_indices <- c()
  for (id_o in outlier_ids){
    if (id_o %in% data_merged$id){
      outlier_i <- which(data_merged$id == id_o)
      outlier_indices[length(outlier_indices) + 1] <- outlier_i
    }
  }
  data_merged <- data_merged[-c(outlier_indices),]
  d2 <- dim(data_merged)[1]
  message("Dropped ", d1 - d2, " rows of manually marked outliers.")
  log_var[length(log_var) + 1] <- paste("[", now(), "]\tRemoved ", d1 - d2,
                                        " manually selected outliers.", sep = "")
}


# -------------------------- AGGREGATE -----------------------------------------

data_merged$year <- year(data_merged$datetime)
data_merged$month <- month(data_merged$datetime)
data_merged$day <- day(data_merged$datetime)
data_merged$hour <- hour(data_merged$datetime)

if (DO_AGGREGATION){  # Add an aggregation column
  data_merged$aggregate_category <- data_merged$year * 1000000 + 
    data_merged$month * 10000 + 
    data_merged$day * 100 + 
    (data_merged$hour %/% AGGREGATE_OVER_MULTIPLE_HRS)
  
  # Aggregate data per aggregate category, but also per depth (to not lump all the
  # depths together).
  # NOTE: Be patient, this might take a while, depending on your system.
  message("Aggregating data. This might take a while...")
  data_merged <- aggregate(data_merged, by = list(data_merged$aggregate_category,
                                                  data_merged$depth_m),
                           FUN = "mean", na.rm = TRUE)
  # Drop columns with NAs that were created during aggregate()
  data_merged <- subset(data_merged, select = -c(depth_m, file))
  # Update the column names
  names(data_merged)[names(data_merged) == 'Group.2'] <- "depth_m"
}


# ---------------------- ROLLING MEAN ------------------------------------------

if (USE_ROLLING_MEAN){
  message("Using rolling mean!")
  data_merged <- group_by(data_merged, depth_m) 
  data_merged$rolling_avg_temp <- rollmean(data_merged$temp, k=WINDOW_SIZE,
                                           fill = NA)
  data_merged <- ungroup(data_merged)
  log_var[length(log_var) + 1] <- paste("[", now(), "]\tUsed rolling mean with window size", WINDOW_SIZE, sep = "")
}


# ------------------------- PLOT COMPARISON ------------------------------------

data_raw$facet_groups <- factor(paste(data_raw$depth_m, "m"),
                                levels = c("3 m", "7 m", "15 m", "25 m"))
data_merged$facet_groups <- factor(paste(data_merged$depth_m, "m"),
                                   levels = c("3 m", "7 m", "15 m", "25 m"))
COLOR_PALETTE <- c("#33CCCC", "#009999", "#006666", "#336666")

# Create a dataset that does not have the facet categories so it will show up in
# all plots:
# Create a plot that will be faceted by depth category
comp_plot <- ggplot(mapping = aes(x = datetime, y = temp)) +
  theme_bw() +
  # Map the background data in grey (actually transparent black).
  facet_grid(facet_groups ~ .) +
  geom_line(data = data_raw, mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged, color = "seagreen") +
  ggtitle(label = "Temperature data per depth over time",
          subtitle = "Source: Hobo loggers.") +
  xlab("Date") + ylab("Temperature (\u00b0C)") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")
comp_plot


# --------------------------- SAVE LOG -----------------------------------------

if (SAVE_FILES) {
  fconn <- file(paste(SAVE_LOCATION, "LOG_", now_clean, ".txt", sep = ""))
  writeLines(log_var, fconn)
  close(fconn)
}


# ---------------------------- SAVE FILES --------------------------------------

data_merged <- subset(data_merged, select = c(datetime,
                                              depth_m,
                                              temp,
                                              conductivity_raw))

# Save long format
data_merged_out <- data_merged
data_merged_out$datetime <- as.character(data_merged_out$datetime)
if (SAVE_FILES){
  write_csv(data_merged_out, paste(SAVE_LOCATION, "hobo_data_merged_",
                                   now_clean, ".csv", sep = ""))
} 
rm(data_merged_out)

# Save wide format
data_wide <- reshape(data_merged, idvar = "datetime", timevar = "depth_m",
                     direction = "wide")
colnames(data_wide) <- str_replace(colnames(data_wide), "\\.", "_")
for (i  in 2:length(colnames(data_wide))){
  colnames(data_wide)[i] <- paste(colnames(data_wide)[i], "m", sep = "")
}
data_wide <- data_wide[order(data_wide$datetime), ]
data_wide$datetime <- as.character(data_wide$datetime)
data_wide <- subset(data_wide, select = c(datetime,
                                          temp_3m, temp_7m,
                                          temp_15m, temp_25m,
                                          conductivity_raw_3m,
                                          conductivity_raw_7m,
                                          conductivity_raw_15m,
                                          conductivity_raw_25m))
if (SAVE_FILES){
  write_csv(data_wide, paste(SAVE_LOCATION, "hobo_data_wide_",
                             now_clean, ".csv", sep = ""))
}
