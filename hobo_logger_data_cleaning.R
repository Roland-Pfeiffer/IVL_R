rm(list=ls())# Clear workspace (aka work environment)
cat("\014") # Clear the console

library(dplyr)
library(ggplot2)
library(glue)
library(gridExtra)
library(logging)
library(lubridate)
library(stringr)  # Check if this works on Windows. Otherwise perhaps use "stringi"
library(tcltk)
library(tidyverse)

Sys.setlocale("LC_TIME", "en_GB.UTF-8")



# -------------------------------- SCRIPT SETTINGS ------------------------------

offsets <- c()

# Adjust these according to your system and where your files are stored.

PATH_TO_HOBO_FILES <- "/media/findux/DATA/Documents/IVL/Data/hobo_data_all/"
SAVE_LOCATION <- "/media/findux/DATA/Documents/IVL/Data/hobo_out/"

DO_AGGREGATION <- TRUE
AGGREGATE_OVER_MULTIPLE_HRS <- 3

# Outlier removal settings:
COND_THRESHOLD <- 300

SD_BASED_OUTLIER_DELETION <- TRUE
SD_THRESHOLD <- 2
SD_BASIS <- "%m"

DELETE_MANUAL_OUTLIERS <- TRUE
PATH_TO_OUTLIER_FILE <- "file:///media/findux/DATA/Documents/IVL/Data/outliers.csv"


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
log_var[length(log_var) + 1] <- paste("Aggregating over hrs:", AGGREGATE_OVER_MULTIPLE_HRS)
log_var[length(log_var) + 1] <- paste("Conductivity threshold:", COND_THRESHOLD)
log_var[length(log_var) + 1] <- paste("SD-based outlier removal:", SD_BASED_OUTLIER_DELETION)
log_var[length(log_var) + 1] <- paste("SD thgreshold:", SD_THRESHOLD)
log_var[length(log_var) + 1] <- paste("Time interval SD was based on:", SD_BASIS, "\n\n")

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
  return(as.numeric(gsub("([0-9]+).*$", "\\1", depth_str))) # Copied from the internet
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
    return("3m")
  } else{
    return(re_match)
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
  return(df)
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
  out
}


get_temp_unit <- function(dframe){
  # Returns the temperature units (F or C) from the column name.
  temp_cname <- get_colname(dframe, "Temp")  # get_colname() defined above.
  temp_unit <- str_match(temp_cname, "Temp, °(F|C)")[,2]
  temp_unit
}


gmt_offset_exists <- function(df){
  gmt_os_exists <- FALSE
  if (datetime_col_exists(df)){
    gmt_offset <- extract_gmt_format(df)
    if (!is.na(gmt_offset)){
      gmt_os_exists <- TRUE
    }
  }
  gmt_os_exists
}


homogenize_colnames <- function(cnames){
  # Gives new, standardised column names to the dataframe:
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
    replace(which(cnames == cond_name), "conductivity_raw") %>% # This skips if there's no cond.
    replace(which(cnames == datetime_name), "datetime") %>%
    return()  # works w/o argument because of pipe.
}

missing_conductivity <- function(df){
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
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tNo info on depth in filename ", fname, ". File skipped.", sep = "")
    skipped_files[length(skipped_files) + 1] <- fname
  }
}

# Overwrite old fnames
n_files_with_depth <- length(fnames_with_depth)
message(paste("Skipping ", n_files_total - n_files_with_depth, " out of ", n_files_total, " files due to missing depth info.", sep = ""))
log_var[length(log_var) + 1] <- paste("[", now(), "]\tSkipping ", n_files_total - n_files_with_depth, " out of ", n_files_total, " files due to missing depth info.", sep = "")
fnames <- fnames_with_depth
log_var[length(log_var) + 1] <- "\n"

for (fname in fnames){
  depth <- extract_depth(fname)
  depth_num <- depth_as_number(depth)
  full_path_to_file <- paste(PATH_TO_HOBO_FILES, fname, sep = "/")
  
  # Try to read the file, if it fails, store the error in the variable.
  data_tmp <-  tryCatch(read.csv(full_path_to_file, skip = 1, check.names = FALSE),
                           error = function(e) e)
  
  # Check if there is an error stored in the variable
  if (inherits(data_tmp, "error")){
    error_msg <- paste
    message("Encountered error when reading ", fname, ". File skipped.")
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tError when reading file [ ", fname, "]. File skipped.", sep = "")
    skipped_files[length(skipped_files) + 1] <- fname
    # Skip the rest of this for-loop and jump to the next iteration.
    next
  }
  
  # If no error encountered, continue as planned
  log_var[length(log_var) + 1] <- paste("[", now(), "]\tReading: ", depth, ":\t", fname, sep = "")
  message('Depth: ', depth, '\tFile: ', fname)

  # Get temperature units. Needs to be done before colnames are changed.
  temperature_units <- get_temp_unit(data_tmp)

  # Add GMT offset.
   
  gmt_offset <- extract_gmt_format(data_tmp)
  offsets <- c(offsets, gmt_offset)
  if (is.na(gmt_offset)){
    message(paste("No GMT offset could be extracted for file", fname, ". File skipped.", sep = ""))
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tCould not extract GMT offset for file ", fname, ". File skipped.", sep = "")
    next
  } else {
    data_tmp <- add_gmt_offset(data_tmp)
  }
  
  
  # Adjust the column names for datetime, temp, and (if available) conductivity
  colnames(data_tmp) <- homogenize_colnames(colnames(data_tmp))
  
  # Correct Fahrenheit to Celsius if necessary.
  if (temperature_units == "F"){
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tTemperature for file ", fname, " is in Fahrenheit and will be converted to Celsius.", sep = "")
    message("(Temperature is in Fahrenheit and will be converted to Celsius.)")
    data_tmp$temp <- convert_f_to_c(data_tmp$temp)
  }
  
  if (missing_conductivity(data_tmp)){
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tMissing conductivity replaced with NA in file: ", fname, sep = "")
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
rm(data_tmp)
log_var[length(log_var) + 1] <- paste("[", now(), "]\tDone loading data.\n", sep = "")



# Show info about skipped and processed files
n_processed <- length(processed_files)
n_skipped <- length(skipped_files)
log_var[length(log_var) + 1] <- paste("[", now(), "]\tProcessed ", n_processed, " files.", sep = "")
log_var[length(log_var) + 1] <- paste("[", now(), "]\tSkipped ", n_skipped, " files.", sep = "")

message("Processed ", length(processed_files), " files.\n",
        length(fnames_with_depth), " file names contained depth info.\n",
        "Merged ",length(merged_files), " files (", n_skipped, " skipped).")
if (length(skipped_files) > 0){
  for (fname in skipped_files){
    log_var[length(log_var) + 1] <- paste("[", now(), "]\tSkipped: ", fname, sep = "")
    message("Skipped csv file: ", fname)
  }
}

# ------------------ PREPPING DATA ---------------------------------------------

log_var[length(log_var) + 1] <- paste("\n[", now(), "]\tPreparing data.", sep = "")

# Adjust datetime
data_merged <- fm_pm_to_AM_FM(data_merged) # Turn fm/em into AM/PM
data_merged$datetime <- mdy_hms(data_merged$datetime)
data_merged <- adjust_gmt_offset(data_merged)

# Treat depth as factor 
data_merged$depth_m <- as.factor(data_merged$depth_m)

# Remove duplicates
len_old <- dim(data_merged)[1]
data_merged <- distinct(data_merged, datetime, temp, conductivity_raw, depth_m,
                        .keep_all = TRUE)
len_new <- dim(data_merged)[1]
n_duplicates <- len_old - len_new
log_var[length(log_var) + 1] <- paste("[", now(), "]\tRemoved ", n_duplicates, " duplicate data points.", sep = "")

message("Removed ", n_duplicates, " duplicate rows.")

# Add unique identifier column
data_merged$id <- paste(as.character(data_merged$datetime),
                        data_merged$depth_m,
                        data_merged$temp,
                        data_merged$file, sep = "_")
# Make sure it's actually unique:
message('Identifier column is unique for every row: ',
        length(unique(data_merged$id)) == dim(data_merged)[1])


# Sort the data by depth and datetime
data_merged <- data_merged[with(data_merged, order(depth_m, datetime)), ]

data_raw <- data_merged

# ------------------------- SAVE RAW -------------------------------------------
data_raw_out <- data_raw
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
data_raw_out$datetime <- as.character(data_raw_out$datetime)
write_csv(data_raw, paste(SAVE_LOCATION, "hobo_data_raw_", now_clean, ".csv", sep = ""))
rm(data_raw_out)



# -------------------- DATA CLEANING -------------------------------------------


# Remove rows with conductivity below threshold
c0 <- dim(data_merged)[1]
data_merged <- data_merged[is.na(data_merged$conductivity_raw) | 
                             data_merged$conductivity_raw > COND_THRESHOLD,]
c1 <- dim(data_merged)[1]
message("Removed ", c0 - c1, " rows where conductivity <= ", COND_THRESHOLD)


# Remove rows with NA in temperature:
temp_na_0 <- dim(data_merged)[1]
data_merged <- data_merged[!is.na(data_merged$temp), ]
temp_na_1 <- dim(data_merged)[1]
message("Removed ", temp_na_0 - temp_na_1, " rows where temp = NA.")


# ------------------------------- DELETE OUTLIERS ------------------------------
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
}


if (SD_BASED_OUTLIER_DELETION){
  # data_merged$ym <- paste(year(data_merged$datetime),
  #                         str_pad(as.character(month(data_merged$datetime)), 2, "left", "0"),
  #                         sep = "")
  data_merged$ym <- paste(year(data_merged$datetime),
                          format(data_merged$datetime, format = SD_BASIS),
                          sep = "")
  for (yearmon in unique(data_merged$ym)){
    data_merged$temp_sd[data_merged$ym == yearmon] <- sd(data_merged$temp[data_merged$ym == yearmon])
    data_merged$temp_mean[data_merged$ym == yearmon] <- mean(data_merged$temp[data_merged$ym == yearmon])
    data_merged$tdiff_from_sd <- sqrt((data_merged$temp_mean - data_merged$temp)^2)
    data_merged$tdiff_above_th <- data_merged$tdiff_from_sd > (data_merged$temp_sd * SD_THRESHOLD)
  }
  
  t0 <- length(data_merged[,1])
  data_merged <- data_merged[!data_merged$tdiff_above_th, ]
  t1 <- length(data_merged[,1])
  message("Removed ", t0 - t1, " rows where temperature was above ",
          SD_THRESHOLD, " standard deviations above time unit (",
          SD_BASIS, ").")
}



# -------------------------- AGGREGATE -----------------------------------------

if (DO_AGGREGATION){  # Add an aggregation column
  data_merged$year <- year(data_merged$datetime)
  data_merged$month <- month(data_merged$datetime)
  data_merged$day <- day(data_merged$datetime)
  data_merged$hour <- hour(data_merged$datetime)
  data_merged$aggregate_category <- data_merged$year * 1000000 + 
    data_merged$month * 10000 + 
    data_merged$day * 100 + 
    (data_merged$hour %/% AGGREGATE_OVER_MULTIPLE_HRS)
  
  # Aggregate data NOTE: Be patient, this might take a while.
  message("Aggregating data. This might take a while...")
  data_merged <- aggregate(data_merged, by = list(data_merged$aggregate_category,
                                                  data_merged$depth_m), FUN = "mean", na.rm = TRUE)
  # Drop columns with NAs that were created during aggregate()
  data_merged <- subset(data_merged, select = -c(depth_m, file))
  # Update the column names
  names(data_merged)[names(data_merged) == 'Group.2'] <- "depth_m"
}

# --------------------------- SAVE LOG -----------------------------------------

fconn <- file(paste(SAVE_LOCATION, "LOG_", now_clean, ".txt", sep = ""))
writeLines(log_var, fconn)
close(fconn)

# ----------------------------- CREATE WIDE FORMAT -----------------------------
# Create a data frame in wide format
data_wide <- reshape(data_merged, idvar = "datetime", timevar = "depth_m",
                     direction = "wide")

# Adjust column names
colnames(data_wide) <- str_replace(colnames(data_wide), "\\.", "_")
for (i  in 2:length(colnames(data_wide))){
  colnames(data_wide)[i] <- paste(colnames(data_wide)[i], "m", sep = "")
}


# ------------------- CREATE PLOTS  --------------------------------------------


# Define axis limits for the plots
xlims <- c(min(data_merged$datetime), max(data_merged$datetime))
ylims <- c(min(data_merged$temp), max(data_merged$temp))

# Plotting cleaned temperature data
plot_temp_all <- ggplot(data = data_merged,
                        mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line() +
  ggtitle("All depths")

plot_temp_03m <- ggplot(data = data_merged[data_merged$depth_m == 3, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#33CCCC" ) +
  ggtitle(label = "03 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_07m <- ggplot(data = data_merged[data_merged$depth_m == 7, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#009999" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#006666" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_25m <- ggplot(data = data_merged[data_merged$depth_m == 25, ],
                        mapping = aes(x = datetime, y = temp)) +
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# Plot conductivity (raw)
cond_data <- subset(data_raw, select = c(datetime, conductivity_raw, depth_m))
cond_data <- cond_data[!is.na(cond_data$conductivity_raw), ]

plot_cond_03m <- ggplot(data = cond_data[cond_data$depth_m == 3, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("03 m") +
  xlab("Time") + ylab("Raw conductivity (µS cm⁻¹)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_07m <- ggplot(data = cond_data[cond_data$depth_m == 7, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Raw conductivity (µS cm⁻¹)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_15m <- ggplot(data = cond_data[cond_data$depth_m == 15, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Raw conductivity (µS cm⁻¹)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_cond_25m <- ggplot(data = cond_data[cond_data$depth_m == 25, ],
                        mapping = aes(x = datetime, y = conductivity_raw)) + 
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Raw conductivity (µS cm⁻¹)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

# Not working: 
# faceted_plot <- ggplot() +
#   geom_line(data = data_raw, mapping = aes(x=datetime, y=temp), color="red") +
#   geom_line(data = data_merged, mapping = aes(x=datetime, y=temp), color=C07) +
#   facet_grid(~ data_merged$depth_m) +
#   ggtitle(label = "03 m") + 
#   xlab("Time") + ylab("Temp. (°C)") +
#   xlim(xlims) + ylim(ylims)
# faceted_plot


plot_temp_comparison_03m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 3, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 3, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "03 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_comparison_07m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 7, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 7, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "07 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_comparison_15m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 15, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 15, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "15 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_temp_comparison_25m <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 25, ],
            mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 25, ],
            mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "25 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ---------------------- SAVE FILES AND SHOW PLOTS -----------------------------
data_merged_out <- data_merged
data_merged_out$datetime <- as.character(data_merged_out$datetime)
write_csv(data_merged_out, paste(SAVE_LOCATION, "hobo_data_merged_", now_clean, ".csv", sep = ""))
rm(data_merged_out)

data_wide$datetime <- as.character(data_wide$datetime)
write_csv(data_wide, paste(SAVE_LOCATION, "hobo_data_wide_", now_clean, ".csv", sep = ""))

# grid.arrange(plot_03m_raw, plot_07m_raw, plot_15m_raw, plot_25m_raw, ncol=1)
grid.arrange(plot_temp_comparison_03m, plot_temp_comparison_07m,
             plot_temp_comparison_15m, plot_temp_comparison_25m,
             ncol=1)
grid.arrange(plot_temp_03m, plot_temp_07m, plot_temp_15m, plot_temp_25m, ncol=1)
plot_temp_all
# a <- grid.arrange(plot_temp_comparison_03m, plot_temp_comparison_15m,
#              plot_cond_03m, plot_cond_15m,
#              plot_temp_comparison_07m, plot_temp_comparison_25m,
#              plot_cond_07m, plot_cond_25m,
#              ncol=2)
# plot_15m
# plot_raw
# plot_all
# ggsave(filename = paste(SAVE_LOCATION, "plot_comparison.png", sep = "_"),
#        plot = a,
#        width = 420, height = 297, units = "mm")
# plot_stacked <- ggplot(data = data_merged, aes(x = datetime, y = temp)) + 
#   geom_line(aes(color = depth_m)) + 
#   facet_grid(depth_m ~ ., scales = "free_y") + theme(legend.position = "none")



# rm(list = c("c0", "C03", "C07", "c1", "C15", "C25", "d1", "d2", "date_now", "depth", "depth_num",
#             "fname", "fnames", "full_path_to_file", "i", "id_o", "len_new", "len_old",
#             "n_processed", "n_skipped", "outlier_i", "outlier_ids", "outlier_indices",
#             "temp_na_0", "temp_na_1", "temperature_units", "xlims", "ylims"))
