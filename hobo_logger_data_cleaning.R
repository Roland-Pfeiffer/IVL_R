# Clear workspace (aka work environment)
rm(list=ls())

# Close any open plots
dev.off()

# Clear the console
cat("\014") 

# Load packages. If you don't have them, install them using:
# install.packages("package name") # include the quotation marks!
library(tidyverse)
library(stringr)  # Check if this works on Windows. Otherwise perhaps use "stringi"
library(tcltk)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

# -------------------------- FUNCTIONS -----------------------------------------
# Function defititions who will be used in the script section below.

add_gmt_offset <- function(data_df){
  gmt_offset <- extract_gmt_format(data_df)
  offset_numstr <-substr(gmt_offset, 4,6)
  gmt_offset_num <- as.numeric(offset_numstr)
  data_df$gmt_offset <- gmt_offset_num
  data_df
}


adjust_gmt_offset <- function(dframe){
  dframe$gmt_correction_sec <- (2 - dframe$gmt_offset) * 60 * 60
  dframe$datetime <- dframe$datetime + dframe$gmt_correction_sec
  dframe <- subset(dframe, select = -c(gmt_correction_sec))
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
    if (startsWith(cname, "Date Time")){
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
PATH_TO_FILES <- "/media/findux/DATA/Documents/IVL/Data/hobo_data_all/"
SAVE_LOCATION <- "/media/findux/DATA/Documents/IVL/Data/hobo_out/hobo_data"
COND_THRESHOLD <- 800


DELETE_OUTLIERS <- TRUE
fname_outliers <- "file:///media/findux/DATA/Documents/IVL/Data/outliers.csv"

# Colors:
C03 <- "#33CCCC"
C07 <- "#009999"
C15 <- "#006666"
C25 <- "#336666"

# Initiate empty data frames and lists for later use.
data_merged <- data.frame()
skipped_files <- c()
processed_files <- c()

# Read filenames into a list called fnames:
fnames <- list.files(PATH_TO_FILES)

# Go over every single filename in fnames
for (fname in fnames){
  # Check if it ends with ".csv" and then add it to the list of processed files
  if (endsWith(fname, ".csv")){
    processed_files[length(processed_files) + 1] <- fname
    # If filename contains depth information, continue processing it.
    if (fname_contains_depth(fname)){
      depth <- extract_depth(fname)
      depth_num <- depth_as_number(depth)
      full_path_to_file <- paste(PATH_TO_FILES, fname, sep = "/")
      data_tmp <- read.csv(full_path_to_file, skip = 1, check.names = FALSE)
      # TODO: add an error catch
      
      message('Depth: ', depth, '\tFile: ', fname)
      
      # Get temperature units. Needs to be done before colnames are changed.
      temperature_units <- get_temp_unit(data_tmp)
      # Get GMT offset. Needs to be done before colnames are changed.
      data_tmp <- add_gmt_offset(data_tmp)
      
      # Adjust the column names for datetime, temp, and (if available) conductivity
      colnames(data_tmp) <- homogenize_colnames(colnames(data_tmp))
      
      # Correct Fahrenheit to Celsius if necessary.
      if (temperature_units == "F"){
        message("(Temperature is in Fahrenheit and will be converted to Celsius.)")
        data_tmp$temp <- convert_f_to_c(data_tmp$temp)
      }
  
      data_tmp <- fill_cond_if_missing(data_tmp)
      data_tmp$conductivity_raw <- as.numeric(data_tmp$conductivity_raw)
      data_tmp <- select(data_tmp, datetime, temp, conductivity_raw, gmt_offset)
      data_tmp$depth_m <- depth_num
      
      # Add a column w/ filename to see where the data came from.
      data_tmp$file <- fname
      
      # Add currently processed data to the data_merged data frame
      data_merged <- rbind(data_merged, data_tmp)
    } else {
      # If filename did not contain depth information, add it to the list of skipped files.
      skipped_files[length(skipped_files) + 1] <- fname
    }
  }
}


# Show info about skipped and processed files
n_processed <- length(processed_files)
n_skipped <- length(skipped_files)
message("Processed ", length(processed_files), " files.\nMerged ", n_processed - n_skipped, " files (", n_skipped, " skipped).")
if (length(skipped_files) > 0){
  for (fname in skipped_files){
    message("Skipped csv file: ", fname)
  }
}

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
message("Removed ", len_old - len_new, " duplicate rows.")

# Plot raw data
xlims <- c(min(data_merged$datetime), max(data_merged$datetime))
ylims <- c(min(data_merged$temp), max(data_merged$temp))
plot_raw <- ggplot(data=data_merged, mapping = aes(x=datetime, y=temp, color=depth_m)) +
  geom_line() + 
  ggtitle("Raw data") + 
  xlim(xlims) 
plot_03m_raw <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#33CCCC" ) +
  ggtitle("03 m raw") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_07m_raw <- ggplot(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#009999" ) +
  ggtitle("07 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_15m_raw <- ggplot(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#006666" ) +
  ggtitle("15 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_25m_raw <- ggplot(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#003333" ) +
  ggtitle("25 m raw") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)


# Sort the data by datetime
data_merged <- data_merged[order(data_merged$datetime),]

# Add a numerical timestamp
data_merged$month <- month(data_merged$datetime)
data_merged$year <- year(data_merged$datetime)
data_merged$day <- day(data_merged$datetime)
data_merged$hour <- hour(data_merged$datetime)
data_merged$timenum <- data_merged$year * 1000000 + data_merged$month * 10000 + data_merged$day * 100 + data_merged$hour


data_raw <- data_merged


# Remove conductivity below threshold rows
c0 <- dim(data_merged)[1]
data_merged <- data_merged[is.na(data_merged$conductivity_raw) | data_merged$conductivity_raw > COND_THRESHOLD,]
c1 <- dim(data_merged)[1]
message("Removed ", c0 - c1, " rows where conductivity <= ", COND_THRESHOLD)


# Remove rows with NA in temperature:
temp_na_0 <- dim(data_merged)[1]
data_merged <- data_merged[!is.na(data_merged$temp), ]
temp_na_1 <- dim(data_merged)[1]
message("Removed ", temp_na_0 - temp_na_1, " rows where temp = NA.")


# Delete manually marked outliers
if (DELETE_OUTLIERS){
  data_merged$id <- paste(as.character(data_merged$datetime), data_merged$depth_m, sep = "_")
  d1 <- dim(data_merged)[1]
  outliers <- read.csv(fname_outliers)
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
  message("Dropped ", d1 - d2, " rows of manually marked outliers.")
}


# # Aggregate data NOTE: Be patient, this might take a while.
message("Aggregating data. This might take a while...")
data_merged <- aggregate(data_merged, by = list(data_merged$datetime, data_merged$depth_m), FUN = "mean")
# Drop columns with NAs that were created during aggregate()
data_merged <- subset(data_merged, select = -c(depth_m, datetime, file, id))
# Update the column names
names(data_merged)[names(data_merged) == 'Group.1'] <- "datetime"
names(data_merged)[names(data_merged) == 'Group.2'] <- "depth_m"

# Create a data frame in wide format
data_wide <- reshape(data_merged, idvar = "datetime", timevar = "depth_m", direction = "wide")
# Adjust column names
colnames(data_wide) <- str_replace(colnames(data_wide), "\\.", "_")
for (i  in 2:length(colnames(data_wide))){
  colnames(data_wide)[i] <- paste(colnames(data_wide)[i], "m", sep = "")
}

# Plotting 
plot_all <- ggplot(data = data_merged, mapping = aes(x = datetime, y = temp, color = depth_m)) +
  geom_line() + 
  ggtitle("All depths")
plot_03m <- ggplot(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#33CCCC" ) +
  ggtitle(label = "03 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_07m <- ggplot(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#009999" ) +
  ggtitle("07 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#006666" ) +
  ggtitle("15 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_25m <- ggplot(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x = datetime, y = temp)) + 
  geom_line(color="#003333" ) +
  ggtitle("25 m") +
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)


plot_comparison_03 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 3, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 3, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "03 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_comparison_07 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 7, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 7, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "07 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_comparison_15 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 15, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 15, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "15 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)
plot_comparison_25 <- ggplot() +
  geom_line(data = data_raw[data_raw$depth_m == 25, ], mapping = aes(x=datetime, y=temp), color="red") +
  geom_line(data = data_merged[data_merged$depth_m == 25, ], mapping = aes(x=datetime, y=temp), color=C07) +
  ggtitle(label = "25 m") + 
  xlab("Time") + ylab("Temp. (°C)") +
  xlim(xlims) + ylim(ylims)


# Save files
data_merged$datetime <- as.character(data_merged$datetime)
data_wide$datetime <- as.character(data_wide$datetime)
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
write_csv(data_merged, paste(SAVE_LOCATION, "_merged_", date_now, ".csv", sep = ""))
write_csv(data_wide, paste(SAVE_LOCATION, "_wide_", date_now, ".csv", sep = ""))
data_raw$datetime <- as.character(data_raw$datetime)
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
write_csv(data_raw, paste(SAVE_LOCATION, "_raw_", date_now, ".csv", sep = ""))


# grid.arrange(plot_03m_raw, plot_07m_raw, plot_15m_raw, plot_25m_raw, ncol=1)
# grid.arrange(plot_comparison_03, plot_comparison_07, plot_comparison_15, plot_comparison_25, ncol=1)
# plot_15m
# plot_raw
# plot_all
# ggsave(paste(SAVE_LOCATION, "plot_all.png", sep = "/"), plot_all)
# plot_stacked <- ggplot(data = data_merged, aes(x = datetime, y = temp)) + 
#   geom_line(aes(color = depth_m)) + 
#   facet_grid(depth_m ~ ., scales = "free_y") + theme(legend.position = "none")
