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

Sys.setlocale("LC_TIME", "en_GB.UTF-8")

# -------------------------- FUNCTIONS -----------------------------------------
# Functions that will be used in the script section below.

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
  for (cname in colnames(data_df)){
    if (startsWith(cname, "Date Time")){
      dt_match_str <- str_extract(cname, "GMT[+|-]\\d\\d:\\d\\d")
    }}
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
  # Returns TRUE if dataframe is missing conductivity data, else returns FALSE.
  conductivity_missing <- TRUE
  for (cname in colnames(df)){
    if (startsWith(cname, "High Range") || startsWith(cname, "conductivity_raw")){
      conductivity_missing <- FALSE
    }
  }
  conductivity_missing
}






# -------------------------------- SCRIPT SECTION ------------------------------


# Adjust these according to your system and where your files are stored.
# NOTE: Make sure to use forward slashes!
PATH_TO_HOBO_FILES <- "/media/findux/DATA/Documents/IVL/Data/hobo_data_all/"
SAVE_LOCATION <- "/media/findux/DATA/Documents/IVL/Data/hobo_out/"
PATH_TO_OUTLIER_FILE <- "file:///media/findux/DATA/Documents/IVL/Data/outliers.csv"

# Set cutoff value for raw conductivity. Everything below is discarded.
COND_THRESHOLD <- 800

# Delete manually listed outliers in "outliers.csv" file.
DELETE_OUTLIERS <- TRUE

# Colors:
C03 <- "#33CCCC"
C07 <- "#009999"
C15 <- "#006666"
C25 <- "#336666"

# Initiate empty data frames and lists for later use.
data_merged <- data.frame()
skipped_files <- c()
processed_files <- c()

# Read filenames into a list called fnames.
# Only use the ones that have the pattern .csv at the end ($)
fnames <- list.files(PATH_TO_HOBO_FILES, pattern = ".csv$")

# Go over every single filename in fnames
for (fname in fnames){
  processed_files[length(processed_files) + 1] <- fname
  # If filename contains depth information, continue processing it.
  if (fname_contains_depth(fname)){
    depth <- extract_depth(fname)
    depth_num <- depth_as_number(depth)
    full_path_to_file <- paste(PATH_TO_HOBO_FILES, fname, sep = "/")
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
    # If filename didn't contain depth info, add to list of skipped files.
    skipped_files[length(skipped_files) + 1] <- fname
  }
}
rm(data_tmp)


# Show info about skipped and processed files
n_processed <- length(processed_files)
n_skipped <- length(skipped_files)
message("Processed ", length(processed_files), " files.\nMerged ",
        n_processed - n_skipped, " files (", n_skipped, " skipped).")
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

# Plot lims
xlims <- c(min(data_merged$datetime), max(data_merged$datetime))
ylims <- c(min(data_merged$temp), max(data_merged$temp))

# plot_temp_raw <- ggplot(data=data_merged,
#                         mapping = aes(x=datetime, y=temp, color=depth_m)) +
#   geom_line() + 
#   ggtitle("Raw data") + 
#   xlim(xlims) 
# 
# plot_temp_03m_raw <- ggplot(data = data_merged[data_merged$depth_m == 3, ],
#                             mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#33CCCC" ) +
#   ggtitle("03 m raw") + 
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_temp_07m_raw <- ggplot(data = data_merged[data_merged$depth_m == 7, ],
#                             mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#009999" ) +
#   ggtitle("07 m raw") +
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_temp_15m_raw <- ggplot(data = data_merged[data_merged$depth_m == 15, ],
#                             mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#006666" ) +
#   ggtitle("15 m raw") +
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_temp_25m_raw <- ggplot(data = data_merged[data_merged$depth_m == 25, ],
#                             mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#003333" ) +
#   ggtitle("25 m raw") +
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Sort the data by datetime
data_merged <- data_merged[order(data_merged$datetime),]

# Add a numerical timestamp
data_merged$year <- year(data_merged$datetime)
data_merged$month <- month(data_merged$datetime)
data_merged$day <- day(data_merged$datetime)
data_merged$hour <- hour(data_merged$datetime)
data_merged$timenum <- data_merged$year * 1000000 + 
  data_merged$month * 10000 + 
  data_merged$day * 100 + 
  data_merged$hour


data_raw <- data_merged


# Remove conductivity below threshold rows
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


# Delete manually marked outliers
if (DELETE_OUTLIERS){
  data_merged$id <- paste(as.character(data_merged$datetime),
                          data_merged$depth_m, sep = "_")
  d1 <- dim(data_merged)[1]
  outliers <- read.csv(PATH_TO_OUTLIER_FILE)
  outliers$id <- paste(outliers$datetime, outliers$depth, sep = "_")
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


# # Aggregate data NOTE: Be patient, this might take a while.
# message("Aggregating data. This might take a while...")
# data_merged <- aggregate(data_merged, by = list(data_merged$datetime,
#                                                 data_merged$depth_m), FUN = "mean", na.rm = TRUE)
# # Drop columns with NAs that were created during aggregate()
# data_merged <- subset(data_merged, select = -c(depth_m, datetime, file, id))
# # Update the column names
# names(data_merged)[names(data_merged) == 'Group.1'] <- "datetime"
# names(data_merged)[names(data_merged) == 'Group.2'] <- "depth_m"


# Create a data frame in wide format
data_wide <- reshape(data_merged, idvar = "datetime", timevar = "depth_m",
                     direction = "wide")

# Adjust column names
colnames(data_wide) <- str_replace(colnames(data_wide), "\\.", "_")
for (i  in 2:length(colnames(data_wide))){
  colnames(data_wide)[i] <- paste(colnames(data_wide)[i], "m", sep = "")
}

# # Plotting cleaned temperature data
# plot_temp_all <- ggplot(data = data_merged,
#                         mapping = aes(x = datetime, y = temp, color = depth_m)) +
#   geom_line() + 
#   ggtitle("All depths")
# 
# plot_temp_03m <- ggplot(data = data_merged[data_merged$depth_m == 3, ],
#                         mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#33CCCC" ) +
#   ggtitle(label = "03 m") + 
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_temp_07m <- ggplot(data = data_merged[data_merged$depth_m == 7, ],
#                         mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#009999" ) +
#   ggtitle("07 m") +
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_temp_15m <- ggplot(data = data_merged[data_merged$depth_m == 15, ],
#                         mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#006666" ) +
#   ggtitle("15 m") +
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# plot_temp_25m <- ggplot(data = data_merged[data_merged$depth_m == 25, ],
#                         mapping = aes(x = datetime, y = temp)) + 
#   geom_line(color="#003333" ) +
#   ggtitle("25 m") +
#   xlab("Time") + ylab("Temp. (°C)") +
#   scale_x_datetime(limits = xlims, date_breaks = "1 month", date_labels = "%b-'%y") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



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

# Save files
# data_merged$datetime <- as.character(data_merged$datetime)
data_wide$datetime <- as.character(data_wide$datetime)
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
write_csv(data_merged, paste(SAVE_LOCATION, "hobo_data_merged_", date_now, ".csv", sep = ""))
write_csv(data_wide, paste(SAVE_LOCATION, "hobo_data_wide_", date_now, ".csv", sep = ""))
data_raw$datetime <- as.character(data_raw$datetime)
date_now <- as.character(strptime(now(), format = "%Y-%m-%d"))
write_csv(data_raw, paste(SAVE_LOCATION, "hobo_data_raw_", date_now, ".csv", sep = ""))


# grid.arrange(plot_03m_raw, plot_07m_raw, plot_15m_raw, plot_25m_raw, ncol=1)
# grid.arrange(plot_temp_comparison_03m, plot_temp_comparison_07m,
#              plot_temp_comparison_15m, plot_temp_comparison_25m,
#              ncol=1)
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



rm(list = c("c0", "C03", "C07", "c1", "C15", "C25", "d1", "d2", "date_now", "depth", "depth_num",
            "fname", "fnames", "full_path_to_file", "i", "id_o", "len_new", "len_old",
            "n_processed", "n_skipped", "outlier_i", "outlier_ids", "outlier_indices",
            "temp_na_0", "temp_na_1", "temperature_units", "xlims", "ylims"))
