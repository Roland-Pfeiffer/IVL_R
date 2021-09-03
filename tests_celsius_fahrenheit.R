fname <- "file:///media/findux/DATA/Documents/IVL/Data/HOBO logger data 2020/Logger15m_20210823_Cond.csv"
data <- read.csv(fname, skip = 1, check.names = FALSE)
cnames <- colnames(data)

tempname <- get_colname(data, "Temp")
temp_units <- set_temp_to_celsius(data)