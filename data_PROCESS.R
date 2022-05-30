# Read and process csv of segment data generated from Veloviewer
# When generating data click on any entry in the "Dist away km" column first to get numeric values
# The columns in the data have varied over time.
library(tidyverse)
library(edwards)
source("FUNC.R")

latest <- latest_file("data", "vv_bike")
file_date <- str_extract(latest, "\\d{4}-\\d{2}-\\d{2}")
dt <- vv_process(latest, location = "settle")
save_name <- paste0("data_processed/bike_segments_", file_date, ".RDS")
save_check(dt, save_name, overwrite = T)

dt2 <- vv_process_local(dt)
save_name <- paste0("data_processed/", attr(dt2, "location"), "_", file_date, ".RDS")
save_check(dt2, save_name, overwrite = T)
