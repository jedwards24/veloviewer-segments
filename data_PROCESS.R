# Read and process csv of segment data generated from Veloviewer
# When generating data click on any entry in the "Dist away km" column first to get numeric values
# The columns in the data have varied over time.
#
# See local_PROCESS to process local climbs directly


library(tidyverse)
library(edwards)
library(snakecase)

latest <- latest_file("data", "vv_bike")
file_date <- str_extract(latest, "\\d{4}-\\d{2}-\\d{2}")
#dt <- read_csv("./vv_segments_bike_2020-04-25.csv")
dt <- read_csv(latest)
names(dt)

dt2 <- dt %>% 
  rename(starred = "★") %>% # warning <U+2605>
  rename(pos_change = `Pos +/-`) %>% 
  rename_all(~str_replace_all(., " %", "_pc")) %>% 
  rename_all(to_snake_case) 
  
names(dt2)  

if (!identical(dt2$segment_id, dt2$segment_id_1)){
  message("`segment_id` and `segment_id_1` are not identical.")
  compare_vars(dt2, segment_id, segment_id_1)
}

summary(dt2$dist_away_km)
# cmts (comments) and gear are blank.
# type, "pace_km", "pace_100_m" are redundant for if  segments are all bike
col_remove <- c("segment_id_1", "trn", "type", "cmts", "gear", "pace_km", "pace_100_m") 
dt3 <- dt2 %>%
  select(-any_of(col_remove)) %>% 
  rename(dist_away = dist_away_km)


if (median(dt2$dist_away_km) > 1000){
  message("`dist_away_km` was in metres and has been converted to km.")
  dt3 <- dt3  %>%  #actually metres 
    mutate(dist_away = dist_away / 1000)
}

if(F){
  save_name <- paste0("data_processed/bike_segments_", file_date, ".RDS")
  saveRDS(dt3, save_name)
}
