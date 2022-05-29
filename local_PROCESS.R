# Processes full segment csv then selects some local climbs
# based on `local_climbs.csv` which I created.

library(tidyverse)
library(edwards)
library(snakecase)
library(lubridate)

latest <- latest_file("data", "vv_segments_bike")
file_date <- str_extract(latest, "\\d.*\\d")

local <- read_csv("data/local_climbs.csv")

# the cols "Dist km" and "Dist away km" are actually in meters
cols_lose <- c("segment_id_1", "trn", "type", "cmts", "gear", "pace_km", "pace_100_m") #not always present
dt <- read_csv(latest) %>%
  rename(starred = "★") %>% 
  rename(pos_change = `Pos +/-`) %>% 
  rename_all(~str_replace_all(., " %", "_pc")) %>% 
  rename_all(to_snake_case) %>% 
  select(-any_of(cols_lose)) %>% 
  rename(dist_away = dist_away_km) %>% #name varies between versions: dist_away_m or _km
  mutate(dist_away = dist_away / 1000) %>%
  rename(dist = dist_km) %>% 
  select(name, city, dist_away, pos_score, pos, total, tries, dist, elapsed_time, behind_leader_pc,
         grade_pc, elv_gain_m, cat, vam, p_vam, heart_rate, cadence, power_w, power_meter,
         activity, when, segment_id) %>% 
  mutate_at(vars(pos, total, elapsed_time), as.integer) %>% 
  mutate(power_meter = as.logical(power_meter)) %>% 
  mutate_if(is.double, ~round(., 1)) %>% 
  mutate(when = str_sub(when, 5, 15)) %>% 
  rename(score = pos_score,
         elv_gain = elv_gain_m,
         time = elapsed_time,
         grade = grade_pc,
         behind_pc = behind_leader_pc) %>% 
  mutate(time = time / 60) %>% 
  mutate_at(vars(elv_gain, dist), ~round(., 0)) %>% 
  mutate_at(vars(time), ~round(., 1)) 

dt2 <- local %>% 
  select(name, area, direction, climb, full, sub, notes, segment_id) %>% 
  left_join(dt, by = c("segment_id", "name")) %>%
  mutate(when = lubridate::mdy(when))

if (F){
  save_name <- glue::glue("data_processed/local_{file_date}.RDS")
  saveRDS(dt2, save_name)
}
