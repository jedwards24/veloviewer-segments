# Explore local climbs in Settle

library(tidyverse)
library(edwards)
source("FUNC.R")

latest <- latest_file("data_processed", "bike_seg")
file_date <- str_extract(latest, "\\d{4}-\\d{2}-\\d{2}")
dt <- readRDS(latest)

names(dt)
dt2 <- filter(dt, dist_away < 15, elv_gain_m > 90, grade_pct > 3) %>%   
  select(name, city, dist_km, grade_pct, elv_gain_m, dist_away, cat) %>% 
  arrange(dist_away)
count(dt2, city)
count(dt2, cat)
dt2
prinf(dt2)
datapasta::dpasta(dt2$name)
seg_names <- c("Townhead Lane to Crummack Gravel", "Stainforth - Halton Gill Total Climb Accurate", 
               "Old Rd full climb east to west", "Henside Rd Climb", "Steep Section Langcliffe Scar",
               "Langcliffe Scar", "langcliffe church to henshaw rd.", "Halton Gill Between Cattle Grids", 
               "Ooossshhh #270 Halton Gill", "Craven Bank Ln Climb", 
               "OFFICIAL 100Climbs No143 Bowland Knotts", 
               "High Hill Lane OFFICIAL 100 Climbs X-List No63", "Victoria St to Top of High Hill Lane", 
               "Brootes Ln Climb", "Bowland Knotts proper", "Dent Dale Head",  
               "Arncliffe proper", "Newby Head official climb", 
               "Gordale Ln Climb", "Malham Cove",  "Steeeeep", "Bentham bridge to Cross O'Greet", 
               "Cross O' Greet from Aikengill Rd", "Ingleton to Ribblehead",
               "Settle nipple")
# To add: climb from kirkby malham
dts <- dt %>% 
  filter(name %in% seg_names) %>% 
  arrange(name) %>% 
  select(segment_id, name, dist_km, dist_away) 
dts2 <- dts %>% 
  slice(-15) %>% 
  arrange(dist_away)
prinf(dts2)
pull(dts2, segment_id) %>% 
  datapasta::dpasta()
c(28975969, 4955682, 6474203, 998225, 7818808, 6688008, 8181855, 7081580, 14906198, 984974, 
  16336352, 29205109, 12499508, 714980, 5613513, 5513688, 14867453, 1546074, 17652953, 816007, 
  6688083, 15367928, 4651703, 8556711, 1345351)
write_csv(dts2, "data_aux/settle_local.csv")
