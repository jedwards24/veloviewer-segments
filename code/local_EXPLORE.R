
library(tidyverse)
library(edwards)
library(lubridate)

file <- latest_file("data_processed", "local", ext = "RDS")
file <- latest_file("data_processed", "settle", ext = "RDS")

dt <- readRDS(file) %>% 
  select(-city)

dt %>% 
  arrange(climb, full) %>% 
  add_count(climb) %>% 
  filter(n > 2) %>% view

dt %>% 
  filter(full == 1) %>% 
  count(year(when))

# PR pre 2019
dt %>% 
  filter(full == 1) %>% 
  filter(year(when) < 2019) %>% 
  arrange(score) %>% 
  select(-c(sub, full, notes, area, dist_away, behind_pc, vam, p_vam, 
            heart_rate, cadence, power_w, power_meter, segment_id, direction)) %>% 
  mutate(name = str_sub(name, 1, 20)) %>% 
  select(-activity)

# low score
dt %>% 
  filter(full == 1) %>% 
  arrange(score) %>% 
  mutate(power = if_else(power_meter, power_w, NA_real_)) %>% 
  select(-c(name, sub, full, notes, area, dist_away, vam, p_vam, 
            heart_rate, cadence, power_w, power_meter, segment_id, direction, cat)) %>% 
  select(-activity) %>% view

n_distinct(dt$climb) #33 unique climbs

names(dt)
count(dt, activity, sort = T) %>% prinf

# power -----------
dtp <- dt %>% 
  filter(power_meter == 1) %>%
  filter(full == 1) %>% 
  arrange(desc(time)) %>% 
  mutate(cum_max_power = cummax(power_w)) %>% 
  mutate(is_max = (cum_max_power == power_w))

# just frontier
dtp %>% 
  filter(is_max) %>% 
  ggplot(aes(x = time, y = cum_max_power, label = climb)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  geom_line(group = 1) +
  theme_minimal()

# show all
dtp %>% 
  select(climb, time,  power_w, cum_max_power, is_max) %>% 
  ggplot(aes(x = time, y = power_w, label = climb, colour = is_max)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, NA) +
  ylim(200, NA)

# table
dtp %>% 
  filter(power_w == cum_max_power) %>% 
  select(power_w, time, elv_gain, name, climb) 

# power all segs --------------
# power 
latest <- latest_file("data_processed", "bike_segments")
dt_all <- readRDS(latest)

dtp2 <- dt_all %>% 
  filter(power_meter == 1) %>%
  mutate(time = time / 60) %>% 
  arrange(desc(time)) %>% 
  mutate(cum_max_power = cummax(power)) %>% 
  mutate(is_max = (cum_max_power == power))

#frontier
dtp2 %>% 
  filter(is_max) %>% 
  mutate(name = str_remove(name, "OFFICIAL (North-west Climbs|100Climbs)")) %>% 
  ggplot(aes(x = time, y = cum_max_power, label = name)) +
  geom_point() +
  ggrepel::geom_label_repel(max.overlaps = 20) +
  geom_line(group = 1) +
  theme_minimal() +
  scale_x_log10() +
  labs(x = "Time (mins)", y = "Power (W)")

# table
dtp2 %>% 
  filter(power == cum_max_power) %>% 
  select(power, time, elv_gain, name) %>% 
  prinf()

# behind vs score ---------
dt %>% 
  filter(score > 0) %>% 
  ggplot(aes(behind_pc, score)) +
  geom_point() +
  geom_smooth(method = 'lm')

dt %>% 
  filter(score < 70, score > 25)
