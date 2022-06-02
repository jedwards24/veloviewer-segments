# Create name lookup template to be used for renaming columns
# This just creates a starting point, which I further edited manually.

library(tidyverse)
library(fs)

ff <- fs::dir_ls("data", regexp = "vv_.*")
nms <- map(ff, ~colnames(read_csv(.))) 
name_info <- enframe(nms, name = "file", value = "col") %>% 
  unchop(col)
nms_count <- count(name_info, col)
rare <- nms_count %>% 
  filter(n < 8) %>% 
  pull(col)

name_look <- nms_count %>% 
  rename(raw_name = col) %>% 
  mutate(new_name = str_replace(raw_name, " %", "_pct"), .before = 2) %>% 
  mutate(new_name = snakecase::to_snake_case(new_name))
write_csv(name_look, "data_aux/col_name_lookup.csv")
prinf(name_look)
