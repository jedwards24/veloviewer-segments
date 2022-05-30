
#
# Column removal:
# The `cmts` (comments) and `gear` columns are blank. 
# If all segments are bike then `type`, `pace_km`, and `pace_100_m` are redundant.
vv_process <- function(path,
                       location = "lancaster",
                       col_remove = c("trn", "type", "cmts", "gear", "pace_km", "pace_100_m")) {
  names_lookup <- read_csv("data_aux/col_name_lookup.csv", col_types = "ccd")
  dat <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  cli::cli_alert_success("Read {path}.")
  id_check <- check_id_cols(dat)
  if (!all(id_check)){
    cli::cli_alert_danger("Not all Segment Id columns are the same.")
    return(dat)
  }
  dat <- select(dat, !all_of(names(id_check)[-1])) %>% 
    rename_with(~str_replace(., "Segment Id.*", "Segment Id"))
  cli::cli_alert_info("Removed {length(id_check) - 1} redundant segment id column{?s}.")
  if (!all(names(dat) %in% names_lookup$raw_name)){
    cli::cli_alert_warning("There are column names not seen before: 
                           {setdiff(names(dat), names_lookup$raw_name)}")
  }
  
  names_vec <- names_lookup %>% 
    filter(raw_name %in% names(dat)) %>% 
    select(new_name, raw_name) %>% 
    deframe()
  dat <- rename(dat, any_of(names_vec)) %>% 
    select(-any_of(col_remove)) 

  if (median(dat$dist_away) > 1000){
    cli::cli_alert_info("`dist_away` was in metres and has been converted to km.")
    dat <- mutate(dat, dist_away = dist_away / 1000)
  }
  attr(dat, "location") <- location
  dat
}

check_id_cols <- function(dat) {
  dat <- select(dat, contains("Segment Id"))
  map_lgl(dat, ~identical(., dat[[1]]))
}

# Further processing for output of vv_process() and select only 
# rows in local climbs data.
# test for consistent names??
vv_process_local <- function(dt) {
  local <- get_local_data(attr(dt, "location"))
  
  dt2 <- dt %>% 
    select(city, dist_away, score, pos, total, tries, dist, time, behind_pct,
           grade, elv_gain, cat, vam, p_vam, heart_rate, cadence, power, power_meter,
           activity, when, segment_id) %>% 
    mutate(across(c(pos, total, time), as.integer)) %>% 
    mutate(power_meter = as.logical(power_meter)) %>% 
    mutate_if(is.double, ~round(., 1)) %>% 
    mutate(when = str_sub(when, 5, 15)) %>% 
    mutate(time = round(time / 60, 1)) %>% 
    mutate(across(c(elv_gain, dist), ~round(., 0))) 
  
  local_cols <- c("name", "area", "direction", "climb", "full", "sub", "notes", "segment_id")
  local %>% 
    select(any_of(local_cols)) %>% 
    left_join(dt2, by = c("segment_id")) %>%
    mutate(when = lubridate::mdy(when))
}

get_local_data <- function(location) {
  path <- if (location == "lancaster") "data/local_climbs.csv" else "data_aux/settle_local.csv"
  out <- read_csv("data_aux/settle_local.csv", show_col_types = FALSE, progress = FALSE)
  attr(out, "location") <- location
  out
}