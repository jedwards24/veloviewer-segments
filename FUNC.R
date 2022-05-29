
#
# Column removal:
# The `cmts` (comments) and `gear` columns are blank. 
# If all segments are bike then `type`, `pace_km`, and `pace_100_m` are redundant.
vv_process <- function(path,
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
  dat
}

check_id_cols <- function(dat) {
  dat <- select(dat, contains("Segment Id"))
  map_lgl(dat, ~identical(., dat[[1]]))
}

