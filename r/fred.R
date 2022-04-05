get_fredr_series <- function(series_id, file_name) {
  
  if (
    # if rds file is NOT in the inputs folder OR...
    !file.exists(paste0("inputs/", file_name, ".rds")) |
    # if FRED data was last modified last month (or before that)...
    month(file.mtime(paste0("inputs/", file_name, ".rds"))) <
    month(today())) {
    # ... fetch new data
    
    params <- get("params")
    
    data <- map(series_id, fredr,
                observation_start = params$start_date,
                observation_end = params$end_date) %>%
      bind_rows() %>%
      mutate(series_id = str_trunc(series_id, 4, "right", ""),
             series_id = case_when(
               series_id == "PORT" ~ "Portland",
               series_id == "AUST" ~ "Austin",
               series_id == "INDI" ~ "Indianapolis"))
    
    saveRDS(data, paste0("inputs/", file_name, ".rds"))
    
    return(data)
  } else {
    readRDS(paste0("inputs/", file_name, ".rds"))
  }
}

get_fred_latest_value <- function(df) {
  if (unique(df$series_id) == 1) {
    # if only 1 series_id, assume that we're not 
    # comparing to peer regions
    df %>%
      filter(date == max(date)) %>%
      pull(value)
  } else {
    df %>%
      filter(series_id == "Portland",
             date == max(date)) %>%
      pull(value)
  }
}