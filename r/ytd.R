get_current_ytd <- function(df, date, value) {
  df %>%
  filter(!!sym(date) < (params$end_date + months(1)),
         !!sym(date) > (floor_date(params$end_date, unit = "year") - months(1))) %>%
  summarize(!!value := sum(!!sym(value), na.rm = TRUE)) %>%
  pull(value)
}