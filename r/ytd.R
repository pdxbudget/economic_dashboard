get_ytd <- function(df, type, date, value, params) {
  
  # date: string, colname with date values
  # value: string, colname with values
  
  if (type == "current") {
    df <- df %>%
      # adding 1 month to avoid <= and >=, which was causing issues with the dynamic colnames
      filter(!!sym(date) < (params$end_date + months(1)),
             !!sym(date) > (floor_date(params$end_date, unit = "year") - months(1)))
  } else if (type == "last") {
    df <- df %>%
      filter(!!sym(date) < (params$end_date - years(1)) + months(1),
             !!sym(date) > (floor_date(params$end_date, unit = "year") - years(1)) + months(1))
  }
  
 df %>%
  summarize(!!value := sum(!!sym(value), na.rm = TRUE)) %>%
  pull(value)
}