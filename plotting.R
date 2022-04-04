plot_fred <- function(df) {
  ggplotly(
    df %>%
      ggplot(aes(x = date, y = value, group = series_id)) +
      geom_line(aes(color = series_id)) +
      scale_y_continuous() +
      scale_color_viridis(discrete=TRUE) +
      theme_minimal()
  )
}
