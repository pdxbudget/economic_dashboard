plot_fred <- function(df) {
  ggplotly(
    df %>%
      ggplot(aes(x = date, y = value, group = series_id)) +
      geom_line(aes(color = series_id)) +
      scale_y_continuous() +
      scale_x_date(date_breaks = "3 months" , date_labels = "%b %Y") +
      scale_color_viridis(discrete=TRUE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45),
            axis.title.x = element_blank())
  )
}
