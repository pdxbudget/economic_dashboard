make_plot <- function(df, x = date, y = value, group = NULL) {
  
  if (is.null(group)) {
    plot <- df %>%
      ggplot(aes(x = date, y = value)) +
      geom_line()
  } else {
    plot <- df %>%
      ggplot(aes_string(x = "date", y = "value", group = group)) +
      geom_line(aes_string(color = group))
  }

  plot <- plot +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_breaks = "3 months" , date_labels = "%b %Y") +
      scale_color_viridis(discrete=TRUE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank())
  
  ggplotly(plot)
}
