

viz_corelogic_shutdown_lines <- function(data = load_data(),
                                         arg1 = as.Date("2020-01-01"),
                                         arg2 = as.Date("2020-03-22")) {
  df <- data[["corelogic_daily"]]

  min_date <- arg1
  index_date <- arg2

  df <- df %>%
    select(-agg) %>%
    # filter(date >= min_date) %>%
    pivot_longer(
      cols = -date,
      names_to = "city"
    ) %>%
    mutate(city = tools::toTitleCase(city)) %>%
    group_by(city) %>%
    mutate(value = 100 * (value / value[date == index_date]))


  labels <- df %>%
    group_by(city) %>%
    filter(date == max(date)) %>%
    mutate(
      change = (value / 100) - 1,
      label = paste0(
        city,
        "\n",
        if_else(change > 0, "+", ""),
        round(change * 100, 1),
        "%"
      )
    )

  top_of_range <- max(c(102, max(df$value)))

  df %>%
    filter(date >= min_date) %>%
    ggplot(aes(x = date, y = value, col = city)) +
    geom_hline(yintercept = 100) +
    geom_vline(xintercept = index_date) +
    geom_line() +
    geom_point(
      data = labels,
      size = 2, stroke = 1.25, fill = "white", shape = 21
    ) +
    grattantheme::grattan_label_repel(
      data = labels,
      aes(
        label = label,
        y = value
      ),
      hjust = 0,
      direction = "y",
      nudge_x = 5,
      segment.size = 0,
      size = 14
    ) +
    grattantheme::theme_grattan() +
    scale_y_continuous(
      limits = c(
        min(df$value) - 2.5,
        max(c(102, df$value))
      ),
      labels = function(x) paste0(x - 100, "%")
    ) +
    scale_x_date(
      date_labels = "%e\n%b\n%Y",
      breaks = c(
        min(df$date),
        index_date,
        max(df$date)
      ),
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_colour_manual(values = c(
      "Melbourne" = grattantheme::grattan_red,
      "Sydney" = grattantheme::grattan_lightorange,
      "Adelaide" = grattantheme::grattan_darkred,
      "Brisbane" = grattantheme::grattan_darkorange,
      "Perth" = grattantheme::grattan_yellow
    )) +
    theme(
      strip.text = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 14),
      panel.spacing = unit(1.5, "lines")
    ) +
    labs(
      title = "House price changes in Australian capital cities",
      subtitle = paste0("Cumulative change in home values since ",
                        format(index_date, "%d %B %Y"),
                        " (per cent)"),
      caption = "Sources: Corelogic and Grattan analysis."
    )
}
