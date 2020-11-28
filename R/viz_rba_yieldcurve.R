
viz_rba_yieldcurve <- function(data = load_data(),
                               arg1 = NULL,
                               arg2 = NULL) {
  df <- data$rba_detailed_yields

  df <- df %>%
    mutate(years_to_maturity = as.numeric(
      difftime(.data$maturity_date,
        .data$date,
        units = "weeks"
      ) / 52
    ))

  raw_dates <- max(df$date) -
    c(lubridate::days(0),
      lubridate::days(30),
      lubridate::years(1),
      lubridate::years(5),
      lubridate::years(10))

  find_nearest_date <- function(date, date_vec) {
    date_vec[which.min(abs(date_vec - date))]
  }

  highlighted_dates <- map(raw_dates,
                           find_nearest_date,
                           date_vec = df$date) %>%
    purrr::reduce(c)

  # df <- df %>%
  #   mutate(date_type = case_when(
  #     .data$date == max(.data$date) ~
  #     format(.data$date, "%d %b %Y"),
  #     .data$date == max(.data$date) - lubridate::days(30) ~
  #     "1 month earlier",
  #     .data$date == max(.data$date) - lubridate::years(1) ~
  #     "1 year earlier",
  #     .data$date == max(.data$date) - lubridate::years(5) ~
  #     "5 years earlier",
  #     .data$date == max(.data$date) - lubridate::years(10) ~
  #     "10 years earlier",
  #     TRUE ~ NA_character_
  #   )) %>%
  #   filter(!is.na(.data$date_type))

  df <- df %>%
    filter(.data$date %in% highlighted_dates)

  df %>%
    ggplot(aes(
      x = .data$years_to_maturity,
      y = .data$value,
      col = factor(.data$date)
    )) +
    geom_line() +
    grattan_point_filled(
      data = ~ group_by(., .data$date) %>%
        filter(.data$years_to_maturity == max(.data$years_to_maturity)),
      size = 2, fill = "white", stroke = 1
    ) +
    grattan_label_repel(
      data = ~ group_by(., .data$date) %>%
        filter(.data$years_to_maturity == max(.data$years_to_maturity)),
      # aes(label = stringr::str_wrap(.data$date_type, 7)),
      aes(label = format(date, "%d %b\n%Y")),
      direction = "y",
      segment.size = 0,
      hjust = 0.5,
      size = 16,
      nudge_x = 0 # 0.2
    ) +
    scale_y_continuous(
      limits = c(
        min(0, min(df$value)),
        max(6, max(df$value))
      ),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_continuous(
      expand = expansion(add = c(0, 2)),
      breaks = seq(0, 30, 10),
      limits = c(0, max(df$years_to_maturity))
    ) +
    theme_grattan() +
    # scale_colour_manual(values = cols) +
    grattan_colour_manual(n = length(unique(highlighted_dates))) +
    labs(
      x = "Years to maturity",
      subtitle = "Yield on Australian Government bonds",
      title = "The yield curve has shifted down"
    )
}
