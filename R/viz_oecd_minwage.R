
viz_oecd_minwage <- function(data = load_data(),
                             arg1 = c("mean", "median"),
                             arg2 = TRUE) {

  df <- data$oecd_min2ave
  .series <- match.arg(arg1)
  .show_range <- arg2

  df <- df %>%
    mutate(series = tolower(.data$series)) %>%
    filter(.data$series == .series,
           .data$time >= 1985)

  if (.show_range == TRUE) {
    non_aus <- df %>%
      filter(.data$country != "Australia") %>%
      group_by(.data$time) %>%
      summarise(min = min(.data$obs_value),
                max = max(.data$obs_value))

    chart_data <- df %>%
      filter(.data$country == "Australia") %>%
      select(.data$time, aus = .data$obs_value) %>%
      left_join(non_aus, by = "time")

    base_chart <- chart_data %>%
      ggplot(aes(x = .data$time)) +
      geom_ribbon(aes(ymin = .data$min, ymax = .data$max),
                  fill = grattantheme::grattan_grey3,
                  alpha = 0.33,
                  size = 0) +
      geom_line(aes(y = .data$aus)) +
      grattan_label_repel(data = ~filter(., time == max(.data$time)) %>%
                            mutate(mid = (.data$min + .data$max) / 2.5) %>%
                            select(time,
                                   "Australia" = .data$aus,
                                   "Other OECD countries" = .data$mid) %>%
                            gather(key = "key", value = "value",
                                   -.data$time),
                          aes(y = .data$value,
                              label = stringr::str_wrap(.data$key, 5),
                              col = .data$key),
                          hjust = 0,
                          direction = "y",
                          segment.size = 0,
                          size = 14,
                          nudge_x = 0.2) +
      scale_colour_manual(values = c("Australia" = grattantheme::grattan_lightorange,
                                     "Other OECD countries" = grattantheme::grattan_grey3)) +
      theme_grattan()

  } else {
    main_countries <- c("Australia" = grattantheme::grattan_lightorange,
              "Colombia" = "black",
              "New Zealand" = grattantheme::grattan_darkorange,
              "Canada" = grattantheme::grattan_red,
              "France" = grattantheme::grattan_lightyellow,
              "United Kingdom" = grattantheme::grattan_darkred,
              "United States" = grattantheme::grattan_yellow)

    other_countries <- unique(df$country)[!unique(df$country) %in% names(main_countries)]

    other_countries <- rep(grattantheme::grattan_grey2, length(other_countries)) %>%
      purrr::set_names(other_countries)

    cols <- c(main_countries, other_countries)

    base_chart <- df %>%
      ggplot(aes(x = .data$time, y = .data$obs_value,
                 group = .data$country, col = .data$country)) +
      geom_line() +
      geom_line(data = ~filter(., .data$country %in% names(main_countries))) +
      geom_point(data = ~filter(.,
                                .data$country %in% names(main_countries),
                                .data$time == max(.data$time)),
                 fill = "white",
                 size = 3, stroke = 1.5, shape = 21) +
      grattan_label_repel(data = ~filter(.,
                                         .data$country %in% c(names(main_countries),
                                                              "Ireland"),
                                         .data$time == max(.data$time)) %>%
                            mutate(label = ifelse(.data$country == "Ireland", "Other OECD countries", .data$country)),
                          aes(label = stringr::str_wrap(.data$label, 7)),
                          size = 14,
                          hjust = 0,
                          nudge_x = 0.33,
                          segment.size = 0,
                          direction = "y") +
      scale_colour_manual(values = cols) +
      theme_grattan()
  }


  base_chart +
    grattan_y_continuous(limits = c(0, ceiling(max(df$obs_value) / .2) /5),
                         labels = function(x) paste0(x * 100, "%")) +
    labs(title = "Australia's minimum wage has been drifting towards the middle of the OECD pack",
         subtitle = paste0("Minimum wage as a percentage of ",
                           ifelse(.series == "mean", "average", "median"),
                           " wage in OECD countries"),
         caption = "Source: OECD.Stat") +
    theme(axis.title = element_blank() ) +
    scale_x_continuous(expand = expansion(mult = c(0, .12)))
}
