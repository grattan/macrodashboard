
viz_apple_mobility <- function(data = load_data(),
                               arg1 = c("Perth",
                                          "Sydney",
                                          "Melbourne"),
                               arg2 = NULL) {

  df <- data$apple_mobility

  cities <- arg1

  df <- df %>%
    filter(.data$geo_type == "city",
           .data$region %in% cities) %>%
    pivot_longer(
      cols = dplyr::starts_with("202"),
      names_to = "date"
    ) %>%
    mutate(date = as.Date(date))

  df <- df %>%
    group_by(.data$region, .data$transportation_type) %>%
    arrange(date) %>%
    mutate(weekly_ave = zoo::rollmeanr(.data$value, k = 7, fill = NA)) %>%
    ungroup() %>%
    filter(!is.na(.data$weekly_ave))

  df <- df %>%
    mutate(transportation_type = tools::toTitleCase(.data$transportation_type),
           transportation_type = if_else(.data$transportation_type == "Transit",
                                         "Public transport",
                                         .data$transportation_type))

  df <- df %>%
    filter(date >= lubridate::dmy("01-02-2020")) %>%
    group_by(.data$region) %>%
    mutate(cf_feb = (100 * (.data$weekly_ave / mean(.data$weekly_ave[lubridate::month(date) == 2]))) - 100)

  max_y <- max(25, max(df$cf_feb))

  # Graph -----
  df %>%
    ggplot(aes(x = .data$date, y = .data$cf_feb, col = .data$region)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point(data = ~filter(., date == max(date))) +
    grattan_label_repel(data = ~filter(., date == max(date)),
                        aes(label = paste0(if_else(.data$cf_feb > 0, "+", ""),
                                           round(.data$cf_feb, 0))),
                        hjust = 0,
                        size = 14,
                        direction = "y",
                        segment.size = 0,
                        nudge_x = 12) +
    grattan_label(data = ~filter(., .data$transportation_type == "Public transport") %>%
                    group_by(.data$transportation_type, .data$region) %>%
                    summarise(latest = .data$cf_feb[date == max(date)]) %>%
                    ungroup() %>%
                    arrange(-latest) %>%
                    mutate(x = as.Date("2020-04-01"),
                           y = (max_y + 6.5) - (row_number() * 6.5)),
                  inherit.aes = F,
                  aes(x = .data$x, y = .data$y,
                      label = .data$region, col = .data$region),
                  vjust = 1,
                  hjust = 0,
                  size = 16) +
    grattan_colour_manual(n = length(cities), reverse = T) +
    facet_wrap(~.data$transportation_type) +
    theme_grattan() +
    grattan_y_continuous(limits = c(-100, max_y),
                         breaks = seq(-75, max_y, 25),
                         labels = function(x) paste0(x, "%")) +

    scale_x_date(breaks = seq.Date(lubridate::ymd("2020-02-01"), max(df$date), length.out = 4L),
                 date_labels = "%b",
                 expand = expansion(mult = c(0, 0.16))) +
    theme(axis.title = element_blank(),
          panel.spacing = unit(1.5, "lines")) +
    labs(title = "Movement has recovered, but is mostly still below pre-COVID levels",
         subtitle = "Change in the number of requests for directions via Apple Maps since February (seven-day moving average)",
         caption = "Source: Apple.")

}
