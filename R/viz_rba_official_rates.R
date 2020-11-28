
viz_rba_official_rates <- function(data = load_data(),
                                   arg1 = c(as.Date("1990-01-31"),
                                            Sys.Date()),
                                   arg2 = c("Australia",
                                            "Canada",
                                            "Euro area",
                                            "Japan",
                                            "UK",
                                            "US")) {

  countries <- arg2
  min_date <- arg1[1]
  max_date <- arg1[2]

  df <- data$rba_official_rates %>%
    select(.data$date, .data$series, .data$value) %>%
    filter(.data$date >= min_date &
             .data$date <= max_date)


  df <- df %>%
    mutate(country = case_when(grepl("Australia", series) ~ "Australia",
                               grepl("Euro", series) ~ "Euro area",
                               grepl("Canada", series) ~ "Canada",
                               grepl("Japan", series) ~ "Japan",
                               grepl("United Kingdom", series) ~ "UK",
                               grepl("United States", series) ~ "US"))

  df <- df %>%
    filter(.data$country %in% .env$countries)


  df %>%
    ggplot(aes(x = date, y = value, col = country)) +
    geom_step() +
    # grattan_point_filled(data = ~filter(., date == max(date))) +
    grattan_label_repel(data = ~group_by(., country) %>%
                          filter(date == max(date)),
                        aes(label = country),
                        segment.size = 0,
                        nudge_x = 70,
                        force = 3,
                        size = 16,
                        direction = "y",
                        hjust = 0) +
    theme_grattan() +
    grattan_colour_manual(6) +
    scale_x_date(expand = expansion(mult = c(0, .17))) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(axis.title = element_blank() ) +
    labs(title = "Interest rates are low everywhere",
         subtitle = "Official interest rates of major central banks",
         caption = "Source: RBA.")

  }
