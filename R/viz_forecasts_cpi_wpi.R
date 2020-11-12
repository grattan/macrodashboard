
viz_forecasts_cpi_wpi <- function(data = load_data(),
                                  arg1 = NULL,
                                  arg2 = NULL) {
  cpi <- data$cpi_8 %>%
    filter(.data$series_id == "A3604511X") %>%
    select(.data$date, .data$value) %>%
    mutate(series = "cpi")

  wpi <- data$wpi_1 %>%
    filter(.data$series_id == "A83895396W") %>%
    select(.data$date, .data$value) %>%
    mutate(series = "wpi")

  actuals <- bind_rows(cpi, wpi) %>%
    mutate(type = "actual")

  # Load RBA forecasts
  raw_forecasts <- data$rba_forecasts
  raw_forecasts <- filter(raw_forecasts,
                          .data$forecast_date ==
                            max(.data$forecast_date))

  smp_date <- raw_forecasts %>%
    pull(.data$forecast_date) %>%
    unique()

  forecasts <- raw_forecasts %>%
    filter(series %in% c("wpi_change", "underlying_annual_inflation")) %>%
    select(.data$date, .data$series, .data$value) %>%
    mutate(series = case_when(grepl("wpi", .data$series) ~ "wpi",
                              grepl("inflation", .data$series) ~ "cpi",
                              TRUE ~ NA_character_)) %>%
    mutate(type = "forecast")

  # Combine actual and forecast
  combined <- forecasts %>%
    bind_rows(actuals) %>%
    filter(date >= as.Date("2005-01-01"))

  # Title
  max_cpi_forecast <- max(forecasts$value[forecasts$series == "cpi"])

  title <- case_when(max_cpi_forecast < 2 ~
                       "Inflation is forecast to remain below the RBA's target",
                     max_cpi_forecast < 2.5 ~
                       "Inflation is forecast to be in the bottom half of the RBA's target band",
                     max_cpi_forecast < 3 ~
                       "Inflation is forecast to be in the top half of the RBA's target band",
                     max_cpi_forecast > 3 ~
                       "Inflation is forecast to be above the RBA's target band")

  # Graph
  forecast_band <- summarise(forecasts,
                             min_date = min(.data$date),
                             max_date = max(.data$date))

  combined %>%
    mutate(series = case_when(series == "cpi" ~ "Inflation",
                              series == "wpi" ~ "Wages growth",
                              TRUE ~ .data$series)) %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$type)) +
    geom_rect(data = forecast_band,
              inherit.aes = FALSE,
              aes(xmin = .data$min_date, xmax = .data$max_date,
                  ymin = -Inf, ymax = Inf),
              fill = grattantheme::grattan_lightorange,
              size = 0,
              alpha = 0.1) +
    geom_text(data = tibble(series = "Inflation",
                            x = c(stats::median(combined$date),
                                  mean(c(forecast_band$min_date,
                                         forecast_band$max_date))),
                            y = c(3.15, 4.5),
                            type = c("actual", "forecast")),
              aes(x = .data$x, y = .data$y, label = tools::toTitleCase(.data$type)),
              # inherit.aes = FALSE,
              size = 16 / .pt) +
    geom_line() +
    facet_wrap(~.data$series) +
    scale_colour_manual(values = c("black",
                                   grattantheme::grattan_lightorange)) +
    grattan_y_continuous(limits = c(0, 5),
                         labels = function(x) paste0(x, "%")) +
    theme_grattan() +
    theme(axis.title = element_blank() ) +
    labs(title = title,
         subtitle = "Actual and forecast CPI inflation and wages growth",
         caption = paste0("Notes: Inflation refers to the trimmed mean. Wages growth is the Wage Price Index. Both series are seasonally adjusted. Source: ABS Consumer Price Index; ABS Wage Price Index; RBA Statement on Monetary Policy ", lubridate::month(smp_date, label = TRUE, abbr = FALSE), " ", lubridate::year(smp_date)))

}
