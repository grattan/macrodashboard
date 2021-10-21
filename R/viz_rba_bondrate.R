
viz_rba_bondrate <- function(data = load_data(),
                              arg1 =  c(as.Date("1900-01-01"), Sys.Date()),
                              arg2 = NULL) {

  min_date <- arg1[1]
  max_date <- arg1[2]
 #Try agin
  lr <- auseconhist::butlin_t6 %>%
      mutate(date = lubridate::dmy(paste0("30-06", .data$year))) %>%
      select(.data$date, value = .data$bond_yield) %>%
      mutate(series = "Annual (Butlin)")

  daily <- data$rba_daily_yields %>%
    filter(.data$series_id == "FCMYGBAG10D") %>%
    select(.data$date, .data$value) %>%
    mutate(series = "Daily (RBA)")

  monthly <- data$rba_monthly_yields %>%
    filter(.data$series_id == "FCMYGBAG10") %>%
    select(.data$date, .data$value) %>%
    mutate(series = "Monthly (RBA)")

  lr <- lr %>%
    filter(.data$date <= min(monthly$date))

  monthly <- monthly %>%
    filter(.data$date <= min(daily$date))

  comb <- purrr::reduce(list(lr, monthly, daily), bind_rows)

  comb <- comb %>%
    filter(!is.na(.data$value),
           .data$date >= min_date,
           .data$date <= max_date)

  unique_series <- unique(comb$series)

  notes <- paste0("Note: ",
         if_else("Annual (Butlin)" %in% unique_series, "annual data to 1969; ", ""),
         if_else("Monthly (RBA)" %in% unique_series, "monthly data from 1969 to January 1995; ", ""),
         if_else("Daily (RBA)" %in% unique_series, "daily data from 1995 to present", ""),
         ".")

  source <- paste0("Sources: ",
                   if_else(any(grepl("RBA", unique_series)),
                           "RBA Table F2", ""),
                   if_else(any(grepl("RBA", unique_series)) &&
                                 any(grepl("Butlin", unique_series)),
                               " and ", ""),
                   if_else(any(grepl("Butlin", unique_series)),
                           "Butlin, Dixon and Lloyd (2014)", ""),
                   ".")

  comb %>%
    ggplot(aes(x = .data$date, y = .data$value )) +
    geom_line() +
    theme_grattan() +
    grattan_y_continuous(labels = function(x) paste0(x, "%"),
                         limits = c(min(0, min(comb$value)),
                                    max(17, max(comb$value)))) +
    labs(title = "It's never been cheaper for the Australian Government to borrow money",
         subtitle = "Nominal yield on Australian Government bond, 10 years maturity",
         caption = paste(notes, source, sep = " ")) +
    theme(axis.title = element_blank())

}
