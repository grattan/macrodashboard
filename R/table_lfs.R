#' @importFrom rlang .data
table_lfs_summary <- function(data = load_data()) {
  df <- data$lfs_m_1

  raw_summ <- df %>%
    filter(
      .data$series_type == "Seasonally Adjusted",
      !grepl("work", .data$series)
    ) %>%
    mutate(months_since_latest = lubridate::interval(.data$date, max(.data$date)) %/%
      months(1)) %>%
    filter(.data$months_since_latest %in% c(0, 1, 12)) %>%
    select(.data$months_since_latest, .data$series, .data$value) %>%
    readabs::separate_series() %>%
    select(-.data$series) %>%
    rename(series = .data$series_1, sex = .data$series_2) %>%
    spread(key = .data$months_since_latest, value = .data$value) %>%
    mutate(
      abs_1m = .data$`0` - .data$`1`,
      abs_12m = .data$`0` - .data$`12`,
      perc_1m = 100 * ((.data$`0` / .data$`1`) - 1),
      perc_12m = 100 * ((.data$`0` / .data$`12`) - 1),
      sex = factor(.data$sex, levels = c("Persons", "Females", "Males"))
    )


  raw_summ %>%
    gt::gt(rowname_col = "sex", groupname_col = "series")
}
