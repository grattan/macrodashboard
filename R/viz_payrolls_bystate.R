
viz_payrolls_bystate <- function(data = load_data(),
                                 arg1 = c( # "Australia",
                                   # "ACT",
                                   "NSW",
                                   # "NT",
                                   "QLD",
                                   "SA",
                                   "TAS",
                                   "VIC",
                                   "WA"
                                 ),
                                 arg2 = "jobs") {
  states <- arg1
  series <- arg2

  if (series == "jobs") {
    df <- data$payrolls_industry_jobs
    series_desc <- "number of payroll jobs"
  } else if (series == "wages") {
    df <- data$payrolls_industry_wages
    series_desc <- "wages bill for payroll jobs"
  } else {
    stop()
  }

  df <- df %>%
    dplyr::filter(
      .data$industry == "All industries",
      .data$sex == "Persons",
      .data$age == "All ages",
      .data$state %in% states,
      .data$date >= as.Date("2020-03-14")
    )

  cols <- c(
    "Australia" = "black",
    "VIC" = grattantheme::grattan_darkred,
    "NSW" = grattantheme::grattan_red,
    "QLD" = grattantheme::grattan_darkorange,
    "WA" = grattantheme::grattan_lightorange,
    "SA" = grattantheme::grattan_yellow,
    "TAS" = grattantheme::grattan_lightyellow,
    "ACT" = grattantheme::grattan_grey4,
    "NT" = grattantheme::grattan_grey3
  )

  latest <- df %>%
    filter(date == max(.data$date))

  title <- dplyr::case_when(
    all(latest$value < 100) ~ paste0("The total ", series_desc, " is still below March levels in every state"),
    any(latest$value < 100) ~ paste0(
      "The total ", series_desc, " is still below March levels in ",
      paste0(latest$state[latest$value < 100], collapse = ", ")
    ),
    TRUE ~ paste0("The total ", series_desc, " is above March levels in every state")
  )

  date_breaks <- ifelse(max(df$date) - min(df$date) >= 365,
    "2 months",
    "1 month"
  )

  date_labels <- ifelse(lubridate::year(max(df$date)) ==
    lubridate::year(min(df$date)),
  "%b",
  "%b\n%Y"
  )

  df %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$state)) +
    geom_hline(yintercept = 100) +
    geom_line() +
    geom_point(
      data = latest,
      size = 2.5, stroke = 1.5, fill = "white",
      shape = 21
    ) +
    grattantheme::grattan_label_repel(
      data = latest,
      aes(label = paste0(.data$state, "\n", round(.data$value - 100, 1), "%")),
      hjust = 0,
      size = 16,
      segment.size = 0,
      direction = "y",
      nudge_x = 4
    ) +
    scale_colour_manual(values = cols) +
    scale_x_date(
      expand = expansion(mult = c(0, 0.10)),
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x - 100, "%"),
      limits = c(
        min(90, min(df$value)),
        max(100, max(df$value))
      ),
      n.breaks = 6
    ) +
    theme_grattan() +
    theme(axis.title = element_blank()) +
    labs(
      title = title,
      subtitle = paste0("Total ", series_desc, " by state, percentage change since 14 March 2020"),
      caption = "Source: ABS Weekly Payroll Jobs and Wages."
    )
}
