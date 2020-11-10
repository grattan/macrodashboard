#' @importFrom stats reorder
viz_payrolls_construction_bystate <- function(data = load_data(),
                                              arg1 = "Construction",
                                              arg2 = NULL) {
  df <- data$payrolls_industry_jobs

  df$industry <- gsub("[[:alpha:]]-", "", df$industry)

  chart_data <- df %>%
    filter(
      .data$industry == "Construction",
      .data$age == "All ages",
      .data$sex == "Persons",
      .data$state %in% c("SA", "WA", "QLD", "VIC", "NSW")
    ) %>%
    group_by(.data$state) %>%
    filter(.data$date >= as.Date("2020-03-14"))

  top_of_range <- max(max(chart_data$value), 102)
  bottom_of_range <- min(min(chart_data$value), 90)

  latest <- chart_data %>%
    filter(.data$date == max(.data$date))

  title <- case_when(
    all(latest$value < 100) ~
    "Construction employment is down in all states",
    sum(latest$value < 100) >= 3 ~
    "Construction employment is down in most states",
    sum(latest$value < 100) >= 1 ~
    paste0(
      "Construction employment is down in ",
      paste0(latest$state[latest$value < 100], collapse = " and ")
    )
  )

  chart_data %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$state)) +
    geom_hline(yintercept = 100) +
    geom_line(size = 1) +
    geom_point(
      data = ~ filter(., date == max(.data$date)),
      size = 3,
      fill = "white",
      stroke = 1.5,
      shape = 21
    ) +
    grattan_label_repel(
      data = ~ filter(., .data$date == max(.data$date)),
      direction = "y",
      aes(label = paste0(
        .data$state, "\n",
        round(.data$value - 100, 1),
        "%"
      )),
      segment.size = 0,
      size = 16,
      nudge_x = 3,
      hjust = 0
    ) +
    scale_x_date(
      expand = expansion(mult = c(0, .12)),
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    scale_y_continuous(
      breaks = seq(bottom_of_range, top_of_range, 2),
      labels = function(x) paste0(x - 100, "%"),
      limits = c(bottom_of_range, top_of_range)
    ) +
    theme_grattan() +
    grattan_colour_manual(5) +
    theme(axis.title = element_blank()) +
    labs(
      title = title,
      subtitle = "Weekly payroll jobs indexed, change since 14 March 2020",
      caption = "Source: ABS Weekly Payroll Jobs and Wages in Australia."
    )
}
