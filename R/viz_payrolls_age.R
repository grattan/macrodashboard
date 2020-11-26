viz_payrolls_age <- function(data = load_data(),
                             arg1 = NULL,
                             arg2 = NULL) {

  df <- data$payrolls_industry_jobs

  df <- df %>%
    filter(.data$sex == "Persons",
           .data$industry == "All industries",
           .data$series == "jobs",
           .data$state == "Australia",
           .data$age != "All ages",
           .data$date >= as.Date("2020-03-14"))

  df <- df %>%
    mutate(value = value - 100)

  break_increment <- 10
  min_y <- floor(min(df$value) / break_increment) * break_increment
  max_y <- ceiling(max(df$value) / break_increment) * break_increment

  min_x <- as.Date("2020-03-14")
  ceiling_month <- lubridate::ceiling_date(max(df$date), "month")
  max_x <- lubridate::ymd(
    paste(lubridate::year(ceiling_month),
          lubridate::month(ceiling_month),
          "14",
          sep = "-")
    )

  cols <- c("Under 20" = grattantheme::grattan_lightyellow,
            "20-29" = grattantheme::grattan_yellow,
            "30-39" = grattantheme::grattan_lightorange,
            "40-49" = grattantheme::grattan_darkorange,
            "50-59" = grattantheme::grattan_red,
            "60-69" = grattantheme::grattan_darkred,
            "70 and over" = "black")

  df %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$age)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point(data = ~filter(., .data$date == max(.data$date)),
               size = 3, stroke = 1.5, fill = "white", shape = 21) +
    grattan_label_repel(data = ~filter(., date == max(date)),
                        aes(label =
                              stringr::str_wrap(
                                paste0(.data$age, ": ",
                                       round(.data$value, 1), "%"),
                                6)
                            ),
                        direction = "y",
                        size = 14,
                        hjust = 0,
                        nudge_x = 4,
                        segment.size = 0) +
    grattan_y_continuous(limits = c(min_y, max_y),
                         breaks = seq(min_y, max_y, break_increment),
                         labels = function(x) paste0(x, "%")) +
    scale_x_date(breaks = seq.Date(from = min_x, to = max_x,
                          length.out = 5),
                 limits = c(min_x, max_x),
                 date_labels = "%b",
                 expand = expansion(mult = c(0, 0.04))) +
    scale_colour_manual(values = cols) +
    theme_grattan() +
    theme(axis.title = element_blank() ) +
    labs(title = "Jobs for teenagers fell fastest, but rebounded quickest",
         subtitle = "Change in payroll jobs since 14 March 2020",
         caption = "Source: ABS Weekly Payroll Jobs.")

}
