
viz_payroll_bysex <- function(data = load_data(),
                              arg1 = NULL,
                              arg2 = NULL) {
  df <- data$payrolls_industry_jobs

  df <- df %>%
    filter(
      .data$age == "All ages",
      .data$sex != "Persons",
      .data$industry == "All industries",
      .data$state == "Australia",
      .data$date >= as.Date("2020-03-14")
    )

  df <- df %>%
    mutate(sex = case_when(
      .data$sex == "Females" ~ "Women",
      .data$sex == "Males" ~ "Men",
      TRUE ~ NA_character_
    ))

  df <- df %>%
    mutate(value = .data$value - 100)

  df %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$sex)) +
    geom_line() +
    geom_point(
      data = ~ filter(., .data$date == max(.data$date)),
      size = 3, stroke = 1.5, fill = "white", shape = 21
    ) +
    grattan_label_repel(
      data = ~ filter(., .data$date == max(.data$date)),
      aes(label = paste0(.data$sex, "\n", round(.data$value, 1), "%")),
      hjust = 0,
      nudge_x = 5,
      segment.size = 0,
      direction = "y"
    ) +
    grattan_colour_manual(2) +
    scale_y_continuous(
      n.breaks = 6,
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_date(
      expand = expansion(mult = c(0, 0.17)),
      breaks = seq.Date(
        from = as.Date("2020-03-14"), to = max(df$date) + 30,
        length.out = 5
      ),
      date_labels = "%b"
    ) +
    theme_grattan() +
    theme(axis.title = element_blank()) +
    labs(
      title = "Women lost jobs quicker, but rebounded more than men",
      subtitle = "Change in the number of payroll jobs since 14 March 2020",
      caption = "Source: ABS Weekly Payroll Jobs."
    )
}
