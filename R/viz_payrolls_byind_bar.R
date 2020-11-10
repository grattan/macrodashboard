
viz_payrolls_byind_bar <- function(data = load_data(),
                                   arg1 = "Construction",
                                   arg2 = NULL) {
  df <- data$payrolls_industry_jobs

  highlight_industry <- arg1

  chart_data <- df %>%
    filter(
      .data$industry != "All industries",
      .data$state == "Australia",
      .data$sex == "Persons",
      .data$age == "All ages",
      .data$date == max(date)
    ) %>%
    mutate(industry = gsub("[[:alpha:]]-", "", .data$industry))

  highlight_level <- chart_data %>%
    filter(.data$industry == highlight_industry) %>%
    pull(.data$value) %>%
    round(digits = 0)

  title <- case_when(
    highlight_level < 100 ~
    paste0(
      "About ", 100 - highlight_level, " per cent of ",
      tolower(highlight_industry), " jobs have been lost since March 2020"
    ),
    highlight_level == 100 ~
    paste0(
      "The number of jobs in ", tolower(highlight_industry),
      " is about the same as it was in March 2020"
    ),
    TRUE ~
    paste0(
      "The number of jobs in ", tolower(highlight_industry),
      " has grown by about ", abs(100 - highlight_level),
      " per cent since March"
    )
  )

  chart_data %>%
    mutate(highlight = ifelse(.data$industry == highlight_industry, TRUE, FALSE)) %>%
    ggplot(aes(
      x = reorder(.data$industry, .data$value),
      y = .data$value - 100,
      fill = .data$highlight
    )) +
    geom_col() +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_grattan(flipped = T) +
    theme(axis.ticks = element_blank()) +
    grattan_fill_manual(2) +
    theme(axis.title = element_blank()) +
    labs(
      title = title,
      subtitle = paste0(
        "Change in number of payroll jobs between 14 March 2020 and ",
        format(max(chart_data$date), "%d %B %Y"), ", per cent"
      ),
      caption = "Source: ABS Weekly Payroll Jobs and Wages in Australia."
    )
}
