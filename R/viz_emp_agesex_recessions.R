#' @importFrom ggtext geom_richtext

viz_emp_agesex_recessions <- function(data = load_data(),
                                      arg1 = NULL,
                                      arg2 = NULL) {
  df <- data$lfs_m_22

  df <- df %>%
    filter(.data$series_id %in% c(
      "A84424191X",
      "A84424163R",
      "A85255850V",
      "A85255790C",
      "A85255778L",
      "A85255898F",
      "A85255562A",
      "A85255502X",
      "A85255490A",
      "A85255610J"
    )) %>%
    separate_series(column_names = c("series_desc", "sex", "age"))

  df <- df %>%
    mutate(age_group = case_when(
      .data$age == "15-24 years" ~ "15-24",
      .data$age %in% c(
        "25-34 years",
        "35-44 years",
        "45-54 years"
      ) ~ "25-54",
      .data$age == "55 years and over" ~ "55+"
    )) %>%
    group_by(.data$date, .data$sex, .data$age_group) %>%
    summarise(emp = sum(.data$value))

  # Define recessions ----
  # Based on emp-pop ratio for all persons
  tps <- dplyr::tribble(
    ~peak_date, ~min_date, ~recession,
    "1981-09-01", "1983-04-01", "1980s",
    "1990-05-01", "1993-02-01", "1990s",
    "2008-08-01", "2009-10-01", "GFC"
  ) %>%
    mutate(across(ends_with("date"), as.Date)) %>%
    bind_rows(dplyr::tibble(
      peak_date = as.Date("2020-02-01"),
      min_date = max(df$date),
      recession = "COVID"
    )) %>%
    mutate(min_date = .data$peak_date + lubridate::years(3))


  dates_between_tps <- tps %>%
    group_by(.data$recession) %>%
    summarise(date = list(seq.Date(
      from = .data$peak_date,
      to = .data$min_date,
      by = "1 day"
    ))) %>%
    unnest(.data$date)

  df <- df %>%
    left_join(dates_between_tps, by = "date") %>%
    filter(!is.na(.data$recession))

  df <- df %>%
    arrange(.data$date) %>%
    group_by(.data$sex, .data$age_group, .data$recession) %>%
    mutate(
      months_since_start = row_number() - 1,
      change_since_start = (.data$emp / .data$emp[.data$date == min(.data$date)]) - 1
    )

  df <- df %>%
    ungroup() %>%
    mutate(sex = case_when(
      .data$sex == "Females" ~ "Women",
      .data$sex == "Males" ~ "Men",
      TRUE ~ NA_character_
    ))

  df$recession <- factor(df$recession, levels = unique(df$recession))

  make_plot <- function(.sex) {
    stopifnot(.sex %in% unique(df$sex))
    plot <- df %>%
      filter(.data$sex == .sex) %>%
      ggplot(aes(x = .data$months_since_start, y = .data$change_since_start, col = .data$recession)) +
      geom_hline(yintercept = 0) +
      geom_line() +
      facet_grid(~ .data$age_group) +
      theme_grattan() +
      grattan_colour_manual() +
      scale_y_continuous(
        labels = function(x) paste0(x * 100, "%"),
        breaks = seq(-0.2, 0.2, 0.1),
        limits = c(-0.2, 0.2)
      ) +
      labs(subtitle = .sex) +
      theme(
        plot.subtitle = element_text(
          size = 18, colour = "black",
          hjust = 0.54,
          vjust = 0,
          margin = margin(0.2, 0, 0, 0, "lines")
        ),
        strip.text = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)
      )

    if (.sex == "Women") {
      plot +
        ggtext::geom_richtext(
          data = dplyr::tibble(
            age_group = "15-24",
            x = c(0, 12),
            y = .05,
            label = c(
              "<span style='color:#FFC35A'>1980s</span><br><span style='color:#F68B33'>1990s</span>",
              "<span style='color:#D4582A'>GFC</span><br><span style='color:#A02226'>COVID</span>"
            )
          ),
          aes(x = .data$x, y = .data$y, label = .data$label),
          label.size = 0,
          label.padding = unit(0, "lines"),
          hjust = 0,
          vjust = 0,
          size = 18 / .pt,
          inherit.aes = F
        )
    } else {
      plot +
        labs(x = "Months since downturn started")
    }
  }


  make_plot("Women") +
    make_plot("Men") +
    patchwork::plot_layout(ncol = 1) +
    plot_annotation(
      title = "Youth employment fell sharply with the COVID shock",
      subtitle = "Change in employment from the start of a downturn",
      caption = "Sources: ABS Labour Force and Grattan calculations.",
      theme = theme_grattan()
    )
}
