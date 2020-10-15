

viz_emphours_sex_recessions <- function(data = load_data(),
                                        measure = "employment",
                                        facet_on = "recession"
                                        ) {

  stopifnot(measure %in% c("employment", "hours", "hours_pop"))
  stopifnot(facet_on %in% c("recession", "sex"))

  lfs_raw <- data$lfs_m_1
  hours_raw <- data$lfs_m_19

  # Define recession peaks ----
  # Based on emp-pop ratio for all persons
  tps <- tibble::tribble(
             ~peak_date,    ~min_date, ~recession,
           "1981-09-01", "1983-04-01",      1981L,
           "1990-05-01", "1993-02-01",      1989L,
           "2008-08-01", "2009-10-01",      2008L
           ) %>%
    mutate(across(ends_with("date"), as.Date)) %>%
    bind_rows(tibble(peak_date = as.Date("2020-02-01"),
                     min_date = max(lfs_raw$date),
                     recession = 2020L))


  dates_between_tps <- tps %>%
    group_by(recession) %>%
    summarise(date = list(seq.Date(from = peak_date,
                                   to = min_date,
                                   by = "1 day"))) %>%
    unnest(date)

  # Define functions to get employment, hours, or hours per capita

  get_emp <- function(df = lfs_raw) {
    df %>%
      filter(series_id %in% c("A84423057T",
                              "A84423029J") &
               !is.na(value)) %>%
      separate_series() %>%
      mutate(series = "employed") %>%
      select(date, series, sex = series_2, value)
  }

  get_hours <- function(df = hours_raw) {
    df %>%
      filter(!is.na(value) &
               series_id %in% c("A84426280L",
                                "A84426274T")) %>%
      separate_series() %>%
      mutate(series = "hours") %>%
      select(date, series, sex = series_2, value)
  }

  get_pop <- function(df = lfs_raw) {
    df %>%
      filter(!is.na(value) &
               series_id %in% c("A84423105X",
                                "A84423077A")) %>%
      separate_series() %>%
      mutate(series = "pop") %>%
      select(date, series, sex = series_2, value)
  }

  get_hours_pop <- function() {
    hours <- get_hours()
    pop <- get_pop()
    df <- bind_rows(hours, pop)

    df %>%
      spread(key = series, value = value) %>%
      filter(!is.na(hours)) %>%
      mutate(value = hours / pop,
             series = "hours_pop") %>%
      select(date, series, sex, value)
}



  if (measure == "employment") {
    df <- get_emp()
  } else if (measure == "hours") {
    df <- get_hours()
  } else if (measure == "hours_pop") {
    df <- get_hours_pop()
  }


  # Get employment by sex during recessions
  recession_df <- df %>%
    left_join(dates_between_tps, by = "date") %>%
    filter(!is.na(recession))

  recession_df <- recession_df %>%
    mutate(sex = case_when(sex == "Females" ~ "Women",
                           sex == "Males" ~ "Men",
                           TRUE ~ NA_character_)) %>%
    group_by(recession, sex) %>%
    mutate(change_since_peak = (value / value[date == min(date)]) -1,
           months_since_peak = row_number() -1,
           recession_desc = case_when(recession == 1981 ~ "1980s recession",
                                      recession %in% c(1989, 1990) ~ "1990s recession",
                                      recession == 2008 ~ "GFC",
                                      recession == 2020 ~ "COVID"),
           recession_desc = factor(recession_desc,
                                   levels = c("1980s recession",
                                              "1990s recession",
                                              "GFC",
                                              "COVID"),
                                   ordered = T))


  # Line chart -- emp change by sex during recessions
  title_text <- case_when(measure == "employment" ~ "employment",
                          measure == "hours" ~ "total hours worked",
                          measure == "hours_pop" ~ "hours per person")

  facet_by_recession_plot <- function () {
    recession_df %>%
      ggplot(aes(x = months_since_peak, y= change_since_peak, col = sex)) +
      geom_hline(yintercept = 0) +
      geom_line() +
      grattan_label_repel(data = ~filter(., date == max(date)),
                    aes(label = paste0(if_else(recession == 1981,
                                               paste0(sex,
                                                      "\n"),
                                               ""),
                                       round(change_since_peak * 100, 1), "%")),
                    nudge_x = 2,
                    direction = "y",
                    segment.size = 0,
                    size = 14,
                    hjust = 0 ) +
      geom_point(data = ~filter(., date == max(date)),
                 stroke = 1.5,
                 shape = 21,
                 fill = "white") +
      facet_wrap(~recession_desc, nrow = 1) +
      scale_x_continuous(expand = expansion(add = c(1, 16)),
                         breaks = seq(0, 36, 12),
                         labels = function(x) x / 12) +
      scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
      grattan_colour_manual(2) +
      theme_grattan() +
      labs(x = "Years since downturn started",
           captions = "Notes: Start of downturn defined as the month in which the total employment-to-population ratio reached its peak; trough is the month in which the ratio reached its lowest point. COVID recession starts at February 2020. Sources: ABS Labour Force and Grattan calculations.",
           title = paste0(tools::toTitleCase(title_text), " during Australian downturns"),
           subtitle = paste0("Change in ", title_text, " between start of downturn and trough"))
  }

  facet_by_sex_plot <- function() {
    recession_df %>%
      mutate(recession_desc = gsub(" recession", "", recession_desc)) %>%
      ggplot(aes(x = months_since_peak, y= change_since_peak, col = recession_desc)) +
      geom_hline(yintercept = 0) +
      geom_line() +
      grattan_label_repel(data = ~filter(., date == max(date)),
                          aes(label = paste0(stringr::str_wrap(recession_desc, 10),
                                             "\n",
                                             round(change_since_peak * 100, 1), "%")),
                          nudge_x = 1,
                          direction = "y",
                          segment.size = 0,
                          size = 16,
                          hjust = 0 ) +
      geom_point(data = ~filter(., date == max(date)),
                 stroke = 1.5,
                 shape = 21,
                 fill = "white") +
      facet_wrap(~sex, nrow = 1) +
      scale_x_continuous(expand = expansion(add = c(1, 10)),
                         breaks = seq(0, 36, 12),
                         labels = function(x) x / 12) +
      scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
      grattan_colour_manual() +
      theme_grattan() +
      labs(x = "Years since downturn started",
           captions = "Notes: Start of downturn defined as the month in which the total employment-to-population ratio reached its peak; trough is the month in which the ratio reached its lowest point. COVID recession starts at February 2020. Sources: ABS Labour Force and Grattan calculations.",
           title = paste0(tools::toTitleCase(title_text), " during Australian downturns"),
           subtitle = paste0("Change in ", title_text, " between start of downturn and trough"))
  }



  if (facet_on == "recession") {
    facet_by_recession_plot()
  } else {
    facet_by_sex_plot()
  }

}
