
viz_lf_grossflows <- function(data = load_data(),
                              min_date = as.Date("2000-09-01"),
                              arg2 = NULL) {
  gf_tot <- data$lfs_m_grossflows

  gf_tot_ts <- gf_tot %>%
    tsbox::ts_ts()

  gf_tot_ts <- gf_tot_ts %>%
    ggseas::tsdf() %>%
    mutate(date = unique(gf_tot$date)) %>%
    select(-x)

  gf_tot_ts <- gf_tot_ts %>%
    tidyr::pivot_longer(
      names_to = "series",
      values_to = "perc",
      cols = !one_of("date")
    )

  gf_tot_ts %>%
    mutate(series = gsub("_", " to ", series, fixed = TRUE)) %>%
    ggplot(aes(x = date, y = perc, col = series)) +
    ggseas::stat_seas(
      frequency = 12,
      start = 2000.16666666667
    ) +
    scale_y_continuous(labels = function(x) scales::percent(x, 0.1)) +
    facet_wrap(~series,
      scales = "free"
    ) +
    theme_grattan() +
    coord_cartesian(xlim = c(min_date, max(gf_tot_ts$date))) +
    suppressWarnings(grattan_colour_manual(n = 9)) +
    theme(
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_blank()
    ) +
    labs(
      title = "Month-to-month transitions in the labour force",
      subtitle = "Percentage of people with a labour force status (eg. 'employed') in a given month by status the following month (eg. 'unemployed')",
      caption = "Source: ABS 6202.0 and Grattan analysis."
    )
}
