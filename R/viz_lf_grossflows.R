
viz_lf_grossflows <- function(data = load_data(),
                              arg1 = as.Date("2000-09-01"),
                              arg2 = NULL) {

  gf_tot <- data$lfs_m_grossflows
  min_date <- arg1

  gf_tot_ts <- gf_tot %>%
    tsbox::ts_ts()

  gf_tot_ts <- gf_tot_ts %>%
    ggseas::tsdf() %>%
    mutate(date = unique(gf_tot$date)) %>%
    select(-x)

  gf_tot_ts <- gf_tot_ts %>%
    filter(date >= min_date)

  gf_tot_ts <- gf_tot_ts %>%
    gather(key = series, value = perc,
           -date)


  gf_tot_ts %>%
    mutate(series = gsub("_", " to ", series)) %>%
    ggplot(aes(x = date, y = perc, col = series)) +
    ggseas::stat_seas(frequency = 12,
                      start = 2000.16666666667) +
    scale_y_continuous(labels = function(x)scales::percent(x, 0.1)) +
    # scale_x_continuous(breaks = seq(2000, 2020, 10)) +
    facet_wrap(~series,
               scales = "free") +
    theme_grattan() +
    suppressWarnings(grattan_colour_manual(n = 9)) +
    theme(strip.text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_blank() ) +
    labs(title = "Month-to-month transitions in the labour force",
         subtitle = "Percentage of people with a labour force status (eg. 'employed') in a given month by status the following month (eg. 'unemployed')",
         caption = "Source: ABS 6202.0 and Grattan analysis.")

}
