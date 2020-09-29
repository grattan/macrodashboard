
viz_lf_grossflows <- function(data = load_data(),
                              arg1 = function() {}) {

  gf_tot <- data$lfs_m_grossflows

  gf_tot_ts <- gf_tot %>%
    tsbox::ts_ts()

  gf_tot_ts <- gf_tot_ts %>%
    ggseas::tsdf() %>%
    gather(key = series, value = perc, -x)


  gf_tot_ts %>%
    mutate(series = gsub("_", " to ", series)) %>%
    ggplot(aes(x = x, y = perc, col = series)) +
    ggseas::stat_seas(frequency = 12,
                      start = 2000.16666666667) +
    scale_y_continuous(labels = function(x)scales::percent(x, 0.1)) +
    scale_x_continuous(breaks = seq(2000, 2020, 10)) +
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
