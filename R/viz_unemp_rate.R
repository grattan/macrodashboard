
viz_unemp_rate <- function(df = dash_data$lfs_m_1,
                           arg1 = c(max(df$date) - lubridate::years(40),
                                     max(df$date))
                           ) {

  dates <- arg1
  min_date <- dates[1]
  max_date <- dates[2]

  # Load historical (pre-1978) unemp data
  lr_unemp <- auseconhist::lr_lfs %>%
    select(date, unemp_rate) %>%
    filter(date < min(df$date)) %>%
    mutate(unemp_rate = unemp_rate * 100)

  # Load post-1978 unemp data and combine with older data
  unemp <- df %>%
    filter(series_id == "A84423050A") %>%
    select(date, unemp_rate = value) %>%
    bind_rows(lr_unemp)

  # Filter data to only show that between dates of interest
  unemp <- unemp %>%
    filter(date >= min_date & date <= max_date)

  # Create dynamic title
  latest_date <- max(unemp$date)

  latest_ur <- unemp %>%
    filter(date == latest_date) %>%
    pull(unemp_rate)

  prev_high <- unemp %>%
    filter(unemp_rate >= latest_ur,
           date != latest_date) %>%
    arrange(desc(date)) %>%
    filter(row_number() == 1)

  days_since_prev_high <- latest_date - prev_high$date[1]

  monthly_change <- unemp %>%
    arrange(desc(date)) %>%
    filter(row_number() %in% c(1, 2)) %>%
    mutate(change = unemp_rate - lead(unemp_rate, 1)) %>%
    filter(!is.na(change)) %>%
    pull(change)

  sign_of_monthly_change <- ifelse(sign(monthly_change) == 1, "rose", "fell")

  latest_month <- lubridate::month(latest_date, label = TRUE, abbr = FALSE)

  graph_title <- ifelse(days_since_prev_high > 365 | is.na(days_since_prev_high),
                        paste0("The unemployment rate is the highest it's been since ",
                               format(prev_high$date[1], "%B %Y")),
                        paste0("The unemployment rate ", sign_of_monthly_change,
                               " by ", round(monthly_change, 2),
                               " percentage points in ", latest_month))

  graph_title <- case_when(
    days_since_prev_high > 365 ~ paste0("The unemployment rate is the highest it's been since ",
                                        format(prev_high$date[1], "%B %Y")),
    days_since_prev_high < 365 ~ paste0("The unemployment rate ", sign_of_monthly_change,
                                        " by ", round(monthly_change, 2),
                                        " percentage points in ", latest_month),
    is.na(days_since_prev_high) ~ paste0("The unemployment rate is ",
                                         round(latest_ur, 1), "%")

  )

  # Construct notes and source

  graph_source <- case_when(
    min_date >= as.Date("1978-02-01") ~ "ABS Labour Force (cat. no. 6202.0).",
    min_date >= as.Date("1966-08-01") ~ "ABS Labour Force (cat. no. 6202.0) and ABS Labour Force Historical Timeseries (cat. no. 6204.0.55.001).",
    TRUE ~ "ABS Labour Force (cat. no. 6202.0),  ABS Labour Force Historical Timeseries (cat. no. 6204.0.55.001), and Butlin (1977).")

  graph_notes <- case_when(
    min_date >= as.Date("1978-02-01") ~ "Monthly seasonally adjusted data.",
    min_date >= as.Date("1966-08-01") ~ "Monthly seasonally adjusted data from 1978, quarterly unadjusted data from 1966 to 1978.",
    TRUE ~ "Monthly seasonally adjusted data from 1978, quarterly unadjusted data from 1966 to 1978, annual data prior to 1966."
  )

  source_with_notes <- paste("Notes:", graph_notes, "Source:", graph_source,
                             sep = " ")

  unemp %>%
    ggplot(aes(x = date, y = unemp_rate)) +
    geom_line() +
    theme_grattan() +
    grattan_y_continuous(limits = c(0, ceiling(max(unemp$unemp_rate)))) +
    labs(title = graph_title,
         subtitle = paste0("Unemployment rate, ", format(min_date, "%B %Y"),
                           " to ", format(max_date, "%B %Y")),
         caption = source_with_notes ) +
    theme(axis.title.x = element_blank() )

}

