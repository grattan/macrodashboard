#' @import sf
#' @importFrom rlang .env .data

viz_payrolls_map <- function(data = load_data(),
                             arg1 = "Melbourne",
                             arg2 = NULL) {
  df <- data$payrolls_sa3_jobs

  city <- arg1

  df <- df %>%
    filter(.data$date == max(.data$date))

  df <- df %>%
    mutate(city = case_when(
      grepl("Perth", .data$sa4) ~ "Perth",
      grepl("Adelaide", .data$sa4) ~ "Adelaide",
      grepl("Darwin", .data$sa4) ~ "Darwin",
      grepl("Adelaide", .data$sa4) ~ "Adelaide",
      grepl("Hobart", .data$sa4) ~ "Hobart",
      grepl("Melbourne", .data$sa4) ~ "Melbourne",
      grepl("Australian Capital Territory", .data$sa4) ~ "Canberra",
      grepl("Sydney", .data$sa4) ~ "Sydney",
      grepl("Brisbane", .data$sa4) ~ "Brisbane",
      TRUE ~ NA_character_
    ))

  df <- df %>%
    filter(.data$city == .env$city)

  df <- absmapsdata::sa32016 %>%
    select(
      sa3 = .data$sa3_name_2016,
      sa4 = .data$sa4_name_2016,
      .data$geometry,
      .data$cent_long,
      .data$cent_lat
    ) %>%
    right_join(df, by = c("sa3", "sa4"))

  df <- df %>%
    mutate(value = .data$value - 100)

  worst_value <- df %>%
    filter(.data$value == min(.data$value)) %>%
    pull(value)

  worst_area <- df %>%
    filter(.data$value == worst_value) %>%
    pull(.data$sa3)

  dir_of_change <- if_else(worst_value <= 100, "-", "+")

  df %>%
    ggplot() +
    geom_sf(
      aes(
        geometry = geometry,
        fill = value
      ),
      colour = "white",
      size = 0.1
    ) +
    geom_text(aes(
      x = .data$cent_long,
      y = .data$cent_lat,
      label = round(.data$value, 1)
    ),
    size = 10 / .pt
    ) +
    grattan_fill_manual(
      discrete = F, palette = "full_f",
      labels = function(x) paste0(x, "%")
    ) +
    theme_grattan(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "right",
      plot.title.position = "plot",
      legend.direction = "vertical"
    ) +
    coord_sf() +
    labs(
      title = paste(worst_area,
        "is the hardest-hit area in",
        city,
        sep = " "
      ),
      subtitle = paste0(
        "Change in the number of payroll jobs between",
        " 14 March 2020 and ",
        format(max(df$date), "%d %b %Y"),
        ", per cent"
      )
    )
}
