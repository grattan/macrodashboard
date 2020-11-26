viz_wpi_bysector <- function(data = load_data(),
                             arg1 = c(as.Date("1997-09-01"), Sys.Date()),
                             arg2 = c(
                               "Public",
                               "Private",
                               "Total"
                             )) {
  df <- data$wpi_1

  user_date_range <- arg1
  user_sector <- arg2

  df <- df %>%
    dplyr::filter(
      .data$series_type == "Seasonally Adjusted",
      grepl(
        "Percentage Change From Corresponding Quarter of Previous Year",
        .data$series
      ),
      !is.na(value)
    )

  df <- df %>%
    filter(.data$date >= user_date_range[1] & .data$date <= user_date_range[2])

  df <- df %>%
    mutate(sector = case_when(
      .data$series_id == "A83895309L" ~ "Private",
      .data$series_id == "A83895396W" ~ "Total",
      .data$series_id == "A83895333L" ~ "Public",
      TRUE ~ NA_character_
    ))

  df <- df %>%
    filter(.data$sector %in% user_sector)

  cols <- c(
    "Private" = grattantheme::grattan_red,
    "Public" = grattantheme::grattan_yellow,
    "Total" = grattantheme::grattan_lightorange
  )

  latest <- df %>%
    filter(date == max(.data$date))

  title <- case_when(
    latest$value[latest$sector == "Total"] < 3 ~ "Wages growth has been sluggish",
    latest$value[latest$sector == "Total"] < 4 ~ "Wages growth has been around its long-run average",
    latest$value[latest$sector == "Total"] >= 4 ~ "Wages growth has been robust"
  )


  df %>%
    ggplot(aes(x = .data$date, y = .data$value, col = .data$sector)) +
    geom_line() +
    geom_point(
      data = latest,
      size = 2.5, stroke = 1.5, fill = "white", shape = 21
    ) +
    grattan_label_repel(
      data = latest,
      aes(label = stringr::str_wrap(.data$sector, 7)),
      hjust = 0,
      nudge_x = 45,
      direction = "y",
      segment.size = 0,
      size = 16
    ) +
    scale_x_date(expand = expansion(mult = c(0, 0.12))) +
    grattan_y_continuous(
      limits = c(
        min(0, min(df$value)),
        max(5, max(df$value))
      ),
      labels = function(x) paste0(x, "%")
    ) +
    theme_grattan() +
    scale_colour_manual(values = cols) +
    theme(axis.title = element_blank()) +
    labs(
      title = title,
      subtitle = "Annual growth in the Wage Price Index, per cent",
      caption = "Source: ABS Wage Price Index."
    )
}
