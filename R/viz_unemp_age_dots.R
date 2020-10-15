#' @import dplyr
#' @import readabs
#' @import grattantheme
#' @importFrom stringr str_replace_all str_squish

viz_unemp_age_dots <- function(data = load_data(),
                               month1 = as.Date("2020-03-01"),
                               arg2 = NULL) {

  month1 <- lubridate::dmy(paste("01",
    lubridate::month(month1),
    lubridate::year(month1),
    sep = "-"
  ))

  lfs_22 <- data$lfs_m_22

  # Filter
  df <- lfs_22 %>%
    filter(grepl("Unemployment rate", series, fixed = T) |
      grepl("Underemployment rate", series, fixed = T)) %>%
    filter(
      grepl("Persons", series, fixed = T) &
        grepl("years", series, fixed = T) &
        !grepl("19", series, fixed = T) &
        !grepl("64", series, fixed = T) &
        date %in% c(max(date), month1),
      series_type == "Seasonally Adjusted"
    )

  # Separate
  df <- df %>%
    readabs::separate_series(column_names = c(
      "series_desc",
      "sex",
      "age"
    ))

  # Tidy up

  df <- df %>%
    mutate(
      age = str_replace_all(age, "[:alpha:]", ""),
      age = str_squish(age),
      age = if_else(age == "55", "55+", age),
      series_desc = if_else(grepl("Underemployment", series_desc),
        "Underemployment rate",
        series_desc
      )
    )

  df <- df %>%
    mutate(date_type = if_else(date == month1, "min_date", "max_date")) %>%
    select(date, series_desc, age, value, date_type)

  df$age <- factor(df$age)
  df$age <- forcats::fct_rev(df$age)

  df %>%
    ggplot(aes(
      x = age,
      y = value,
      col = format(date, "%B %Y")
    )) +
    geom_segment(
      data = ~ select(., -date) %>%
        tidyr::pivot_wider(
          names_from = date_type,
          values_from = value
        ),
      aes(
        x = age, xend = age, y = min_date,
        yend = if_else(min_date < max_date,
          max_date - 0.5,
          max_date + 0.5
        )
      ),
      col = grattantheme::grattan_grey3,
      size = 1,
      arrow = arrow(
        angle = 20, length = unit(0.5, "lines"),
        type = "closed"
      ),
      inherit.aes = FALSE
    ) +
    grattantheme::grattan_label_repel(
      data = ~ filter(
        ., age == "15-24",
        series_desc == "Unemployment rate"
      ),
      aes(label = format(date, "%b '%y")),
      nudge_x = 0.25,
      direction = "x",
      segment.size = 0
    ) +
    geom_point(size = 4) +
    grattan_colour_manual(2) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    facet_wrap(~series_desc) +
    coord_flip() +
    theme_grattan() +
    labs(
      y = "Percentage of the labour force",
      title = "Unemployment and underemployment by age",
      subtitle = paste0(
        "Unemployment and underemployment rates by age, ",
        format(month1, "%B %Y"), " and ",
        format(max(df$date), "%B %Y"),
        " (per cent of labour force)"
      ),
      caption = "Note: monthly seasonally adjusted data. Source: ABS Labour Force."
    )
}
