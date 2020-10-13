#' @import dplyr
#' @import ggplot2
#' @import grattantheme
#' @importFrom rlang .data

viz_hours_pop <- function(data = load_data(),
                          arg1 = NULL,
                          arg2 = NULL) {
  civ_pop <- data$lfs_m_1 %>%
    filter(series == "Civilian population aged 15 years and over ;  Persons ;") %>%
    select(date, civ_pop = value)

  hours <- data$lfs_m_19 %>%
    filter(.data$series == "Monthly hours worked in all jobs ;  Persons ;",
           .data$series_type == "Seasonally Adjusted") %>%
    select(hours = .data$value, .data$date)

  hours <- left_join(hours, civ_pop, by = c("date"))

  hours <- hours %>%
    mutate(hours_per_capita = .data$hours / .data$civ_pop)

  hours_to_highlight <- hours %>%
    filter(date %in% c(as.Date("2020-03-01"),
                       as.Date("2020-05-01"),
                       max(.data$date))) %>%
    mutate(label = paste0(round(.data$hours_per_capita, 1), " in\n",
                          lubridate::month(.data$date, abbr = FALSE, label = TRUE)))


  hours %>%
    ggplot(aes(x = .data$date, y = .data$hours_per_capita)) +
    geom_line(size = 0.8) +
    geom_point(data = hours_to_highlight,
               shape = 21, fill = "white", stroke = 1.2) +
    grattan_label(data = hours_to_highlight,
                  aes(label = .data$label),
                  hjust = 0,
                  size = 16,
                  lineheight = 0.8,
                  nudge_x = 180) +
    theme_grattan() +
    scale_y_continuous(limits = c(76, 92),
                       breaks = seq(76, 92, 4)) +
    scale_x_date(expand = expansion(mult = c(0, 0.10))) +
    theme(axis.title = element_blank() ) +
    labs(title = "The number of hours worked per person fell dramatically after the COVID shock",
         subtitle = "Total hours worked per head of population per month",
         caption = "Source: ABS 6202.0 and Grattan analysis.")

}
