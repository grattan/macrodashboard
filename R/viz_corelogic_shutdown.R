# URL: "https://raw.githubusercontent.com/MattCowgill/macro_dashboard_data/master/data/corelogic_daily.csv"

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom readr read_csv
#' @import grattantheme
#' @importFrom patchwork plot_annotation

viz_corelogic_shutdown <- function(df = dash_data$corelogic) {

  shutdown_date <- lubridate::ymd("2020-03-22")

  shutdown <- df %>%
    filter(date >= as.Date("2020-01-01")) %>%
    select(-agg) %>%
    gather(key = city, value = value,
           -date) %>%
    mutate(city = tools::toTitleCase(city)) %>%
    group_by(city) %>%
    mutate(value = 100 * (value / value[date == shutdown_date])) %>%
    arrange(city, date) %>%
    ungroup()


  labels <- shutdown %>%
    group_by(city) %>%
    filter(date == max(date)) %>%
    mutate(change = (value / 100) - 1,
           label = paste0(if_else(change > 0, "+", ""),
                          round(change * 100, 1),
                          "%"),
           label_y = value - 1.5)

  top_of_range <- max(c(102, max(shutdown$value)))

  make_shutdown_graph <- function(cities) {

    if ("Melbourne" %in% cities) {
      shutdown_cols <- c(grattantheme::grattan_red, grattantheme::grattan_lightorange)
    } else {
      shutdown_cols <- c(grattantheme::grattan_darkred, grattantheme::grattan_darkorange,
                         grattantheme::grattan_yellow)
    }

    shutdown_df <- shutdown %>%
      filter(city %in% cities)

    labels_df <- labels %>%
      filter(city %in% cities)

    shutdown_df %>%
      ggplot(aes(x = date, y = value, col = city)) +
      geom_hline(yintercept = 100) +
      geom_vline(xintercept = shutdown_date) +
      geom_line() +
      geom_point(data = labels_df,
                 size = 2, stroke = 1.25, fill = "white", shape = 21) +
      grattan_label(data = labels_df,
                    aes(label = label,
                        y = label_y),
                    nudge_x = -2,
                    hjust = 0.75,
                    size = 14) +
      grattan_label(data = ~filter(.,
                                   date == min(date)),
                    aes(label = city),
                    size = 18,
                    vjust = 1,
                    hjust = 0,
                    y = top_of_range - 0.1) +
      facet_wrap(~city) +
      theme_grattan() +
      scale_y_continuous(limits = c(min(shutdown$value) - 2.5,
                                    max(c(102, shutdown$value))),
                         labels = function(x) paste0(x - 100, "%")) +
      scale_x_date(date_labels = "%e\n%b",
                   breaks = c(min(shutdown_df$date),
                              shutdown_date,
                              max(shutdown_df$date)),
                   expand = expansion(add = c(0, 10))) +
      scale_colour_manual(values = shutdown_cols) +
      theme(strip.text = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = 14),
            panel.spacing = unit(1.5, "lines"))

  }

  melb_syd_graph <- make_shutdown_graph(c("Melbourne", "Sydney"))
  others_graph <- make_shutdown_graph(c("Perth", "Adelaide", "Brisbane"))

  shutdown_panel <- melb_syd_graph / others_graph

  shutdown_panel <- shutdown_panel +
    plot_annotation(title = "House price falls are accelerating in Melbourne, but remain modest elsewhere",
                    subtitle = "Cumulative change in Corelogic daily home value index since 22 March 2020",
                    caption = "Source: Corelogic") &
    theme_grattan() +
    theme(strip.text = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 14),
          panel.spacing = unit(1.25, "lines"))

  shutdown_panel

}
