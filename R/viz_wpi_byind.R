viz_wpi_byind <- function(data = load_data(),
                             arg1 = as.Date("1997-09-01"),
                             arg2 = c("Mining",
                                      "Manufacturing",
                                      "Electricity, gas, water and waste services",
                                      "Construction",
                                      "Wholesale trade",
                                      "Retail trade",
                                      "Accommodation and food services",
                                      "Transport, postal and warehousing",
                                      "Information media and telecommunications",
                                      "Financial and insurance services",
                                      "Rental, hiring and real estate services",
                                      "Professional, scientific and technical services",
                                      "Administrative and support services",
                                      "Public administration and safety",
                                      "Education and training",
                                      "Health care and social assistance",
                                      "Arts and recreation services",
                                      "Other services",
                                      "All industries")) {

  df <- data$wpi_5b

  user_min_date <- arg1
  user_industries <- arg2


  df <- df %>%
    dplyr::filter(grepl("Percentage Change from Corresponding Quarter of Previous Year",
                        .data$series) &
                    grepl("Private and Public", .data$series) &
                    !is.na(value))

  df <- df %>%
    mutate(industry = gsub("Percentage Change from Corresponding Quarter of Previous Year ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  ",
                         "",
                         .data$series),
           industry = gsub(" ;", "", .data$industry))


  df <- df %>%
    filter(.data$date >= user_min_date)

  max_date <- max(df$date)

  df <- df %>%
    filter(.data$industry %in% user_industries)

  latest <- df %>%
    filter(date == max(.data$date))

  ave_latest <- df %>%
    filter(.data$date == max(.data$date)) %>%
    pull(.data$value) %>%
    mean()

  title <- case_when(
    ave_latest < 3 ~ "Wages growth has been sluggish",
    ave_latest < 4 ~ "Wages growth has been around its long-run average",
    ave_latest >= 4 ~ "Wages growth has been robust",
    TRUE ~ "Wages growth has certainly been something"
  )

  df <- df %>%
    mutate(short_ind = case_when(industry == "Other services" ~ industry,
                                 grepl("services", industry) ~ gsub(" services", "", industry),
                                 TRUE ~ industry))

  df <- df %>%
    mutate(short_ind = gsub("and", "&", short_ind),
           short_ind = gsub("telecommunications", "comms.", short_ind))

  df <- df %>%
    group_by(short_ind) %>%
    summarise(latest = .data$value[.data$date == max(.data$date)],
              ave = mean(.data$value)) %>%
    gather(key = "series", value = "value",
           -short_ind)

  df <- df %>%
    filter(series == "latest")  %>%
    mutate(rank = rank(value, ties.method = "first")) %>%
    select(rank, short_ind) %>%
    right_join(df, by = "short_ind")

  df %>%
    ggplot(aes(x = reorder(short_ind, rank),
               y = value,
               col = series)) +
    geom_point(size = 3.5) +
    grattan_label_repel(data = ~filter(., rank == max(rank)) %>%
                    mutate(label = ifelse(series == "latest",
                                          paste0("Year to ", format(max_date, "%b %Y")),
                                          paste0("Ave. since ", format(user_min_date, "%b %Y")))),
                  aes(label = stringr::str_wrap(label, 10)),
                  direction = "x",
                  segment.size = 0,
                  size = 16,
                  nudge_x = 1.5) +
    scale_x_discrete(expand = expansion(add = c(0.5, 2.5))) +
    grattan_y_continuous(limits = c(
      min(0, min(df$value)),
      max(4, max(df$value))
    ),
    labels = function(x) paste0(x, "%")) +
    coord_flip() +
    grattan_colour_manual(2) +
    theme_grattan(flipped = F) +
    theme(axis.text.y = element_text(size = 14),
          axis.title.x = element_blank()) +
    labs(title = title,
         subtitle = "Annual growth in the Wage Price Index by industry, per cent",
         caption = "Note: Not seasonally adjusted. Source: ABS Wage Price Index.")
}
