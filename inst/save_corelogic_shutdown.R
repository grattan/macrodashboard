library(dplyr)

macrodashboard::load_data(named_urls) %>%
  .$corelogic %>%
  filter(date >= as.Date("2020-01-01")) %>%
  macrodashboard::viz_corelogic_shutdown() %>%
  grattan_save(object = .,
               filename = here::here("inst", "charts", "test.png"),
               type = "blog",
               save_pptx = T)
