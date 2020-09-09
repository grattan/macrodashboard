library(dplyr)
library(ggplot2)
devtools::load_all()

load_data(named_urls) %>%
  .$corelogic %>%
  filter(date >= as.Date("2020-01-01")) %>%
  viz_corelogic_shutdown() %>%
  grattan_save(object = ., filename = "inst/charts/test.png",
               type = "blog", save_pptx = T)
