library(dplyr)
pkgload::load_all()

x <- load_data(named_urls) %>%
  .$corelogic %>%
  filter(date >= as.Date("2020-01-01")) %>%
  viz_corelogic_shutdown()

print(class(x))

charts_folder <- here::here("inst", "charts")

if (isFALSE(dir.exists(charts_folder))) {
  dir.create(charts_folder)
}

grattan_save(object = x,
               filename = file.path(charts_folder, "test.png"),
               type = "blog",
               save_pptx = F)

