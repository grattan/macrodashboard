
load_data <- function(named_urls) {
  data <- list()
  files <- file.path(tempdir(), paste0(names(named_urls), ".rds"))
  purrr::walk2(.x = named_urls, .y = files, .f = download.file)
  data <- purrr::map(files, readRDS)
  data <- purrr::map(data, tibble::as_tibble)
  data <- purrr::set_names(data, names(named_urls))
  data
}
