
load_data <- function(named_urls) {
  data <- list()
  data <- purrr::map(named_urls, readr::read_csv, progress = FALSE)
  data <- purrr::map(data, tibble::as_tibble)
  data <- purrr::set_names(data, names(named_urls))
  data
}
