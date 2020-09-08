
load_data <- function(named_urls) {
  data <- list()
  files <- file.path(tempdir(), paste0(names(named_urls), ".rds"))
  download.file(url = named_urls, destfile = files, quiet = TRUE, method = "libcurl", cacheOK = TRUE)
  # purrr::walk2(.x = named_urls, .y = files, .f = download.file)
  data <- purrr::map(files, fst::read_fst)
  data <- purrr::map(data, tibble::as_tibble)
  data <- purrr::set_names(data, names(named_urls))
  data <- purrr::map(data, ~dplyr::mutate_if(.tbl = .x, .predicate = is.factor, .funs = as.character))
  data
}

