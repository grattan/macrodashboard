
load_data <- function(urls = named_urls) {
  data <- list()
  files <- file.path(tempdir(), paste0(names(urls), ".rds"))
  download.file(url = urls, destfile = files, quiet = TRUE, method = "libcurl", cacheOK = TRUE)
  data <- purrr::map(files, fst::read_fst)
  data <- purrr::map(data, tibble::as_tibble)
  data <- purrr::set_names(data, names(urls))
  data <- purrr::map(data, ~dplyr::mutate_if(.tbl = .x, .predicate = is.factor, .funs = as.character))
  data
}

