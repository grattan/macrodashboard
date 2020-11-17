
load_data <- function() {
  temp_file <- tempfile(fileext = ".rds")
  download.file(
    url = "https://github.com/MattCowgill/macro_dashboard_data/blob/master/data/all_data.rds?raw=TRUE",
    destfile = temp_file,
    quiet = TRUE,
    cacheOK = FALSE
  )
  data <- readRDS(temp_file)
  data <- purrr::map(data, dplyr::as_tibble)
  data <- purrr::map(data, ~ dplyr::mutate_if(.tbl = .x, .predicate = is.factor, .funs = as.character))
  data
}
