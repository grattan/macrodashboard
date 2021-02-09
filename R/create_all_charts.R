#' Function to create all charts within package
#' @param data default is `load_data()`
#' @return A list of ggplot2 objects
#' @export

create_all_charts <- function(data = load_data()) {

  fns <- ls("package:macrodashboard", pattern = "viz_")

  charts <- purrr::map(.x = fns,
                       .f = ~ eval(parse(text = paste0(.x, "(data = data)"))))

  charts
}
