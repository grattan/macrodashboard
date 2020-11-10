test_that("all viz_*() functions at least produce a plot", {
  viz_funcs <- ls("package:macrodashboard", pattern = "viz_")

  name_to_eval <- function(func_name_as_string) {
    eval(str2lang(paste0(func_name_as_string, "()")))
  }

  plots <- purrr::map(
    .x = viz_funcs,
    .f = name_to_eval
  )

  purrr::map(
    plots,
    ~ expect_is(.x, "ggplot")
  )
})
