test_that("viz_corelogic_shutdown() creates a graph", {
  corelogic_url <- "https://raw.githubusercontent.com/MattCowgill/macro_dashboard_data/master/data/corelogic_daily.csv"

  corelogic_graph <- viz_corelogic_shutdown(corelogic_url)

  expect_s3_class(corelogic_graph, "ggplot")
  expect_s3_class(corelogic_graph, "patchwork")
})
