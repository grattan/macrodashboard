

test_that("viz_corelogic_shutdown() creates a graph", {
  testthat::skip_if_offline("github.com")

  data <- load_data()

  corelogic_graph <- viz_corelogic_shutdown_panel(data)

  expect_s3_class(corelogic_graph, "ggplot")
  expect_s3_class(corelogic_graph, "patchwork")
})
