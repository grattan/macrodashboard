

test_that("viz_corelogic_shutdown() creates a graph", {
  testthat::skip_if_offline("github.com")

  corelogic_df <- load_data(named_urls["corelogic"])

  corelogic_graph <- viz_corelogic_shutdown(corelogic_df)

  expect_s3_class(corelogic_graph, "ggplot")
  expect_s3_class(corelogic_graph, "patchwork")
})
