

test_that("viz_unemp_rate() creates a graph", {
  testthat::skip_if_offline("github.com")

  data <- load_data()

  graph <- viz_unemp_rate(data)

  expect_s3_class(graph, "ggplot")
})
