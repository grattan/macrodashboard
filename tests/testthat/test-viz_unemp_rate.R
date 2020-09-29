

test_that("viz_unemp_rate() creates a graph", {
  df <- load_data(named_urls["lfs_m_1"])

  graph <- viz_unemp_rate(df)

  expect_s3_class(graph, "ggplot")
})
