test_that("load_data() loads data", {
  testthat::skip_if_offline("github.com")

  dash_data <- load_data()

  expect_type(dash_data, "list")

  # Check all elements of data list are tbl
  for (element in seq_len(length(dash_data))) {
    expect_s3_class(dash_data[[element]], "tbl")
  }
})
