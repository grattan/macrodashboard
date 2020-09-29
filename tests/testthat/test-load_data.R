test_that("load_data() loads data", {
  dash_data <- load_data()

  expect_type(dash_data, "list")
  expect_equal(length(dash_data), length(named_urls))
  expect_identical(names(dash_data), names(named_urls))

  # Check all elements of data list are tibbles
  for (element in seq_len(length(dash_data))) {
    expect_s3_class(dash_data[[element]], "tbl")
  }

})
