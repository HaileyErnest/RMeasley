test_that("plot_cases() returns a ggplot object", {

  data <- load_data()

  expect_s3_class(plot_cases(data), "ggplot")

  expect_s3_class(plot_cases(data, 2014, 2024), "ggplot")

  expect_error(plot_cases(data, "2014", 2024), "Please enter a numeric value for the beginning year.")

  expect_error(plot_cases(data, 2014, "2024"), "Please enter a numeric value for the end year.")

  expect_error(plot_cases(data, 2014.5, 2024), "Please enter an integer between 2012 and 2025.")

  expect_error(plot_cases(data, 2010, 2024), "Please enter a year between 2012 and 2024.")

  expect_error(plot_cases(data, 2014, 2026), "Please enter a year between 2012 and 2024.")

  expect_warning(plot_cases(data, 2024, 2014), "Years not in order. Switching start and end year.")

  expect_s3_class(plot_cases(data, 2024, 2014), "ggplot")

  expect_error(plot_cases(data, 2014, 2014), "Please select an end year that is different from the start year.")

})
