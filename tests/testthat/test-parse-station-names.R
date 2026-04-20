test_that("parse_station_names reproduces expected output from test CSV", {

  # Load the expected results
  fname <- system.file("extdata", "test_parsed_names.csv", package = "streamgis")
  expected <- read.csv(fname, stringsAsFactors = FALSE)

  # Run column 1 (raw_name) through parse_station_names
  result <- parse_station_names(expected$raw_name)

  # Check dimensions match
  expect_equal(nrow(result), nrow(expected))
  expect_equal(ncol(result), ncol(expected))

  # Check column names match
  expect_equal(colnames(result), colnames(expected))

  # Normalize NAs for comparison: CSV reads NA as character NA sometimes
  # Convert any literal "NA" strings to real NA in expected
  for (col in colnames(expected)) {
    expected[[col]][expected[[col]] == "NA"] <- NA_character_
  }

  # Compare each column
  for (col in colnames(expected)) {
    # Replace NA with a sentinel for comparison so identical() works cleanly
    res_col <- result[[col]]
    exp_col <- expected[[col]]
    expect_equal(res_col, exp_col,
                 info = paste("Column mismatch:", col))
  }

  # Also verify full data.frame equality
  expect_equal(result, expected)

})
