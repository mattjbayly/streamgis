test_that("test strange fail", {

  # test-strange-fail-path.R
  # library(sf)
  fname <- system.file("extdata", "/strange/t1/strange01_lines.gpkg", package = "streamgis")
  s_lines <- sf::st_read(fname)

  fname <- system.file("extdata", "/strange/t1/strange01_points.gpkg", package = "streamgis")
  s_points <- sf::st_read(fname)

  # Fix geometry of s_lines
  s_lines <- sf::st_make_valid(s_lines)

  # This works
  path_result <- extract_stream_path(s_lines, s_points[1, ], s_points[2, ])
  expect_true(nrow(path_result) == 3)
  mset <- setdiff(path_result$rid,  c(3870, 3869, 3868))
  expect_true(length(mset) == 0)

  # This works - neighboring path
  path_result <- extract_stream_path(s_lines, s_points[3, ], s_points[4, ])
  expect_true(nrow(path_result) == 1)
  expect_true(path_result$rid == 3888)

  # But if we edit one of the points - slightly upsteam it becomes invalid
  fname <- system.file("extdata", "/strange/t1/strange01_points_fail.gpkg", package = "streamgis")
  s_points <- sf::st_read(fname)

  # Fix geometry of s_lines
  s_lines <- sf::st_make_valid(s_lines)

  # This works
  path_result <- extract_stream_path(s_lines, s_points[1, ], s_points[2, ])
  expect_true(nrow(path_result) == 3)
  mset <- setdiff(path_result$rid,  c(3870, 3869, 3868))


  # Try again with another single line segment
  # library(sf)
  fname <- system.file("extdata", "/strange/t2/failing_line.gpkg", package = "streamgis")
  s_lines <- sf::st_read(fname)

  fname <- system.file("extdata", "/strange/t2/passing_points.gpkg", package = "streamgis")
  s_points <- sf::st_read(fname)

  # This works - neighboring path
  path_result <- extract_stream_path(s_lines, s_points[1, ], s_points[2, ])
  expect_true(nrow(path_result) == 1)
  expect_true(path_result$rid == 3868)


  # This works - neighboring path
  path_result <- extract_stream_path(s_lines, s_points[3, ], s_points[4, ])
  expect_true(nrow(path_result) == 1)
  expect_true(path_result$rid == 3868)

})
