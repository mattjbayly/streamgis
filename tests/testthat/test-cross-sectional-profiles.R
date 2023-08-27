test_that("test cross sectional profiles", {

  # Import a simple stream center line
  # center_line <- st_read("./path/to/my/file.gpkg", layer = "layer name")
  # or use default provided for tutorial
  fname <- system.file("extdata", "center_line.gpkg", package="streamgis")
  center_line <- sf::st_read(fname)

  # plot(sf::st_geometry(center_line))

  # Sample points along line
  pol <- suppressWarnings({ points_on_line(center_line,
                                           point_spacing = 100) })

  # points <- pol


  # plot(sf::st_geometry(center_line[center_line$id == 3, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")

  # Do distances make sense
  diff <- pol$distance_m[6] - pol$distance_m[5]
  expect_true(round(diff, 0) == 100)

  # Do points start at origin
  expect_true(round(pol$distance_m[2], 0) == 100)


  # =============================================
  # =============================================
  # test  --- cross_section_lines
  # =============================================
  # =============================================

  csl <- cross_section_lines(center_line = center_line,
                      points = pol,
                      cross_profile_length = 250,
                      epsg = 26910)


  # plot(sf::st_geometry(center_line[center_line$id == 3, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")
  # plot(sf::st_geometry(csl[csl$l_id == 3, ]), add = TRUE, col = "blue")


  # Test that total profile legnth is reasonable
  total_length <- as.numeric(round(sum(sf::st_length(csl)), 0))
  expect_length <- nrow(pol) * 250

  diff <- abs(1 - (total_length / expect_length))
  # diff <- abs(1 - (expect_length / total_length))

  expect_true(diff < 0.1)




})
