test_that("test cross sectional profiles", {

  # Import a simple stream center line
  # center_line <- st_read("./path/to/my/file.gpkg", layer = "layer name")
  # or use default provided for tutorial
  fname <-
    system.file("extdata", "center_line.gpkg", package = "streamgis")
  # fname <- "./inst/extdata/center_line.gpkg"
  center_line <- sf::st_read(fname)

  # plot(sf::st_geometry(center_line))

  # Sample points along line
  pol <- suppressWarnings({
    points_on_line(center_line,
                   point_spacing = 100)
  })

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

  csl <- cross_section_lines(
    center_line = center_line,
    points = pol,
    cross_profile_length = 250,
    epsg = 26910
  )


  # plot(sf::st_geometry(center_line[center_line$id == 3, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")
  # plot(sf::st_geometry(csl[csl$l_id == 3, ]), add = TRUE, col = "blue")


  # Test that total profile legnth is reasonable
  total_length <- as.numeric(round(sum(sf::st_length(csl)), 0))
  expect_length <- nrow(pol) * 250

  diff <- abs(1 - (total_length / expect_length))
  # diff <- abs(1 - (expect_length / total_length))

  expect_true(diff < 0.1)



  # =============================================
  # =============================================
  # test  --- clean_reach_buffer
  # =============================================
  # =============================================

  buff <- clean_reach_buffer(
    center_line = center_line,
    buffer_width = 80,
    cross_section_lines = csl,
    us_distance_colname = NA,
    epsg = 26910
  )


  # plot(sf::st_geometry(center_line[center_line$id == 3, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")
  # plot(sf::st_geometry(csl[csl$l_id == 3, ]), add = TRUE, col = "blue")
  # plot(sf::st_geometry(buff[buff$l_id == 3, ]), add = TRUE, col = "grey")
  # plot(sf::st_geometry(csl[csl$l_id == 3, ]), add = TRUE, col = "blue")

  # Test geometry type
  test_geom <- as.character(unique(sf::st_geometry_type(buff)))
  expect_true(test_geom == "POLYGON")
  # Rows populated
  expect_true(nrow(buff) > 0)
  # Rows less than or equal to cross sections
  expect_true(nrow(buff) <= nrow(csl))

  # Fix order and test distance field
  fix_order <- csl[order(csl$l_id, csl$p_id),]
  fix_order$us_distance_m <- cumsum(fix_order$distance_m)
  # plot(fix_order['us_distance_m'])
  csl <- fix_order

  # Add variable length buffer
  center_line$my_buffer <- rnorm(nrow(center_line)) * 20
  center_line$my_buffer <- abs(center_line$my_buffer)

  buff <- clean_reach_buffer(
    center_line = center_line,
    buffer_width = "my_buffer",
    cross_section_lines = csl,
    us_distance_colname = 'us_distance_m',
    epsg = 26910
  )


  # plot(sf::st_geometry(center_line[center_line$id == 3, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")
  # plot(sf::st_geometry(csl[csl$l_id == 3, ]), add = TRUE, col = "blue")
  # plot(sf::st_geometry(buff[buff$l_id == 3, ]), add = TRUE, col = "grey")
  # plot(sf::st_geometry(csl[csl$l_id == 3, ]), add = TRUE, col = "blue")

  # Test geometry type
  test_geom <- as.character(unique(sf::st_geometry_type(buff)))
  expect_true(test_geom == "POLYGON")
  # Rows populated
  expect_true(nrow(buff) > 0)
  # Rows less than or equal to cross sections
  expect_true(nrow(buff) <= nrow(csl))




  # =============================================
  # =============================================
  # test  --- bcfwa_geometry
  # =============================================
  # =============================================

  # or continue with default provided for tutorial
  fname <- system.file("extdata", "bcfwa2.gpkg", package = "streamgis")
  # fname <- "./inst/extdata/bcfwa2.gpkg"

  bcfwa <- sf::st_read(fname)
  any(bcfwa$LINEAR_FEATURE_ID == 701790617)
  # Spius: 701790617; Upper Coldwater: 701794363
  # Lower Nicola: 701747059; Lower Coldwater: 701773410

  ds <- bcfwa_geometry(
    bcfwa = bcfwa,
    upstream = 701794363,
    downstream = 701773410,
    epsg = 26910
  )

  # Plot it out - 2D map
  if (FALSE) {
    plot(st_geometry(bcfwa))
    plot(
      st_geometry(bcfwa[bcfwa$LINEAR_FEATURE_ID == 701794363,]),
      add = TRUE,
      col = "red",
      lwd = 3
    )
    plot(
      st_geometry(bcfwa[bcfwa$LINEAR_FEATURE_ID == 701773410,]),
      add = TRUE,
      col = "blue",
      lwd = 6
    )
    plot(
      st_geometry(bcfwa[bcfwa$LINEAR_FEATURE_ID %in% ds$ds_path$LINEAR_FEATURE_ID,]),
      add = TRUE,
      col = "yellow",
      lwd = 2
    )
  }

  # Plot it out - profile
  if (FALSE) {
    df <- ds$coordinates
    plot(df$us_distance_m, df$Z, type = 'l')
  }


  # Check names
  names(ds)
  expect_true(all(names(ds) == c("ds_path", "coordinates")))
  expect_true(all(class(ds$ds_path) == c("sf", "data.frame")))
  # View coordinates for longitudial profile
  df <- ds$coordinates
  dist1 <- tail(df$us_distance_m)[1]
  dist2 <- sum(as.numeric(sf::st_length(ds$ds_path)))
  diff <- abs(1 - dist1 / dist2)
  expect_true(diff < 0.05)


})
