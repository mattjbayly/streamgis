# Tests for split_at_confluences using the split_lines test dataset
# This dataset contains complex line geometries including self-intersecting lines
# and near-return patterns that test edge cases in confluence detection.

test_that("split_at_confluences finds all expected confluence points", {
  # Load test data
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  expected_path <- system.file("extdata", "split_lines", "point_intersections_v2.gpkg",
                               package = "streamgis")

  skip_if(lines_path == "", "Test data not available")
  skip_if(expected_path == "", "Expected points data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)
  expected_pts <- sf::st_read(expected_path, quiet = TRUE)

  # Run the function with tolerance=1 to handle floating-point precision issues
  result <- split_at_confluences(lines, tolerance = 1.0)

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("lines", "confluences"))
  expect_s3_class(result$lines, "sf")
  expect_s3_class(result$confluences, "sf")

  # Get unique expected points (some duplicates within 1m)
  expected_coords <- sf::st_coordinates(expected_pts)[, 1:2, drop = FALSE]
  unique_expected <- unique(round(expected_coords, 1))

  # Get found confluence coordinates
  found_coords <- sf::st_coordinates(result$confluences)[, 1:2, drop = FALSE]

  # All unique expected points should be found (within 2m tolerance)
  matched <- 0
  for (i in seq_len(nrow(unique_expected))) {
    exp_pt <- unique_expected[i, ]
    for (j in seq_len(nrow(found_coords))) {
      d <- sqrt(sum((found_coords[j, ] - exp_pt)^2))
      if (d < 2) {
        matched <- matched + 1
        break
      }
    }
  }

  expect_equal(matched, nrow(unique_expected),
               info = paste("Expected to match", nrow(unique_expected),
                            "points but only matched", matched))
})


test_that("split_at_confluences finds valid confluences including endpoint junctions", {
  # Load test data
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  expected_path <- system.file("extdata", "split_lines", "point_intersections_v2.gpkg",
                               package = "streamgis")

  skip_if(lines_path == "", "Test data not available")
  skip_if(expected_path == "", "Expected points data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)
  expected_pts <- sf::st_read(expected_path, quiet = TRUE)

  result <- split_at_confluences(lines, tolerance = 1.0)

  # Get unique expected and actual coordinates
  expected_coords <- sf::st_coordinates(expected_pts)[, 1:2, drop = FALSE]
  unique_expected <- unique(round(expected_coords, 1))

  found_coords <- sf::st_coordinates(result$confluences)[, 1:2, drop = FALSE]
  unique_found <- unique(round(found_coords, 1))

  # Should find at least as many confluences as expected (may find more endpoint-to-endpoint junctions)
  expect_gte(nrow(unique_found), nrow(unique_expected))

  # All found confluences should have degree >= 2 (valid confluences where lines meet)
  expect_true(all(result$confluences$degree >= 2),
              info = "All confluences should have degree >= 2")

  # Each found confluence should be where lines actually intersect
  lines_cast <- sf::st_cast(lines, "LINESTRING")
  for (i in seq_len(nrow(result$confluences))) {
    conf_pt <- result$confluences[i, ]
    buff <- sf::st_buffer(sf::st_geometry(conf_pt), dist = 1.5)
    touching_lines <- sf::st_intersects(buff, lines_cast)[[1]]
    expect_true(
      length(touching_lines) >= 1,
      info = paste("Confluence", i, "at (", round(found_coords[i, 1], 1), ",",
                   round(found_coords[i, 2], 1), ") should touch at least 1 line")
    )
  }
})


test_that("split_at_confluences handles self-intersecting lines", {
  # Load test data - line_segments.gpkg contains a self-intersecting line
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  skip_if(lines_path == "", "Test data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)
  lines_cast <- sf::st_cast(lines, "LINESTRING")

  # Find the self-intersecting line
  has_self_int <- !sapply(seq_len(nrow(lines_cast)), function(i) {
    sf::st_is_simple(sf::st_geometry(lines_cast)[i])
  })

  expect_true(any(has_self_int),
              info = "Test data should contain at least one self-intersecting line")

  # Run the function
  result <- split_at_confluences(lines, tolerance = 1.0)

  # Self-intersection point should be detected
  # Line 12 self-intersects at approximately (1738252, 581724.7)
  found_coords <- sf::st_coordinates(result$confluences)
  self_int_pt <- c(1738252, 581724.7)

  found_self_int <- FALSE
  for (i in seq_len(nrow(found_coords))) {
    d <- sqrt(sum((found_coords[i, 1:2] - self_int_pt)^2))
    if (d < 2) {
      found_self_int <- TRUE
      break
    }
  }

  expect_true(found_self_int,
              info = "Self-intersection point at (1738252, 581724.7) should be detected")
})


test_that("split_at_confluences handles near-return patterns", {
  # Load test data - line_segments.gpkg contains a line that returns near its start
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  skip_if(lines_path == "", "Test data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)

  # Run the function
  result <- split_at_confluences(lines, tolerance = 1.0)

  # Near-return point should be detected
  # Line 12 returns near its start at approximately (1738249, 581705)
  found_coords <- sf::st_coordinates(result$confluences)
  near_return_pt <- c(1738249, 581705)

  found_near_return <- FALSE
  for (i in seq_len(nrow(found_coords))) {
    d <- sqrt(sum((found_coords[i, 1:2] - near_return_pt)^2))
    if (d < 2) {
      found_near_return <- TRUE
      break
    }
  }

  expect_true(found_near_return,
              info = "Near-return point at (1738249, 581705) should be detected")
})


test_that("split_at_confluences actually splits lines at confluence points", {
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  skip_if(lines_path == "", "Test data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)
  lines_cast <- sf::st_cast(lines, "LINESTRING")
  n_input <- nrow(lines_cast)

  result <- split_at_confluences(lines, tolerance = 1.0)

  # Should have more output lines than input due to splitting
  expect_gt(nrow(result$lines), n_input)

  # Some lines should be marked as split
  expect_gt(sum(result$lines$was_split), 0)

  # All output should be LINESTRING
  expect_true(all(sf::st_geometry_type(result$lines) == "LINESTRING"))
})


test_that("split_at_confluences results in lines that dont pass through confluences", {
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  expected_path <- system.file("extdata", "split_lines", "point_intersections_v2.gpkg",
                               package = "streamgis")

  skip_if(lines_path == "", "Test data not available")
  skip_if(expected_path == "", "Expected points data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)
  result <- split_at_confluences(lines, tolerance = 1.0)

  # After splitting, no line should have a confluence in its interior
  # (all confluences should be at line endpoints)
  conf_coords <- sf::st_coordinates(result$confluences)

  for (ci in seq_len(nrow(conf_coords))) {
    conf_pt <- conf_coords[ci, 1:2]
    conf_geom <- sf::st_sfc(sf::st_point(conf_pt), crs = sf::st_crs(result$lines))

    for (li in seq_len(nrow(result$lines))) {
      line_geom <- sf::st_geometry(result$lines)[li]
      dist_to_line <- as.numeric(sf::st_distance(conf_geom, line_geom))

      if (dist_to_line < 1.0) {
        # Point is on this line - verify it's at an endpoint
        start_pt <- lwgeom::st_startpoint(line_geom)
        end_pt <- lwgeom::st_endpoint(line_geom)

        dist_to_start <- as.numeric(sf::st_distance(conf_geom, start_pt))
        dist_to_end <- as.numeric(sf::st_distance(conf_geom, end_pt))

        is_at_endpoint <- (dist_to_start < 1.0 || dist_to_end < 1.0)

        expect_true(
          is_at_endpoint,
          info = paste("Confluence", ci, "at (",
                       round(conf_pt[1], 1), ",", round(conf_pt[2], 1),
                       ") is interior to line", li)
        )
      }
    }
  }
})


test_that("split_at_confluences handles near-touching endpoints", {
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  skip_if(lines_path == "", "Test data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)

  # Run the function - tolerance=1 should catch near-misses
  result <- split_at_confluences(lines, tolerance = 1.0)

  # Point (1737999, 581865.2) is where line 5 starts and line 4 passes through
  # but they don't exactly touch - this should still be detected
  expected_pt <- c(1737999, 581865.2)
  found_coords <- sf::st_coordinates(result$confluences)

  found_pt <- FALSE
  for (i in seq_len(nrow(found_coords))) {
    d <- sqrt(sum((found_coords[i, 1:2] - expected_pt)^2))
    if (d < 2) {
      found_pt <- TRUE
      break
    }
  }

  expect_true(found_pt,
              info = "Near-touching endpoint at (1737999, 581865.2) should be detected")
})


test_that("split_at_confluences preserves original attributes after splitting", {

  lines_path <- system.file("extdata", "ifc_coho.gpkg",
                            package = "streamgis")

  # lines_path <- "C:/Users/mattj/Desktop/demo2/demo2.gpkg"

  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")

  skip_if(lines_path == "", "Test data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)

  result <- split_at_confluences(lines, tolerance = 1.0)

  # This line should be split into three parts
  check_3 <- result$lines[result$lines$original_fid == 3, ]

  result$lines$new_id <- 1:nrow(result$lines)
  mline <- result$lines
  # st_write(mline, "split_lines_at_points_output.gpkg", delete_dsn = TRUE)


  # Check that tracking columns are present
  expect_true("original_fid" %in% names(result$lines))
  expect_true("was_split" %in% names(result$lines))

  # Check that confluence tracking columns are present
  expect_true("confluence_id" %in% names(result$confluences))
  expect_true("degree" %in% names(result$confluences))

  # All degrees should be >= 1 (a confluence point should touch at least one line)
  expect_true(all(result$confluences$degree >= 1))
})


test_that("split_lines_at_points works with split_lines dataset", {

  # This test uses split_lines_at_points with the expected intersection points
  lines_path <- system.file("extdata", "split_lines", "line_segments.gpkg",
                            package = "streamgis")
  expected_path <- system.file("extdata", "split_lines", "point_intersections_v2.gpkg",
                               package = "streamgis")

  skip_if(lines_path == "", "Test data not available")
  skip_if(expected_path == "", "Expected points data not available")

  lines <- sf::st_read(lines_path, quiet = TRUE)
  split_points <- sf::st_read(expected_path, quiet = TRUE)

  # Run split_lines_at_points
  result <- split_lines_at_points(lines, split_points, snap_tolerance = 2.0,
                                  endpoint_tolerance = 1.0)

  names(result)
  result$lines$new_id <- 1:nrow(result$lines)
  mline <- result$lines
  # mapview(mline)
  # st_write(mline, "split_lines_at_points_output.gpkg", delete_dsn = TRUE)


  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("lines", "points"))
  expect_s3_class(result$lines, "sf")
  expect_s3_class(result$points, "sf")

  # Should have more lines than input (due to splitting)
  expect_gte(nrow(result$lines), nrow(sf::st_cast(lines, "LINESTRING")))

  # All geometries should be LINESTRING
  expect_true(all(sf::st_geometry_type(result$lines) == "LINESTRING"))
})
