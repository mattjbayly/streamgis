# Tests for split_lines_at_points function

test_that("split_lines_at_points splits line at interior point", {
  # Create a simple horizontal line
  line <- sf::st_linestring(matrix(c(0,0, 10,0, 20,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 32610))

  # Create a point near the middle of the line

  pt <- sf::st_point(c(10, 0.5))  # Slightly off the line
  points <- sf::st_sf(name = "A", geometry = sf::st_sfc(pt, crs = 32610))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should have 2 line segments after split

  expect_equal(nrow(result$lines), 2)

  # Both segments should be marked as split
  expect_true(all(result$lines$was_split))

  # Point should be marked as snapped and split performed
  expect_true(result$points$snapped[1])
  expect_true(result$points$split_performed[1])
  expect_false(result$points$at_endpoint[1])
})


test_that("split_lines_at_points does not split at endpoint", {
  # Create a simple line
  line <- sf::st_linestring(matrix(c(0,0, 10,0, 20,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 32610))

  # Create a point at the start endpoint
  pt <- sf::st_point(c(0, 0.05))  # Very close to endpoint
  points <- sf::st_sf(name = "A", geometry = sf::st_sfc(pt, crs = 32610))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should still have 1 line segment (no split)
  expect_equal(nrow(result$lines), 1)

  # Line should not be marked as split
  expect_false(result$lines$was_split[1])

  # Point should be marked as at endpoint
  expect_true(result$points$snapped[1])
  expect_false(result$points$split_performed[1])
  expect_true(result$points$at_endpoint[1])
})


test_that("split_lines_at_points handles point outside snap tolerance", {
  # Create a simple line
  line <- sf::st_linestring(matrix(c(0,0, 10,0, 20,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 32610))

  # Create a point far from the line
  pt <- sf::st_point(c(10, 10))  # 10 units away
  points <- sf::st_sf(name = "A", geometry = sf::st_sfc(pt, crs = 32610))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should still have 1 line segment (no split)
  expect_equal(nrow(result$lines), 1)

  # Point should not be snapped
  expect_false(result$points$snapped[1])
  expect_false(result$points$split_performed[1])
})


test_that("split_lines_at_points handles multiple points on same line", {
  # Create a simple line
  line <- sf::st_linestring(matrix(c(0,0, 10,0, 20,0, 30,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 32610))

  # Create two points on the line
  pt1 <- sf::st_point(c(5, 0))
  pt2 <- sf::st_point(c(25, 0))
  points <- sf::st_sf(
    name = c("A", "B"),
    geometry = sf::st_sfc(pt1, pt2, crs = 32610)
  )

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should have 3 line segments after 2 splits
  expect_equal(nrow(result$lines), 3)

  # Both points should have split performed
  expect_true(all(result$points$split_performed))
})


test_that("split_lines_at_points handles multiple lines", {
  # Create two lines
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(0,5, 10,5), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  # Create points on each line
  pt1 <- sf::st_point(c(5, 0))   # On line1
  pt2 <- sf::st_point(c(5, 5))   # On line2
  points <- sf::st_sf(
    name = c("A", "B"),
    geometry = sf::st_sfc(pt1, pt2, crs = 32610)
  )

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should have 4 line segments (each line split into 2)
  expect_equal(nrow(result$lines), 4)
})


test_that("split_lines_at_points preserves original attributes", {
  # Create a line with attributes
  line <- sf::st_linestring(matrix(c(0,0, 10,0, 20,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(
    id = 1,
    stream_name = "Test Creek",
    order = 3,
    geometry = sf::st_sfc(line, crs = 32610)
  )

  # Create a split point
  pt <- sf::st_point(c(10, 0))
  points <- sf::st_sf(name = "A", geometry = sf::st_sfc(pt, crs = 32610))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Both segments should have preserved attributes
  expect_true("stream_name" %in% names(result$lines))
  expect_true("order" %in% names(result$lines))
  expect_true(all(result$lines$stream_name == "Test Creek"))
  expect_true(all(result$lines$order == 3))
})


test_that("split_lines_at_points handles mid-segment splits", {
  # Create a line with just two vertices (one segment)
  line <- sf::st_linestring(matrix(c(0,0, 20,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 32610))

  # Create a point in the middle of the segment
  pt <- sf::st_point(c(10, 0))
  points <- sf::st_sf(name = "A", geometry = sf::st_sfc(pt, crs = 32610))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should have 2 segments
  expect_equal(nrow(result$lines), 2)

  # Verify the split point is now a vertex in both segments
  coords1 <- sf::st_coordinates(result$lines[1, ])
  coords2 <- sf::st_coordinates(result$lines[2, ])

  # Last point of first segment should be the split point
  expect_equal(coords1[nrow(coords1), 1], 10, tolerance = 0.01)

  # First point of second segment should be the split point
  expect_equal(coords2[1, 1], 10, tolerance = 0.01)
})


test_that("split_lines_at_points returns correct list structure", {
  line <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  lines <- sf::st_sf(id = 1, geometry = sf::st_sfc(line, crs = 32610))

  pt <- sf::st_point(c(5, 0))
  points <- sf::st_sf(name = "A", geometry = sf::st_sfc(pt, crs = 32610))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Should return a list with lines and points
  expect_type(result, "list")
  expect_named(result, c("lines", "points"))
  expect_s3_class(result$lines, "sf")
  expect_s3_class(result$points, "sf")

  # Lines should have tracking columns
  expect_true("original_fid" %in% names(result$lines))
  expect_true("was_split" %in% names(result$lines))

  # Points should have tracking columns
  expect_true("snapped" %in% names(result$points))
  expect_true("split_performed" %in% names(result$points))
  expect_true("at_endpoint" %in% names(result$points))
  expect_true("snap_distance" %in% names(result$points))
  expect_true("line_fid" %in% names(result$points))
})



test_that("split_lines_at_points works for BCFWA", {

  fname <- system.file("extdata", "bcfwa2.gpkg", package="streamgis")
  bcfwa <- sf::st_read(fname)
  # st_crs(bcfwa)$epsg
  lines <- bcfwa[bcfwa$LINEAR_FEATURE_ID == 701771373, ]

  pt <- sf::st_point(c(1365151.096, 583017.383))
  points <- sf::st_sf(name = c("Segment"), geometry = sf::st_sfc(pt, crs = 3005))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Plot result
  names(result)
  # "lines"  "points"
  # plot(sf::st_geometry(result$lines[2, ]))
  # plot(sf::st_geometry(result$lines), col = as.factor(rownames(result$lines)), lwd = 3, add = TRUE)
  # plot(sf::st_geometry(result$points), col = 'grey', add = TRUE)

  expect_true(round(as.numeric(st_length(result$lines[2, ]))) == 22)


  pt <- sf::st_point(c(1365133.306, 583030.398))
  points <- sf::st_sf(name = c("Vertex"), geometry = sf::st_sfc(pt, crs = 3005))

  result <- split_lines_at_points(lines, points, snap_tolerance = 1, endpoint_tolerance = 0.1)

  # Plot result
  names(result)
  # "lines"  "points"
  # plot(sf::st_geometry(result$lines[2, ]))
  # plot(sf::st_geometry(result$lines), col = as.factor(rownames(result$lines)), lwd = 3, add = TRUE)
  # plot(sf::st_geometry(result$points), col = 'grey', add = TRUE)

  expect_true(round(as.numeric(st_length(result$lines[2, ]))) == 44)

})

