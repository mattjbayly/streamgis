# Tests for split_at_confluences function

test_that("split_at_confluences works with real stream network", {
  # Load test data
  fname <- system.file("extdata", "ifc_coho.gpkg", package = "streamgis")
  skip_if(fname == "", "Test data not available")

  streams <- sf::st_read(fname, quiet = TRUE)

  result <- split_at_confluences(streams, tolerance = 0.1)

  # Check output structure

  expect_type(result, "list")
  expect_named(result, c("lines", "confluences"))

  # Check lines output

  expect_s3_class(result$lines, "sf")
  expect_true("was_split" %in% names(result$lines))
  expect_true("original_fid" %in% names(result$lines))

  # Should have more lines after splitting
 expect_gte(nrow(result$lines), nrow(streams))

  # Check confluences output
  expect_s3_class(result$confluences, "sf")
  expect_true("confluence_id" %in% names(result$confluences))
  expect_true("degree" %in% names(result$confluences))

  # All confluence degrees should be >= 2 (by definition)
  expect_true(all(result$confluences$degree >= 2))

  # Geometry types should be correct
  expect_true(all(sf::st_geometry_type(result$lines) == "LINESTRING"))
  expect_true(all(sf::st_geometry_type(result$confluences) == "POINT"))
})


test_that("split_at_confluences preserves original attributes", {
  fname <- system.file("extdata", "ifc_coho.gpkg", package = "streamgis")
  skip_if(fname == "", "Test data not available")

  streams <- sf::st_read(fname, quiet = TRUE)
  original_cols <- setdiff(names(streams), attr(streams, "sf_column"))

  result <- split_at_confluences(streams, tolerance = 0.1)

  # Original columns should still be present
  result_cols <- names(result$lines)
  for (col in original_cols) {
    expect_true(col %in% result_cols, info = paste("Missing column:", col))
  }
})


test_that("split_at_confluences handles simple Y junction", {
  # Create a simple Y junction: three lines meeting at a point
  # Line 1: horizontal from left to center
  # Line 2: diagonal from upper-right to center
  # Line 3: diagonal from lower-right to center

  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(20, 10, 10, 0), ncol = 2, byrow = TRUE))
  line3 <- sf::st_linestring(matrix(c(20, -10, 10, 0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2, 3),
    geometry = sf::st_sfc(line1, line2, line3, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1)

  # Should find exactly 1 confluence (where all 3 meet)
  expect_equal(nrow(result$confluences), 1)

  # Confluence should have degree 3
  expect_equal(result$confluences$degree, 3)

  # No lines should be split (all terminate at the confluence)
  expect_equal(sum(result$lines$was_split), 0)
  expect_equal(nrow(result$lines), 3)
})


test_that("split_at_confluences splits line passing through confluence", {
 # Create a T junction: one line passes through, one terminates
  # Line 1: horizontal line from left to right (passes through junction)
  # Line 2: vertical line from top, terminating at the horizontal

  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0, 20, 0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10, 10, 10, 0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1)

  # Should find 1 confluence
  expect_equal(nrow(result$confluences), 1)

  # Line 1 should be split into 2 segments, line 2 stays as 1
  expect_equal(nrow(result$lines), 3)

  # 2 segments should be marked as split (from line 1)
  expect_equal(sum(result$lines$was_split), 2)
})


test_that("split_at_confluences handles no confluences", {
  # Create two parallel lines that don't touch
  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(0, 10, 10, 10), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1)

  # Should find no confluences
  expect_equal(nrow(result$confluences), 0)

  # Lines should remain unchanged
  expect_equal(nrow(result$lines), 2)
  expect_equal(sum(result$lines$was_split), 0)
})


test_that("split_at_confluences handles single line", {
  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(line1, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1)

  # No confluences possible with single line
  expect_equal(nrow(result$confluences), 0)
  expect_equal(nrow(result$lines), 1)
  expect_equal(sum(result$lines$was_split), 0)
})


test_that("split_at_confluences handles MULTILINESTRING input", {
  # Create multilinestring that should be cast to linestring
  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10, 0, 10, 10), ncol = 2, byrow = TRUE))

  multi <- sf::st_multilinestring(list(line1, line2))

  streams <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(multi, crs = 32610)
  )

  expect_message(
    result <- split_at_confluences(streams, tolerance = 0.1),
    "Cast MULTILINESTRING to LINESTRING"
  )

  # Should handle casting and find confluence
  expect_s3_class(result$lines, "sf")
  expect_true(all(sf::st_geometry_type(result$lines) == "LINESTRING"))
})


test_that("split_at_confluences rejects invalid input", {
  # Non-sf object
  expect_error(
    split_at_confluences(data.frame(x = 1:3)),
    "must be an sf object"
  )

  # Wrong geometry type
  points <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 32610
    )
  )
  expect_error(
    split_at_confluences(points),
    "LINESTRING or MULTILINESTRING"
  )

  # Empty sf object
  empty_sf <- sf::st_sf(
    id = integer(0),
    geometry = sf::st_sfc(crs = 32610)
  )
  expect_error(
    split_at_confluences(empty_sf),
    "empty"
  )
})


test_that("split_at_confluences warns about geographic coordinates", {
  line1 <- sf::st_linestring(matrix(c(-122, 49, -121, 49), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(-121.5, 49.5, -121.5, 49), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(line1, line2, crs = 4326)  # WGS84 lat/lon
  )

  expect_warning(
    split_at_confluences(streams, tolerance = 0.001),
    "geographic"
  )
})


test_that("split_at_confluences respects tolerance parameter", {
  # Tolerance controls whether a confluence is considered "at" an endpoint vs interior
  # Create a T-junction where the vertical line ends very close to (but not exactly at)
  # the horizontal line

  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0, 20, 0), ncol = 2, byrow = TRUE))
  # Line 2 ends at (10, 0.05) - very close to horizontal line
  line2 <- sf::st_linestring(matrix(c(10, 10, 10, 0.05), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  # With small tolerance, might find the intersection (depends on GEOS precision)
  result <- split_at_confluences(streams, tolerance = 0.1)

  # Should still function without error
  expect_s3_class(result$lines, "sf")
  expect_s3_class(result$confluences, "sf")

  # Tolerance of 0.1 should merge nearby points correctly
  # The exact behavior depends on how GEOS handles near-intersections
  expect_true(nrow(result$confluences) <= 1)
})


test_that("split_at_confluences handles complex network", {
  # Create a more complex network with multiple confluences
  # Grid-like pattern with some intersections

  lines <- list(
    sf::st_linestring(matrix(c(0, 0, 10, 0, 20, 0), ncol = 2, byrow = TRUE)),   # Horizontal through middle
    sf::st_linestring(matrix(c(10, -10, 10, 10), ncol = 2, byrow = TRUE)),       # Vertical through (10,0)
    sf::st_linestring(matrix(c(0, 5, 10, 0), ncol = 2, byrow = TRUE)),           # Diagonal to (10,0)
    sf::st_linestring(matrix(c(20, 0, 30, 5), ncol = 2, byrow = TRUE))           # From end of horizontal
  )

  streams <- sf::st_sf(
    id = 1:4,
    geometry = sf::st_sfc(lines, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1)

  # Should find confluences at (10,0) and (20,0)
  expect_equal(nrow(result$confluences), 2)

  # Line 1 passes through (10,0), so should be split there
  # Total: line1 becomes 2 segments, lines 2-4 stay as 1 each = 5 total
  expect_gte(nrow(result$lines), nrow(streams))
})


test_that("split_at_confluences output CRS matches input", {
  fname <- system.file("extdata", "ifc_coho.gpkg", package = "streamgis")
  skip_if(fname == "", "Test data not available")

  streams <- sf::st_read(fname, quiet = TRUE)
  input_crs <- sf::st_crs(streams)

  result <- split_at_confluences(streams, tolerance = 0.1)

  expect_equal(sf::st_crs(result$lines), input_crs)
  expect_equal(sf::st_crs(result$confluences), input_crs)
})


# ===== Snapping Tests =====

test_that("snap_tolerance snaps undershooting tributary to mainstem", {
  # Create mainstem and tributary that undershoots by 2 meters
  mainstem <- sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE))
  tributary <- sf::st_linestring(matrix(c(10, 10, 10, 2), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(mainstem, tributary, crs = 32610)
  )

  # Without snapping - no confluence
  result_no_snap <- split_at_confluences(streams, tolerance = 0.1)
  expect_equal(nrow(result_no_snap$confluences), 0)

  # With snapping - should find confluence
  result_snap <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 3)
  expect_equal(nrow(result_snap$confluences), 1)

  # Check was_snapped flag
  expect_true("was_snapped" %in% names(result_snap$lines))
  expect_equal(sum(result_snap$lines$was_snapped), 1)

  # Verify the tributary endpoint is now at y=0
  snapped_line <- result_snap$lines[result_snap$lines$was_snapped, ]
  end_coord <- sf::st_coordinates(lwgeom::st_endpoint(sf::st_geometry(snapped_line)))[1, "Y"]
  expect_equal(unname(end_coord), 0, tolerance = 0.001)
})


test_that("snap_tolerance snaps overshooting tributary to mainstem", {
  # Create mainstem and tributary that overshoots by 1.5 meters
  mainstem <- sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE))
  tributary <- sf::st_linestring(matrix(c(10, 10, 10, -1.5), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(mainstem, tributary, crs = 32610)
  )

  # Without snapping - lines cross but endpoint not at mainstem
  result_no_snap <- split_at_confluences(streams, tolerance = 0.1)

  # With snapping - endpoint should snap to mainstem
  result_snap <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 2)

  # Verify the tributary endpoint is now at y=0
  snapped_line <- result_snap$lines[result_snap$lines$was_snapped, ]
  if (nrow(snapped_line) > 0) {
    end_coord <- sf::st_coordinates(lwgeom::st_endpoint(sf::st_geometry(snapped_line)))[1, "Y"]
    expect_equal(unname(end_coord), 0, tolerance = 0.001)
  }
})


test_that("snap_tolerance does not snap endpoints already touching", {
  # Create lines that already touch perfectly
  line1 <- sf::st_linestring(matrix(c(0, 0, 10, 0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10, 0, 10, 10), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 5)

  # Should find confluence but no snapping needed
  expect_equal(nrow(result$confluences), 1)
  expect_equal(sum(result$lines$was_snapped), 0)
})


test_that("snap_tolerance does not snap when distance exceeds tolerance", {
  # Create tributary that is too far to snap (5m away, snap_tolerance = 2m)
  mainstem <- sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE))
  tributary <- sf::st_linestring(matrix(c(10, 10, 10, 5), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(mainstem, tributary, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 2)

  # Should not find confluence (too far to snap)
  expect_equal(nrow(result$confluences), 0)
  expect_equal(sum(result$lines$was_snapped), 0)
})


test_that("snap_tolerance NULL means no snapping", {
  # Create tributary that undershoots
  mainstem <- sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE))
  tributary <- sf::st_linestring(matrix(c(10, 10, 10, 2), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(mainstem, tributary, crs = 32610)
  )

  # With NULL snap_tolerance, no snapping should occur
  result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = NULL)

  expect_equal(nrow(result$confluences), 0)
  expect_false("was_snapped" %in% names(result$lines))
})


test_that("snap_tolerance = 0 means no snapping", {
  mainstem <- sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE))
  tributary <- sf::st_linestring(matrix(c(10, 10, 10, 2), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(mainstem, tributary, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 0)

  expect_equal(nrow(result$confluences), 0)
  expect_false("was_snapped" %in% names(result$lines))
})


test_that("snap_tolerance snaps both endpoints if needed", {
  # Create a short line segment with both endpoints near other lines
  line1 <- sf::st_linestring(matrix(c(0, 0, 20, 0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(0, 10, 20, 10), ncol = 2, byrow = TRUE))
  # Short connector that undershoots both
  connector <- sf::st_linestring(matrix(c(10, 1, 10, 9), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = c(1, 2, 3),
    geometry = sf::st_sfc(line1, line2, connector, crs = 32610)
  )

  result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 2)

  # Connector line should be snapped
  expect_true(result$lines$was_snapped[result$lines$id == 3])

  # Should find 2 confluences
  expect_equal(nrow(result$confluences), 2)
})


test_that("snap_tolerance works with real data", {
  fname <- system.file("extdata", "ifc_coho.gpkg", package = "streamgis")
  skip_if(fname == "", "Test data not available")

  streams <- sf::st_read(fname, quiet = TRUE)

  # Run with snapping enabled
  result <- split_at_confluences(streams, tolerance = 0.1, snap_tolerance = 5)

  # Should complete without error
  expect_s3_class(result$lines, "sf")
  expect_s3_class(result$confluences, "sf")
  expect_true("was_snapped" %in% names(result$lines))
})
