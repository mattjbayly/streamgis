# Tests for get_upstream_downstream_segments function

test_that("get_upstream_downstream_segments finds downstream segments", {

  # Create a simple linear network: A -> B -> C
  # Line 1: (0,0) to (10,0)
  # Line 2: (10,0) to (20,0)
  # Line 3: (20,0) to (30,0)

  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10,0, 20,0), ncol = 2, byrow = TRUE))
  line3 <- sf::st_linestring(matrix(c(20,0, 30,0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(line1, line2, line3, crs = 32610)
  )

  # Get downstream from segment 1 (should return segments 2 and 3)
  result <- get_upstream_downstream_segments(streams, target = 1, direction = "downstream")

  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c(2, 3)))
})


test_that("get_upstream_downstream_segments finds upstream segments", {
  # Create a simple linear network: A -> B -> C
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10,0, 20,0), ncol = 2, byrow = TRUE))
  line3 <- sf::st_linestring(matrix(c(20,0, 30,0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(line1, line2, line3, crs = 32610)
  )

  # Get upstream from segment 3 (should return segments 1 and 2)
  result <- get_upstream_downstream_segments(streams, target = 3, direction = "upstream")

  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c(1, 2)))
})


test_that("get_upstream_downstream_segments excludes target segment", {
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10,0, 20,0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  # Get downstream from segment 1
  result <- get_upstream_downstream_segments(streams, target = 1, direction = "downstream")

  # Should only have segment 2, not segment 1
  expect_equal(nrow(result), 1)
  expect_equal(result$id, 2)
})


test_that("get_upstream_downstream_segments handles Y-junction (confluence)", {

  # Create Y-junction: two tributaries flowing into main stem
  #   Trib1 (1) \
  #              -> Main (3)
  #   Trib2 (2) /

  trib1 <- sf::st_linestring(matrix(c(0,10, 10,0), ncol = 2, byrow = TRUE))
  trib2 <- sf::st_linestring(matrix(c(0,-10, 10,0), ncol = 2, byrow = TRUE))
  main <- sf::st_linestring(matrix(c(10,0, 20,0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:3,
    name = c("trib1", "trib2", "main"),
    geometry = sf::st_sfc(trib1, trib2, main, crs = 32610)
  )

  # Upstream from main should find both tributaries
  result <- get_upstream_downstream_segments(streams, target = 3, direction = "upstream")

  expect_equal(nrow(result), 2)
  expect_true(all(result$name %in% c("trib1", "trib2")))
})


test_that("get_upstream_downstream_segments handles point target", {
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10,0, 20,0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  # Create point near segment 1
  pt <- sf::st_point(c(5, 0.1))
  point <- sf::st_sf(geometry = sf::st_sfc(pt, crs = 32610))

  result <- get_upstream_downstream_segments(streams, target = point, direction = "downstream")

  expect_equal(nrow(result), 1)
  expect_equal(result$id, 2)
})


test_that("get_upstream_downstream_segments returns empty sf when no connections", {
  # Two disconnected segments
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(100,100, 110,100), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  # No downstream from segment 1 (segment 2 is disconnected)
  result <- get_upstream_downstream_segments(streams, target = 1, direction = "downstream")

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "sf")
})


test_that("get_upstream_downstream_segments reverse_direction works", {
  # Create network where lines are digitized AGAINST flow direction
  # Physical flow: (0,0) -> (10,0) -> (20,0) -> (30,0)  [water flows left to right]
  # Digitization: (30,0) -> (20,0) -> (10,0) -> (0,0)   [lines drawn right to left]
  #
  # With reverse_direction=TRUE:
  # - Segment 1 (x=0-10) is most UPSTREAM (where water comes from)
  # - Segment 3 (x=20-30) is most DOWNSTREAM (where water goes to)

  line1 <- sf::st_linestring(matrix(c(10,0, 0,0), ncol = 2, byrow = TRUE))   # Most upstream (x=0-10)
  line2 <- sf::st_linestring(matrix(c(20,0, 10,0), ncol = 2, byrow = TRUE))  # Middle (x=10-20)
  line3 <- sf::st_linestring(matrix(c(30,0, 20,0), ncol = 2, byrow = TRUE))  # Most downstream (x=20-30)

  streams <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(line1, line2, line3, crs = 32610)
  )

  # Physical downstream from segment 1 (most upstream) should be segments 2 and 3
  result <- get_upstream_downstream_segments(
    streams,
    target = 1,
    direction = "downstream",
    reverse_direction = TRUE
  )

  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c(2, 3)))

  # Physical upstream from segment 3 (most downstream) should be segments 1 and 2
  result2 <- get_upstream_downstream_segments(
    streams,
    target = 3,
    direction = "upstream",
    reverse_direction = TRUE
  )

  expect_equal(nrow(result2), 2)
  expect_true(all(result2$id %in% c(1, 2)))
})


test_that("get_upstream_downstream_segments preserves attributes", {
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  line2 <- sf::st_linestring(matrix(c(10,0, 20,0), ncol = 2, byrow = TRUE))

  streams <- sf::st_sf(
    id = 1:2,
    stream_name = c("Upper", "Lower"),
    order = c(2, 3),
    geometry = sf::st_sfc(line1, line2, crs = 32610)
  )

  result <- get_upstream_downstream_segments(streams, target = 1, direction = "downstream")

  expect_true("stream_name" %in% names(result))
  expect_true("order" %in% names(result))
  expect_equal(result$stream_name, "Lower")
  expect_equal(result$order, 3)
})


test_that("get_upstream_downstream_segments handles longer chain", {
  # Create 5-segment chain
  lines <- lapply(0:4, function(i) {
    sf::st_linestring(matrix(c(i*10, 0, (i+1)*10, 0), ncol = 2, byrow = TRUE))
  })

  streams <- sf::st_sf(
    id = 1:5,
    geometry = sf::st_sfc(lines, crs = 32610)
  )

  # From middle (segment 3), should have 2 upstream and 2 downstream
  upstream <- get_upstream_downstream_segments(streams, target = 3, direction = "upstream")
  downstream <- get_upstream_downstream_segments(streams, target = 3, direction = "downstream")

  expect_equal(nrow(upstream), 2)
  expect_equal(nrow(downstream), 2)
  expect_true(all(upstream$id %in% c(1, 2)))
  expect_true(all(downstream$id %in% c(4, 5)))
})


test_that("get_upstream_downstream_segments validates inputs", {
  line1 <- sf::st_linestring(matrix(c(0,0, 10,0), ncol = 2, byrow = TRUE))
  streams <- sf::st_sf(id = 1, geometry = sf::st_sfc(line1, crs = 32610))

  # Invalid target index
  expect_error(
    get_upstream_downstream_segments(streams, target = 10),
    "out of bounds"
  )

  # Invalid target type
  expect_error(
    get_upstream_downstream_segments(streams, target = "not valid"),
    "must be either"
  )

  # Empty streamlines
  expect_error(
    get_upstream_downstream_segments(streams[0,], target = 1),
    "empty"
  )
})




test_that("demo case with BCFWA", {

  fname <- system.file("extdata", "/bcfwa2.gpkg", package = "streamgis")
  strm <- st_read(fname)

  my_target <- which(strm$LINEAR_FEATURE_ID == 701770690)

  ds <- get_upstream_downstream_segments(strm,
                                   target = my_target,
                                   direction = "downstream",
                                   reverse_direction = TRUE)

  us <- get_upstream_downstream_segments(strm,
                                         target = my_target,
                                         direction = "upstream",
                                         reverse_direction = TRUE)

  # Plot out results
  plot(sf::st_geometry(strm), col = "lightgrey")
  plot(sf::st_geometry(ds), col = "blue", lwd = 2, add = TRUE)
  plot(sf::st_geometry(us), col = "orange", lwd = 2, add = TRUE)
  plot(sf::st_geometry(strm[my_target, ]), col = "yellow", lwd = 9, add = TRUE)

  # create a legend
  legend("topright",
         legend = c("Downstream", "Upstream", "Target Segment"),
         col = c("blue", "orange", "yellow"),
         lwd = c(2, 2, 9),
         bty = "n")

  # strmp <- strm[, "LINEAR_FEATURE_ID"]
  # mapview(strmp)

  expect_downstream <- c(701767869, 701750615, 703344609)
  expect_upstream <- c(701779114, 701784499, 701306151)
  expect_neither <- c(701758442, 703315452, 703307544)

  # test ds contains all of expect_downstream but not any from expect_upstream or expect_neither
  expect_true(all(expect_downstream %in% ds$LINEAR_FEATURE_ID))
  expect_true(all(!expect_upstream %in% ds$LINEAR_FEATURE_ID))
  expect_true(all(!expect_neither %in% ds$LINEAR_FEATURE_ID))

  # test us contains all of expect_upstream but not any from expect_downstream or expect_neither
  expect_true(all(expect_upstream %in% us$LINEAR_FEATURE_ID))
  expect_true(all(!expect_downstream %in% us$LINEAR_FEATURE_ID))
  expect_true(all(!expect_neither %in% us$LINEAR_FEATURE_ID))


})

