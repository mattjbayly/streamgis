test_that("test bcfwa functions", {

  library(sf)
  # Test reach break function
  tmp <- st_read("./inst/extdata/bcfwa2.gpkg")
  tmp <- st_transform(tmp, 26910)
  breaks <- read.csv("./inst/extdata/stream_reach_breaks.csv")

  extra <- data.frame(river = NA, reach = "T3 to CW2", upper = 701786765, lower = 703346284, note = NA, tmp_c_Tw8_0_00_1 = NA)
  breaks <- rbind(breaks, extra)

  extra <- data.frame(river = NA, reach = "N1 to SP2", upper = 701778125, lower = 701746017, note = NA, tmp_c_Tw8_0_00_1 = NA)
  breaks <- rbind(breaks, extra)



  # For visual inspection
  slow_plot <- FALSE

  for(i in 1:nrow(breaks)) {

    this_sec <- breaks[i, ]

    if(this_sec$reach == "MA1") {
      next
    }

    bcfwa_sec <- bcfwa_geometry(
      bcfwa = tmp,
      upstream = this_sec$upper,
      downstream = this_sec$lower,
      epsg = 26910
    )

    if(slow_plot) {
      par(mfrow = c(1, 2))
      plot(st_geometry(tmp), main = this_sec$reach)
      plot(st_geometry(bcfwa_sec$ds_path), col = "green", add = TRUE, lwd = 5)
      head(bcfwa_sec$coordinates)
      plot(bcfwa_sec$coordinates$us_distance_m,
           bcfwa_sec$coordinates$Z, type = 'l',
           col = as.factor(bcfwa_sec$coordinates$L1))

      Sys.sleep(1)
    }

  }

  expect_true(TRUE)






})

