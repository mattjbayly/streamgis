test_that("test tree canopy", {

  # Stream Lines
  #fname <-
  #  system.file("extdata", "./ndss/lidar_strms.gpkg", package = "streamgis")
  fname <-
    system.file("extdata", "./ndss/bcfws_strms.gpkg", package = "streamgis")

  # fname <- "./inst/extdata/center_line.gpkg"
  center_line <- sf::st_read(fname)
  center_line <- sf::st_transform(center_line, 26910)


  # fname <- "./inst/extdata/center_line.gpkg"
  center_line <- sf::st_read(fname)
  # Drop duplicate geometries
  center_line <- center_line[!duplicated(sf::st_as_text(sf::st_geometry(center_line))), ]
  # Drop emty geometires
  center_line <- center_line[!sf::st_is_empty(center_line), ]
  center_line$length_m <- as.numeric(sf::st_length(center_line))
  summary(center_line$length_m)
  # plot(sf::st_geometry(center_line))
  center_line <- suppressWarnings({
    sf::st_cast(center_line, "LINESTRING")
  })
  center_line$id <- 1:nrow(center_line)

  # Sample points along line
  pol <- suppressWarnings({
    points_on_line(center_line, point_spacing = 50)
  })

  # points <- pol


  # plot(sf::st_geometry(center_line[center_line$id == 3, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 3, ]), add = TRUE, col = "red")

  # Do distances make sense
  diff <- pol$distance_m[6] - pol$distance_m[5]
  expect_true(round(diff, 0) == 50)

  # Do points start at origin
  expect_true(round(pol$distance_m[2], 0) == 50)


  # =============================================
  # =============================================
  # test  --- cross_section_lines
  # =============================================
  # =============================================

  csl <- cross_section_lines(
    center_line = center_line,
    points = pol,
    cross_profile_length = 100,
    epsg = 26910
  )


  # plot(sf::st_geometry(center_line[center_line$id == 12, ]))
  # plot(sf::st_geometry(pol[pol$l_id == 12, ]), add = TRUE, col = "red")
  # plot(sf::st_geometry(csl[csl$l_id == 12, ]), add = TRUE, col = "blue")

  # Test that total profile legnth is reasonable
  total_length <- as.numeric(round(sum(sf::st_length(csl)), 0))
  expect_length <- nrow(pol) * 100

  diff <- abs(1 - (total_length / expect_length))
  # diff <- abs(1 - (expect_length / total_length))

  expect_true(diff < 0.1)



  # =============================================
  # =============================================
  # test  --- canopy open angle
  # =============================================
  # =============================================

  tree_raster_path <- system.file("extdata", "./ndss/gedi_tree_32610.tif", package = "streamgis")

  spcoa <- sample_profiles_and_canopy_angle(
    csl = csl,
    raster_path_tree_height = tree_raster_path,
    step_m = 1,
    center_distance = (100 / 2),
    set_vertical_offset = 1.5
  )

  # Generate sample QA plots
  spcoa$points  # sf POINTs: l_id, p_id, uid, dist_m, elevation/tree height
  spcoa$canopy  # one row per (l_id, p_id) with canopy_open_angle_deg



  if (FALSE) {
    #--------------------------------------
    # Create Diagnostic Plots
    #--------------------------------------

    u_profile <- sort(unique(spcoa$canopy$uid))
    value_col <- "Layer_1"

    # Load the tree height raster
    # read the raster
    r <- terra::rast(tree_raster_path)

    # mask values < 0.5 to NA (so they render as "clear")
    r_mask <- terra::ifel(r < 0.5, NA, r)
    r_mask <- terra::ifel(r > 99, NA, r)

    # Manually Review a Handful of the Profiles

    for (i in 1:length(u_profile)) {
      # i = 3
      print(i)

      this_profile <- u_profile[[i]]

      this_l_id <- strsplit(this_profile, "__")[[1]][1]
      this_p_id <- strsplit(this_profile, "__")[[1]][2]

      this_set <- spcoa$points[spcoa$points$uid == this_profile, ]
      this_can <- spcoa$canopy[spcoa$canopy$uid == this_profile, ]

      if (is.na(this_can$center_elev)) {
        print("Elevation data is NA")
        next
      }

      if (any(is.na(this_set[[value_col]]))) {
        print("Elevation data is NA")
        next
      }

      png(
        filename = paste0("./tmp/", i, "_crossprofile.png"),
        width = 9,
        height = 10,
        units = "in",
        res = 300
      )

      # sort by distance just in case
      this_set <- this_set[order(this_set$dist_m), ]

      # center coords
      cx <- this_can$center_dist_m[1]
      cy <- this_can$center_elev[1]


      # helper: padded extent (meters) around an sf object
      pad_extent <- function(x, pad = 5) {
        bb <- sf::st_bbox(x)
        terra::ext(bb$xmin - pad, bb$xmax + pad, bb$ymin - pad, bb$ymax + pad)
      }

      # build masked raster + palette once (as you had)
      r  <- terra::rast(tree_raster_path)
      rM <- terra::ifel(r < 0.5, NA, r)
      n_bins <- 20
      greens <- colorRampPalette(
        c(
          "white",
          "#cfeecd",
          "#a9e0a8",
          "#73ca73",
          "#47b047",
          "#2f9a2f",
          "#1f7f1f",
          "#136713",
          "#0c520c",
          "#083d08"
        )
      )(n_bins + 1)
      brks <- c(0, seq(0.5, 20, length.out = n_bins), Inf)

      # choose the profile to center on
      this_csl <- csl[csl$l_id == as.numeric(this_l_id) &
                        csl$p_id == as.numeric(this_p_id), ]
      this_pt  <- pol[pol$l_id == as.numeric(this_l_id) &
                        pol$p_id == as.numeric(this_p_id), ]

      par(mfrow = c(2, 1))

      m_length_buff <- as.numeric(st_length(csl[1, ])) * 1.1

      ## --- MAP PANEL (centered on this_csl) ---
      ext_pad <- pad_extent(this_pt, pad = m_length_buff)         # adjust pad as needed
      r_crop  <- terra::crop(rM, ext_pad, snap = "out")  # faster draw, same view

      terra::plot(
        r_crop,
        col = greens,
        breaks = brks,
        xlim = c(ext_pad[1], ext_pad[2]),
        ylim = c(ext_pad[3], ext_pad[4]),
        axes = TRUE,
        maxpixels = 1e6
      )

      plot(
        sf::st_geometry(center_line),
        add = TRUE,
        col = "blue",
        lwd = 2
      )
      plot(
        sf::st_geometry(csl),
        add = TRUE,
        col = "darkblue",
        lty = 2
      )
      plot(
        sf::st_geometry(this_csl),
        add = TRUE,
        col = "purple",
        lwd = 2,
        lty = 2
      )
      plot(
        sf::st_geometry(this_pt),
        add = TRUE,
        col = "red",
        cex = 2,
        pch = 19
      )

      ## --- PROFILE PANEL (unchanged, opaque background) ---
      value_col <- setdiff(names(this_set),
                           c("l_id", "p_id", "uid", "dist_m", "geometry"))[1]

      ymax_plot <- max(c(this_set[[value_col]], this_can$center_elev))
      ymax_plot <- ymax_plot + 1

      ymin_plot <- min(c(this_set[[value_col]], this_can$center_elev))
      ymin_plot <- ymin_plot - 1
      ymin_plot <- ifelse(ymin_plot < 0, 0, ymin_plot)

      # draw the empty frame so we can add a filled polygon under it
      plot(
        this_set$dist_m,
        this_set[[value_col]],
        type = "n",
        xlab = "Cross Sectional Distance (m)",
        ylab = "Tree Height (m)",
        ylim = c(ymin_plot, ymax_plot),
        main = paste0(
          "Profile ID: ",
          this_profile,
          ", COA: ",
          round(this_can$canopy_open_angle_deg, 1)
        ),
        panel.first = {
          usr <- par("usr")
          rect(usr[1],
               usr[3],
               usr[2],
               usr[4],
               col = "white",
               border = NA)
        }
      )

      # shade area under the curve to baseline = 0
      x <- this_set$dist_m
      y <- this_set[[value_col]]
      baseline <- 0

      # keep inside y-limits and handle NA gaps cleanly
      ok <- !is.na(x) & !is.na(y)
      x <- x[ok]
      y <- pmin(pmax(y, baseline), 9999)  # clip to ylim (0..5)

      if (any(is.na(y))) {
        print("Missing data")
        next
      }

      if (length(x) > 1) {
        polygon(
          c(x, rev(x)),
          c(rep(baseline, length(x)), rev(y)),
          col = adjustcolor("lightgreen", alpha.f = 0.6),
          border = NA
        )
      }

      # draw the curve on top
      lines(this_set$dist_m,
            this_set[[value_col]],
            col = "darkgreen",
            lwd = 2)
      abline(h = baseline,
             col = "grey70",
             lty = 3)


      points(
        this_can$center_dist_m,
        this_can$center_elev,
        pch = 19,
        cex = 1.5,
        col = "red"
      )

      # recompute the first-intersection indices on each side
      cent_idx <- which.min(abs(this_set$dist_m - cx))

      deg2rad <- function(d)
        d * pi / 180

      y_end_from_vertical <- function(cx, cy, x_end, angle_deg) {
        # angle_deg is the angle from the vertical toward the point (0° = vertical line)
        theta <- deg2rad(angle_deg)
        if (abs(tan(theta)) < .Machine$double.eps) {
          print("Angle is ~0° from vertical: x_end must equal cx (undefined slope).")
        }
        cy + (x_end - cx) / tan(theta)
      }


      # left candidate: first going left with value > center
      left_idx <- NA_integer_
      if (cent_idx > 1) {
        left_idx <- y_end_from_vertical(cx, cy, cx + m_length_buff, this_can$left_angle_deg)
      }

      # right candidate: first going right with value > center
      right_idx <- NA_integer_
      if (cent_idx < nrow(this_set)) {
        right_idx <- y_end_from_vertical(cx, cy, cx + m_length_buff, this_can$right_angle_deg)
      }

      # 12.224
      # 90 - 12.224 =  77.776

      # draw segments to those intersection points
      if (!is.na(left_idx)) {
        segments(
          cx,
          cy,
          cx - m_length_buff,
          left_idx,
          col = "red",
          lwd = 2,
          lty = 2
        )
      }
      if (!is.na(right_idx)) {
        segments(
          cx,
          cy,
          cx + m_length_buff,
          right_idx,
          col = "red",
          lwd = 2,
          lty = 2
        )
      }

      # annotate angles
      text(
        cx,
        cy,
        labels = paste0(
          "L=",
          round(this_can$left_angle_deg[1], 1),
          "°, R=",
          round(this_can$right_angle_deg[1], 1),
          "°"
        ),
        pos = 3,
        cex = 0.8
      )

      dev.off()
      Sys.sleep(0.1)


    }

  }


})
