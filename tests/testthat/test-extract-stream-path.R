test_that("test extract stream path", {

    # We will use this test to test the extract_stream_path function.
    # We have prepared a simple gpkg file with stream lines and clip points.
    # We have also prepared results for the expected output for PEP001, PEP17, PEP20, PEP99, and PEP100
    # Import a simple stream
    fname <- system.file("extdata", "/clip_points/clip_points.gpkg", package = "streamgis")
    clip_points <- sf::st_read(fname)
    class(clip_points)

    fname <- system.file("extdata", "/clip_points/clip_lines.gpkg", package = "streamgis")
    clip_lines <- sf::st_read(fname)
    class(clip_lines)

    # Run the extract_stream_path function for each set of points

    # Possible steps for the function.
    # Snap points to stream segment (vertex or segment, not just line verticies)
    # Split/cut the gempetry of the stream segment at the snapped point locations.
    # Find the shortest path on the stream network from point 1 to point 2.
    # Extract the path (streamlines between points 1 and points 2)
    # Note that stream segment may be multi-part of "sf" "data.frame"
    # We want to obtain the original attributes from the clip_lines attribute table.
    # We want to clip the stream segment at the point intersection point for each
    # of point 1 and point 2.
    # we then want to get the lines between these two clipped points.
    # we want to return the line segments to the user.

    # Then loop through all other PEP IDs
    pep_ids <- c("PEP001", "PEP17", "PEP20", "PEP99", "PEP100", "PEP101", "PEP102",
                 "PEP103", "PEP104", "PEP105")

    for (i in 1:length(pep_ids)) {

        # i = 1 # i = 5
        this_pep_id <- pep_ids[i]
        print(paste0("Testing extract_stream_path for ", this_pep_id))

        # -------------------------------------------------------
        # Clip streams for PEP17
        clip_pts_target <- clip_points[clip_points$NAME == this_pep_id, ]
        nrow(clip_pts_target) # should be 2
        point_1 <- clip_pts_target[1, ]
        point_2 <- clip_pts_target[2, ]
        streamlines <- clip_lines
        path_result <- extract_stream_path(streamlines, point_1, point_2)

        plot(st_geometry(path_result), col = 'lightblue')
        plot(st_geometry(streamlines), col = 'grey', add = TRUE)
        plot(st_geometry(path_result), col = 'blue', lwd = 2, add = TRUE)
        plot(st_geometry(clip_pts_target), col = 'red', add = TRUE, pch = 19)

        # Load the expected output
        expected_fname <- system.file("extdata", paste0("/clip_points/tests/", this_pep_id, "_path.gpkg"), package = "streamgis")
        expected_path <- sf::st_read(expected_fname)

        round(as.numeric(st_length(expected_path)), 0)
        round(as.numeric(st_length(path_result)), 0)
        path_result$length_m

        length_1 <- sum(as.numeric(st_length(expected_path)))
        length_2 <- sum(as.numeric(st_length(path_result)))

        # We expect the difference between the lengths to be less than 2m
        expect_true(abs(length_1 - length_2) < 2)

        # We expect the rid values to be the same
        rids_expected <- expected_path$rid
        rids_result <- path_result$rid

        expect_true(all(rids_expected %in% rids_result))
        expect_true(all(rids_result %in% rids_expected))

        #Sys.sleep(4)

    }



    # Next run through all unique entires in clip_points and make sure they
    # run without issues. Don't worry about checking results here.
    # just make sure no errors
    unique_pep_ids <- unique(clip_points$NAME)
    for (i in 1:length(unique_pep_ids)) {

      this_pep_id <- unique_pep_ids[i]
        print(paste0("Running extract_stream_path for ", this_pep_id))
        clip_pts_target <- clip_points[clip_points$NAME == this_pep_id, ]
        nrow(clip_pts_target) # should be 2
        point_1 <- clip_pts_target[1, ]
        point_2 <- clip_pts_target[2, ]

        # check for empty geometries
        if (sf::st_is_empty(sf::st_geometry(point_1)) || sf::st_is_empty(sf::st_geometry(point_2))) {
            print(paste0("Skipping ", this_pep_id, " due to empty geometry"))
            next
        }

        streamlines <- clip_lines
        path_result <- extract_stream_path(streamlines, point_1, point_2)
        plot(st_geometry(path_result), col = 'lightblue')
        plot(st_geometry(streamlines), col = 'grey', add = TRUE)
        plot(st_geometry(path_result), col = 'blue', lwd = 2, add = TRUE)
        plot(st_geometry(clip_pts_target), col = 'red', add = TRUE, pch = 19)

        #Sys.sleep(2)

    }




    expect_true(1 == 1)
})
