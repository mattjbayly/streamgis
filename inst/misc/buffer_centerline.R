
library(streamgis)
# Import a simple stream center line
# center_line <- st_read("./path/to/my/file.gpkg", layer = "layer name")
# or use default provided for tutorial
fname <- system.file("extdata", "center_line.gpkg", package="streamgis")
center_line <- sf::st_read(fname)
plot(sf::st_geometry(center_line))


# Sample points along line
pol <- suppressWarnings({ points_on_line(center_line,
 point_spacing = 100, epsg = 26910) })

cross_section_lines <- cross_section_lines(center_line = center_line,
 points = pol,
 cross_profile_length = 250,
 epsg = 26910)

# Make distance field continuious
fix_order <- cross_section_lines[order(cross_section_lines$l_id,
cross_section_lines$p_id), ]

fix_order$us_distance_m <- cumsum(fix_order$distance_m)
plot(fix_order['us_distance_m'])

# Overwrite original
cross_section_lines <- fix_order

# Run simple buffer trial
buff <- clean_reach_buffer(center_line = center_line,
                   buffer_width = 50,
                   cross_section_lines = cross_section_lines,
                   us_distance_colname = "distance_m",
                   epsg = 26910)

# Plot to visualize
plot(sf::st_geometry(center_line[center_line$id == 1, ]))
plot(sf::st_geometry(pol[pol$l_id == 1, ]), add = TRUE, col = "red")
plot(sf::st_geometry(cross_section_lines[cross_section_lines$l_id == 1, ]),
add = TRUE, col = "blue")
plot(sf::st_geometry(buff[buff$l_id == 1, ]), add = TRUE, col = "grey")
plot(sf::st_geometry(cross_section_lines[cross_section_lines$l_id == 1, ]),
     add = TRUE, col = "blue")



# Run simple buffer trial
buff <- clean_reach_buffer(center_line = center_line,
                           buffer_width = 200,
                           cross_section_lines = cross_section_lines,
                           us_distance_colname = "distance_m",
                           epsg = 26910)

# Plot to visualize
plot(sf::st_geometry(center_line[center_line$id == 1, ]))
plot(sf::st_geometry(pol[pol$l_id == 1, ]), add = TRUE, col = "red")
plot(sf::st_geometry(cross_section_lines[cross_section_lines$l_id == 1, ]),
     add = TRUE, col = "blue")
plot(sf::st_geometry(buff[buff$l_id == 1, ]), add = TRUE, col = "grey")
plot(sf::st_geometry(cross_section_lines[cross_section_lines$l_id == 1, ]),
     add = TRUE, col = "blue")
