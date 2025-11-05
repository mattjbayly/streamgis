# ==================================================
# Try Running with BCFWA Streamlines and GEDI LiDAR
# ==================================================

library(sf)

# Stream Lines
# fname <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/02_SSNObjects_Raw/bcfwa_lsn/edges.gpkg"

# Stream Lines
fname <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/02_SSNObjects_Raw/lidar_lsn/edges.gpkg"


# fname <- "./inst/extdata/center_line.gpkg"
center_line <- sf::st_read(fname)
center_line <- sf::st_transform(center_line, 26910)
center_line <- center_line[!duplicated(sf::st_as_text(sf::st_geometry(center_line))), ]
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

plot(sf::st_geometry(center_line[center_line$id == 10, ]))
plot(sf::st_geometry(pol[pol$l_id == 10, ]), add = TRUE, col = "red")

plot(sf::st_geometry(center_line))
plot(sf::st_geometry(pol), add = TRUE, col = "red")

nrow(pol)
st_write(pol, dsn = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/lidar_50m_pol.gpkg", delete_dsn = TRUE)
length(unique(pol$l_id))
length(unique(center_line$LINEAR_FEATURE_ID))


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


plot(sf::st_geometry(center_line[center_line$id == 12, ]))
plot(sf::st_geometry(pol[pol$l_id == 12, ]), add = TRUE, col = "red")
plot(sf::st_geometry(csl[csl$l_id == 12, ]), add = TRUE, col = "blue")

st_write(csl, dsn = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/lidar_50m_csl.gpkg", delete_dsn = TRUE)

length(unique(csl$l_id))
length(unique(center_line$LINEAR_FEATURE_ID))


# =============================================
# =============================================
# test  --- canopy open angle
# =============================================
# =============================================

tree_raster_path <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/Data/Tree_Height/GEDI_LiDAR_2019.tif"

csl_sub <- csl[csl$l_id %in% seq(1, 25, by = 1), ]
nrow(csl_sub)

spcoa <- sample_profiles_and_canopy_angle(
  csl = csl,#######################
  raster_path_tree_height = tree_raster_path,
  step_m = 1,
  center_distance = (100 / 2),
  set_vertical_offset = 1
)

# Generate sample QA plots
spcoa$points  # sf POINTs: l_id, p_id, uid, dist_m, elevation/tree height
spcoa$canopy  # one row per (l_id, p_id) with canopy_open_angle_deg


st_write(spcoa$points, dsn = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/lidar_50m_profile_pts.gpkg", delete_dsn = TRUE)


write.csv(spcoa$canopy, file = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/lidar_50m_profile_coa.csv", row.names = FALSE)



head(spcoa$points)
head(spcoa$canopy)

# ADJUST THIS.....
spcoa$points$tree_height <- round(spcoa$points$Layer_1, 1)
pts <- spcoa$points
st_geometry(pts) <- NULL

nrow(pts)
pts_test <- pts[1:10000, ]

library(dplyr)
tsum <- pts %>% group_by(uid) %>%
  summarise(
    riparian_height = max(tree_height, na.rm = TRUE),
    riparian_width = sum(tree_height > 1.5, na.rm = TRUE)
  )
head(tsum)
plot(pts_test$dist_m[pts_test$uid == "10__12"], pts_test$tree_height[pts_test$uid == "10__12"], type = 'l')

tsum$riparian_height <- ifelse(tsum$riparian_height < 0, NA, tsum$riparian_height)
tsum$riparian_height <- ifelse(tsum$riparian_height > 99, NA, tsum$riparian_height)

plot(tsum$riparian_height, tsum$riparian_width, pch = '.')

transect <- spcoa$canopy
transect <- merge(transect, tsum, by.x = "uid", by.y = "uid", all.x = TRUE)

plot(transect$riparian_height, transect$riparian_width, pch = '.')
plot(transect$riparian_height, transect$canopy_open_angle_deg, pch = '.')
plot(transect$riparian_width, transect$canopy_open_angle_deg, pch = '.')

# Merge on ID attributes
cline <- center_line
cline <- cline[, c("id", "rid", "rid_old")]
st_geometry(cline) <- NULL
head(cline)
any(duplicated(cline$id))
transect2 <- merge(transect, cline, by.x = "l_id", by.y = "id", all.x = TRUE)
head(transect2)
nrow(transect2)

write.csv(transect2, file = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/lidar_50m_profile_gedi_full.csv", row.names = FALSE)













# ==================================================
# Try Running with LiDAR and DTM/DSM
# ==================================================

library(sf)

# Stream Lines
# fname <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/02_SSNObjects_Raw/bcfwa_lsn/edges.gpkg"

# Stream Lines
fname <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/02_SSNObjects_Raw/lidar_lsn/edges.gpkg"

# fname <- "./inst/extdata/center_line.gpkg"
center_line <- sf::st_read(fname)
center_line <- sf::st_transform(center_line, 26910)
center_line <- center_line[!duplicated(sf::st_as_text(sf::st_geometry(center_line))), ]
center_line <- center_line[!sf::st_is_empty(center_line), ]
center_line$length_m <- as.numeric(sf::st_length(center_line))
summary(center_line$length_m)
# plot(sf::st_geometry(center_line))
center_line <- suppressWarnings({
  sf::st_cast(center_line, "LINESTRING")
})
center_line$id <- 1:nrow(center_line)

pol <- st_read("C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/lidar_50m_pol.gpkg")

csl <- st_read("C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/lidar_50m_csl.gpkg")


# =============================================
# =============================================
# test  --- canopy open angle
# =============================================
# =============================================

path_dtm <- "D:/nd_ss_lidar/LiDAR_DTM.tif"
path_dsm <- "D:/nd_ss_lidar/LiDAR_DSM.tif"

csl_sub <- csl[csl$l_id %in% seq(1, 25, by = 1), ]
nrow(csl_sub)

spcoa <- sample_profiles_and_canopy_angle(
  csl = csl,#######################
  raster_path_tree_height = path_dsm,
  raster_path_terrain = path_dtm,
  step_m = 1,
  center_distance = (100 / 2),
  set_vertical_offset = 1
)

# Generate sample QA plots
spcoa$points  # sf POINTs: l_id, p_id, uid, dist_m, elevation/tree height
head(spcoa$canopy)  # one row per (l_id, p_id) with canopy_open_angle_deg


st_write(spcoa$points, dsn = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/lidar_50m_profile_pts_dsm_dtm.gpkg", delete_dsn = TRUE)


write.csv(spcoa$canopy, file = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/lidar_50m_profile_coa_dsm_dtm.csv", row.names = FALSE)


head(spcoa$points)
head(spcoa$canopy)


spcoa$points$tree_height <- round(spcoa$points$LiDAR_DSM - spcoa$points$LiDAR_DTM, 1)
pts <- spcoa$points
st_geometry(pts) <- NULL

nrow(pts)
pts_test <- pts[1:10000, ]

library(dplyr)
tsum <- pts %>% group_by(uid) %>%
  summarise(
  riparian_height = max(tree_height, na.rm = TRUE),
  riparian_width = sum(tree_height > 1.5, na.rm = TRUE)
)
head(tsum)
plot(pts_test$dist_m[pts_test$uid == "10__12"], pts_test$tree_height[pts_test$uid == "10__12"], type = 'l')

tsum$riparian_height <- ifelse(tsum$riparian_height < 1.5, NA, tsum$riparian_height)
tsum$riparian_height <- ifelse(tsum$riparian_height > 100, NA, tsum$riparian_height)

plot(tsum$riparian_height, tsum$riparian_width, pch = '.')

transect <- spcoa$canopy
transect <- merge(transect, tsum, by.x = "uid", by.y = "uid", all.x = TRUE)

plot(transect$riparian_height, transect$riparian_width, pch = '.')
plot(transect$riparian_height, transect$canopy_open_angle_deg, pch = '.')
plot(transect$riparian_width, transect$canopy_open_angle_deg, pch = '.')

# Merge on ID attributes
cline <- center_line
cline <- cline[, c("id", "rid", "rid_old")]
st_geometry(cline) <- NULL
head(cline)
any(duplicated(cline$id))
transect2 <- merge(transect, cline, by.x = "l_id", by.y = "id", all.x = TRUE)
head(transect2)
nrow(transect2)

write.csv(transect2, file = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/lidar_50m_profile_dsm_dtm_full.csv", row.names = FALSE)














# Update BCFWA

# ==================================================
# Try Running with BCFWA Streamlines and GEDI LiDAR
# ==================================================

library(sf)

# Stream Lines
fname <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/02_SSNObjects_Raw/bcfwa_lsn/edges.gpkg"

# Stream Lines
# fname <- "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/02_SSNObjects_Raw/lidar_lsn/edges.gpkg"


# fname <- "./inst/extdata/center_line.gpkg"
center_line <- sf::st_read(fname)
center_line <- sf::st_transform(center_line, 26910)
center_line <- center_line[!duplicated(sf::st_as_text(sf::st_geometry(center_line))), ]
center_line <- center_line[!sf::st_is_empty(center_line), ]
center_line$length_m <- as.numeric(sf::st_length(center_line))
summary(center_line$length_m)
# plot(sf::st_geometry(center_line))
center_line <- suppressWarnings({
  sf::st_cast(center_line, "LINESTRING")
})
center_line$id <- 1:nrow(center_line)


bcfwa_gedi <- st_read("C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/bcfwa_50m_profile_pts_gedi.gpkg")

pol <- st_read("C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/bcfwa_50m.gpkg")

head(bcfwa_gedi)
head(pol)

length(unique(bcfwa_gedi$l_id))
length(unique(pol$l_id))
setdiff(unique(bcfwa_gedi$l_id), unique(pol$l_id))

# ADJUST THIS.....
bcfwa_gedi$tree_height <- round(bcfwa_gedi$Layer_1, 1)
pts <- bcfwa_gedi
st_geometry(pts) <- NULL

nrow(pts)
pts_test <- pts[1:10000, ]

library(dplyr)
tsum <- pts %>% group_by(uid) %>%
  summarise(
    riparian_height = max(tree_height, na.rm = TRUE),
    riparian_width = sum(tree_height > 1.5, na.rm = TRUE)
  )
head(tsum)
tail(tsum)

plot(pts$dist_m[pts$uid == "99__7"], pts$tree_height[pts$uid == "99__7"], type = 'l')

tsum$riparian_height <- ifelse(tsum$riparian_height < 0, NA, tsum$riparian_height)
tsum$riparian_height <- ifelse(tsum$riparian_height > 99, NA, tsum$riparian_height)

plot(tsum$riparian_height, tsum$riparian_width, pch = '.')

transect <- read.csv("C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/bcfwa_50m_profile_coa_gedi.csv")
transect <- merge(transect, tsum, by.x = "uid", by.y = "uid", all.x = TRUE)

plot(transect$riparian_height, transect$riparian_width, pch = '.')
plot(transect$riparian_height, transect$canopy_open_angle_deg, pch = '.')
plot(transect$riparian_width, transect$canopy_open_angle_deg, pch = '.')

# Merge on ID attributes
cline <- center_line
cline <- cline[, c("id", "rid", "rid_old", "LINEAR_FEATURE_ID")]
st_geometry(cline) <- NULL
head(cline)
any(duplicated(cline$id))
transect2 <- merge(transect, cline, by.x = "l_id", by.y = "id", all.x = TRUE)
head(transect2)
nrow(transect2)

write.csv(transect2, file = "C:/Users/mattj/Dropbox/EN2980 - ND SS CEMPRA Recovery Scenarios/StreamNetwork/03_Stream Network Points/PredictionPoints/riparian/bcfwa_50m_profile_gedi_full.csv", row.names = FALSE)




