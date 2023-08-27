---
title: "Cross Sectional Profile Lines"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Cross Sectional Profile Lines}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



Cross-section profiles are valuable tools in hydrology for understanding, analyzing, and managing water resources and drainage systems. We will use a stream center line GIS file to generate perpendicular cross-sectional profiles.



```r
# Load or install the streamgis package
library(streamgis)

# Import a simple stream cetner line
# center_line <- st_read("./path/to/my/file.gpkg", layer = "layer name")

# or use default provided for tutorial
fname <- system.file("extdata", "center_line.gpkg", package="streamgis")
center_line <- sf::st_read(fname)
#> Reading layer `center_line' from data source 
#>   `C:\Users\mattj\AppData\Local\R\win-library\4.2\streamgis\extdata\center_line.gpkg' 
#>   using driver `GPKG'
#> Simple feature collection with 25 features and 1 field
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 634544.6 ymin: 5500596 xmax: 659225.5 ymax: 5553726
#> Projected CRS: NAD83 / UTM zone 10N
plot(sf::st_geometry(center_line))
```

![](C:/Users/mattj/AppData/Local/Temp/RtmpiUtueP/preview-70787a5e2d12.dir/cross-sectional-profile-lines_files/figure-html/setup-1.png)<!-- -->


### Sample points along line

Sample points along line or supply custom sample locations. Be sure to set the correct EPS G code for a local UTM zone. Set the point spacing to units of meters. `points_on_line` will return a `sf` spatial points object with line segment ID, point ID and cumulative distance. 



```r

points <- suppressWarnings({ points_on_line(center_line,
                                         point_spacing = 100,
                                         epsg = 26910) })
#> Assigning EPSG:26910 UTM UTM zone 10N for input data
head(points, 3)
#> Simple feature collection with 3 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 658618.9 ymin: 5550652 xmax: 658706.6 ymax: 5550830
#> Projected CRS: NAD83 / UTM zone 10N
#>     l_id p_id  group distance_m                 geometry
#> 1      2    1  start          0 POINT (658706.6 5550830)
#> 110    2    2 sample        100 POINT (658660.2 5550742)
#> 2      2    3 sample        200 POINT (658618.9 5550652)

plot(sf::st_geometry(center_line[center_line$id == 1, ]))
plot(sf::st_geometry(points[points$l_id == 1, ]), add = TRUE, col = "red")
```

![](C:/Users/mattj/AppData/Local/Temp/RtmpiUtueP/preview-70787a5e2d12.dir/cross-sectional-profile-lines_files/figure-html/pol-1.png)<!-- -->

### Create cross-sectional profiles

Next we can create cross-sectional profile lines at each of the sample points along the stream centre line. Cross-sectional profile lines will be drawn perpendicular to the stream centre line. Additional information such as distance and bearings are in the data file as attribute fields.



```r

csl <- cross_section_lines(
  center_line = center_line,
  points = points,
  cross_profile_length = 250,
  epsg = 26910)
#> Assigning EPSG:26910 UTM UTM zone 10N for input data

head(csl, 3)
#> Simple feature collection with 3 features and 7 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 658500.2 ymin: 5550613 xmax: 658817.1 ymax: 5550889
#> Projected CRS: NAD83 / UTM zone 10N
#>   l_id p_id  group distance_m bearing_next bearing1 bearing2
#> 1    2    1  start          0     209.6248 119.6248 299.6248
#> 2    2    2 sample        100     206.3168 116.3168 296.3168
#> 3    2    3 sample        200     200.0243 110.0243 290.0243
#>                         geometry
#> 1 LINESTRING (658817.1 555077...
#> 2 LINESTRING (658773.8 555069...
#> 3 LINESTRING (658737.5 555061...

plot(sf::st_geometry(center_line[center_line$id == 1, ]))
plot(sf::st_geometry(points[points$l_id == 1, ]), add = TRUE, col = "red")
plot(sf::st_geometry(csl[csl$l_id == 1, ]), add = TRUE, col = "blue")
```

![](C:/Users/mattj/AppData/Local/Temp/RtmpiUtueP/preview-70787a5e2d12.dir/cross-sectional-profile-lines_files/figure-html/cls-1.png)<!-- -->





