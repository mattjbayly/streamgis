# streamgis

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/streamgis)](https://cran.r-project.org/package=streamgis)

Hydrology, watershed and river bathymetry functions for spatial data in R

### Spatial data  

The 'streamgis' package is intended as a free and readily-available resource for common
functions to support aquatic ecosystem assessments, hydrology and 
other watershed functions with spatial data.

Note that the current version of 'streamgis' is built on 'sp' and 'rgdal' and
will be modified in the near future.

### Installation

`remove.package("streamgis")`
`devtools::install_github("mattjbayly/streamgis")`

### Commonly-used Functions and Workflow

* `points_on_line()` Samples points on stream center line.

* `cross_section_lines()` Create perpendicular Cross-Section Lines.

* `sample_profiles_and_canopy_angle()` Calculate canopy open angle with vegetation height raster or DSM - DTM product.

* `clean_reach_buffer()` Clean buffer along stream cut by cross sections.

* `bcfwa_geometry()` Get the x,y,z geometry from BCFWA streamlines.


### Riparian Analysis

Use the `sample_profiles_and_canopy_angle()` function to calculate canopy open angle.

[![Canopy Open Angle](./man/figures/canopyopenangle.png)](https://onlinelibrary.wiley.com/doi/10.1111/1752-1688.12655?utm_source=researchgate)


### Installation

'streamgis' is only available on GitHub.

The development version can be installed in R with the following code:

`devtools::install_github("mattjbayly/streamgis")`
