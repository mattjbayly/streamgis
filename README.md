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

### Commonly-used functions

* `points_on_line()` Samples points on stream center line.

* `cross_section_lines()` Perpendicular Cross-Section Lines.

* `clean_reach_buffer()` Clean buffer along stream cut by cross sections.


### Installation

'streamgis' is only available on GitHub.

The development version can be installed in R with the following code:

`devtools::install_github("mattjbayly/streamgis")`
