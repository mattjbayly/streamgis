#----------------------------------------------------
# Build and Tests
#----------------------------------------------------

library(usethis)
library(testthat)
library(rhub)
library(devtools)
library(qpdf)
library(kableExtra)
library(testthat)

# usethis::use_roxygen_md()     # once per package (if not already)

# Loading unfinished package to memory...
rm(list = ls())
devtools::load_all()
devtools::document()
devtools::test()  # Run tests - all passed Sept 10 2025
devtools::check() # Operating system test - all passed Sept 10 2025


remove.packages("custompackage")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("username/custompackage")
# library(custompackage)
# Installing unfinished package to computer...


list.files('./inst/extdata/matrix_test/')

# Check on cran against LTR version of R
# devtools::check_win_release()

