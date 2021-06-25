# Install package from CRAN only if not installed, and load the library
if (!require(covr)) install.packages('covr')
library(covr)

covr <- file_coverage(
  c(
    "R/Functions.R"
  ),
  c(
    "tests/testthat/test_load_images.R"
  )
)
covr
report(covr)