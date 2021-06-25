library(ImagePlotting)
# library("ImagePlotting", lib.loc="~/R")

# Document shows usage of the library that the testing is modeled after

# Example 
# load one bernie mitt pic 
load_images("Images/tests/LoadTestImage")
# Images/tests/LoadTestImage/Bernie_Sanders_mittens.jpg



# Other example ? 
load_images(here("Images/tests/LoadTestImage"))
# ?? could not find function "here"

load_images("Images/tests/Load10TestImages")


colors(images)


measure_images(images)


fluency(here("Images/image_1.png"))

