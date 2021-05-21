# Measuring image file type conversion functions 
library(bench)
library(profvis)

library("jpeg")
library(png)


library(magick)

profvis({



img1 <- readJPEG(system.file("img", "NWFP_Cuts1.jpg", package="jpeg"))

writePNG(img1)

# install.packages('TraMineRextras')
# Conversion is done through a call to ImageMagick mogrify function. 
# This means that ImageMagick should be installed on your system. 
# It must also be listed in the path.
# convert.g(path = NULL, fileroot= "*", from = "pdf",
#          to = "png", create.path = TRUE, options = NULL)

# https://github.com/haghish/convertGraph
# install.packages("convertGraph")
# PhantomJS
# convertGraph("./example.svg", "./example.png", path = "path to executable phantomJS" )

# https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Image_IO
# Magick convert
# img2 <- image_read_jpeg('NWFP_Cuts1.jpg') #, width = 350)
# Render svg to png bitmap
# use image_write to export an image in any format to a file on disk, or in memory 
# if path = NULL
# image_write(img2, path = "new2.png", format = "png")


# readbitmap option
img3 <- read.bitmap(system.file("img", "Rlogo.png", package="png"))
# nb the PNG image has an alpha channel
str(img2)


})