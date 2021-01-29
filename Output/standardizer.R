#standardizer script

#arguments to pass
#a. location of paths
#b. desired size OR a standard size for the dataset OR a ratio based reduction to a single sort of size
#c. alpha of the rezised images - like .5
#d. name of the new folder which will be dumped into
#e. a new dataframe that has all the data with the paths for the new files in the new folder
library(imager)
library(OpenImageR)

#openimageR resize routine
A <- OpenImageR::readImage(images$local_path)
#it prefers dataframes
B <- data.frame(A)
C <- OpenImageR::resizeImage(B, 200, 200)
OpenImageR::writeImage(C, "nerb.png")


OpenImageR::GaborFeatureExtract(images$local_path)

imager::resize(images$local_path)
