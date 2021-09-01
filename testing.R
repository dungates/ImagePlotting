library(ImagePlotting)
convert_images()


A<-"/Users/faltesed/Documents/ImagePlottingX/mittens"
load_images(A)
convert_and_import(images)
measure_images(converted_images)
fluency_analysis(converted_images)
library(dplyr)
library(ggplot2)

#combine the data side by side
mypictures<-bind_cols(converted_images, measured_images)

imageplot_output("mypictures", "a", "info.filesize", .5)
color_analysis(converted_images)
mypictures<-bind_cols(mypictures, colors_results)

imageplot_output("mypictures", "mean_red", "mean_blue", .5)

edge_analysis(converted_images)
symmetry_analysis(converted_images)

ImagePlotting::convert_images()
devtools::document()
??load_images
??convert_and_import


