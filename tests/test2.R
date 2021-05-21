library(devtools)
install_github("dungates/ImagePlotting")

devtools::install_github("dungates/ImagePlotting")


library(ImagePlotting)


load_images("Images")
result<-colors(images)
result<-measure_images(images)
edge_analysis(images)
symmetry(images)

X<-c(1,1)
Y<-c(1,1)

library(dplyr)
images<-images%>%
  mutate(X=X)



images<-images%>%
  mutate(Y=Y)


emhoff<-function(D,X,Y,S){
  transparent <- function(img) {
    magick::image_fx(img, expression = ".5*a", channel = "alpha")
  }
  
  
  library(ggplot2)
  ggplot(D, aes(X,Y)) +
    ggimage::geom_image(
      image = images$local_path,
      image_fun = transparent,
      size = S
    )
}

emhoff(images, X,Y,.4)
