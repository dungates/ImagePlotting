#functions that can get information from each of the major libraries 

#reasonably fast, somewhat annoying to parse through a magick pointer
measure_images<-function(x){
  library(magick)
  nerb<-image_read(images$local_path)
  text<-cat(image_ocr(nerb))
  meta<-image_info(nerb)
}

#this is horribly slow
fluency<-function(x){
  library(imagefluency)
  t<-img_read(images$local_path)
  a<-img_contrast(t)
  b<-img_self_similarity(t)
  c<-img_symmetry(t)
  d<-img_complexity(t)
  result<<-data.frame(a,b,c,d)
}

fluency(images$local_path)


#color distance
library(colordistance)

loader<-loadImage(images$local_path, sample.size = 5000)
plot<-loader$filtered.rgb.2d
frame2<-data.frame(plot, reference=1:dim(frame2)[1])
z<-sapply(frame2, function(x) mean(x))
plot2<-loader$filtered.hsv.2d


plotPixels(images$local_path, lower=NULL, upper=NULL)
plotPixels


