library(magick)
#analysis test from image R

#takes result from images

measure_images <- function(x) {
  nerb <- image_read(images$local_path)
  text <- cat(image_ocr(nerb))
  meta <-image_info(nerb)
  print(text)
  print(meta)
  
}

