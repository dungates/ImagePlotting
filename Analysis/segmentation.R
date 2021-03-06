image_segmentation<-function(x) {
  # SEGMENTATION PROCESS
  # edgesdataframe
  rudy <- magick::image_read(x$local_path)
  rudy2 <- magick::image_canny(rudy)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  dim(ZZZZZ)
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)
  # select quarter regions
  # regions start in the upper left and head for bottom right
  
  # select regions
  # TOP ROW
  R1 <- ZZZZZ %>%
    filter(x > 0 & x < .25 * Q) %>%
    filter(y > 0 & y < .25 * P)
  
  R2 <- ZZZZZ %>%
    filter(x > .25 * Q & x < .5 * Q) %>%
    filter(y > 0 & y < .25 * P)
  
  R3 <- ZZZZZ %>%
    filter(x > .5 * Q & x < .75 * Q) %>%
    filter(y > 0 & y < .25 * P)
  
  R4 <- ZZZZZ %>%
    filter(x > .75 * Q & x < Q) %>%
    filter(y > 0 & y < .25 * P)
  
  # UPPER MIDDLE ROW
  R5 <- ZZZZZ %>%
    filter(x > 0 & x < .25 * Q) %>%
    filter(y > .25 & y < .5 * P)
  
  R6 <- ZZZZZ %>%
    filter(x > .25 * Q & x < .5 * Q) %>%
    filter(y > .25 & y < .5 * P)
  
  R7 <- ZZZZZ %>%
    filter(x > .5 * Q & x < .75 * Q) %>%
    filter(y > .25 & y < .5 * P)
  
  R8 <- ZZZZZ %>%
    filter(x > .75 * Q & x < Q) %>%
    filter(y > .25 & y < .5 * P)
  
  # LOWER MIDDLE ROW
  R9 <- ZZZZZ %>%
    filter(x > 0 & x < .25 * Q) %>%
    filter(y > .5 * P & y < .75 * P)
  
  R10 <- ZZZZZ %>%
    filter(x > .25 * Q & x < .5 * Q) %>%
    filter(y > .5 * P & y < .75 * P)
  
  R11 <- ZZZZZ %>%
    filter(x > .5 * Q & x < .75 * Q) %>%
    filter(y > .5 * P & y < .75 * P)
  
  R12 <- ZZZZZ %>%
    filter(x > .75 * Q & x < Q) %>%
    filter(y > .5 * P & y < .75 * P)
  
  # bottom row
  R13 <- ZZZZZ %>%
    filter(x > 0 & x < .25 * Q) %>%
    filter(y > .75 * P & y < P)
  
  R14 <- ZZZZZ %>%
    filter(x > .25 * Q & x < .5 * Q) %>%
    filter(y > .75 * P & y < P)
  
  R15 <- ZZZZZ %>%
    filter(x > .5 * Q & x < .75 * Q) %>%
    filter(y > .75 * P & y < P)
  
  R16 <- ZZZZZ %>%
    filter(x > .75 * Q & x < Q) %>%
    filter(y > .75 * P & y < P)}
