#' Load all images into one
#' 
#' @title Image Loader
#' 
#' @param y Folder where images are stored
#' 
#' @return returns a dataframe of images
#' @export
#' 
#' @examples
#' load_images(here("Images/"))
load_images <- function(y) {
  # x in this case is the name of the directory with the images
  # images would be a great example
  working <- here::here()
  # return file list, full.names allows passage of the entire file paths
  return1 <- list.files(y, full.names = TRUE)
  # prints proof of concept
  print(return1)
  # full paths, if needed
  return2 <- paste(working, return1, sep = "")
  # assign back to global environment
  images <<- data.frame("local_path" = return1, "global_path" = return2)
}

#' @title Measure Image
#' 
#' @description Measure image information and ocr. Reasonably fast, somewhat annoying to parse through a magick pointer.
#' 
#' @param x Image to be read in
#' 
#' 
#' @return Returns image information and text to console
#' @export
#' 
#' @examples
#' measure_images(here("Images/image_1.png"))
measure_images <- function(x = images$local_path) {
  amilli <- magick::image_read(images$local_path)
  text <- cat(magick::image_ocr(amilli))
  meta <- magick::image_info(amilli)
  print(meta)
  text
}

#' Get image fluency
#' 
#' @title Fluency
#' 
#' @param x Images to be read in
#' @param index Index of image to be read in, default is 1
#' 
#' @return Returns image contrast, similarity, symmetry, complexity
#' @export
#' 
#' @examples
#' fluency(here("Images/image_1.png"))
fluency <- function(x = images$local_path, index = 1) {
  t <- img_read(images$local_path[index])
  result <- as.data.frame(a = imagefluency::img_contrast(t),
                          b = imagefluency::img_self_similarity(t),
                          c = imagefluency::img_symmetry(t),
                          d = imagefluency::img_complexity(t))
}


# symmetry function
#' Gets image symmetry
#'
#' @title Image Symmetry
#'
#' @param x Folder where images are stored
#'
#' @details Detects symmetry in an image along three axes.
#' @details Closer to zero means more symmetrical, positive means the image has more ink left or up, negative the opposite
#' @details The diagonal method has defocused areas along the line y=x, the priority is reading symmetry not in the center of the image
#' @return Returns a dataframe with horizontal, vertical, and diagonal symmetry
#' @export
#'
#' @examples
#' symmetry(here("Images/"))
symmetry <- function(x) {
  library(dplyr)
  # this routine segments the image into 16 regions and calculates symmetry
  rudy <- magick::image_read(x$local_path)
  rudy2 <- magick::image_canny(rudy)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)

  #y axis symmetry
  top <- ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5 * P)

  bottom <- ZZZZZ %>%
    dplyr::filter(y > .5 * P & y == P)

  balance<-mean(top$value)-mean(bottom$value)
  horiz<-balance
  sd_top<-sd(top$value)
  sd_bottom<-sd(bottom$value)
 
  #x axis symmetry
  left <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * P)
  
  right <- ZZZZZ %>%
    dplyr::filter(x > .5)
  
  balance<-mean(left$value)-mean(right$value)
  vert<-balance
  sd_left<-sd(left$value)
  sd_right<-sd(right$value)
  
  #TRIANGLE FOLD
  T1 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5 * Q) %>%
    dplyr::filter(y > 0 & y < .5 * P)
  T2 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .25 * Q) %>%
    dplyr::filter(y > .5 * P & y < .75 * P)
  T3 <- ZZZZZ %>%
    dplyr::filter(x > 0 & x < .125 * Q) %>%
    dplyr::filter(y > .75 * P & y < .875 * P)
  T4 <- ZZZZZ %>%
    dplyr::filter(x > .5 *Q & x < .75 * Q) %>%
    dplyr::filter(y > 0 & y < .25 * P)
  T5 <- ZZZZZ %>%
    dplyr::filter(x > .75 * Q & x < .875 * Q) %>%
    dplyr::filter(y > 0 * y & y < .125 * P)
  
  #bottom right big
  T8 <- ZZZZZ %>%
    dplyr::filter(x > .5 *Q & x < Q) %>%
    dplyr::filter(y > .5 *P & y < P)
  #upper right middle
  T9 <- ZZZZZ %>%
    dplyr::filter(x > .75 *Q & x < Q) %>%
    dplyr::filter(y < .5 *P & y > .25 * P)
  #upper right small
  T10 <- ZZZZZ %>%
    dplyr::filter(x > .25 *Q & x < .5 * Q) %>%
    dplyr::filter(y > .75 * P & y < P)
  
  T11 <- ZZZZZ %>%
    dplyr::filter(x < .125 *Q & x < .25 * Q) %>%
    dplyr::filter(y > .875 * P & y <  P)
  T12 <- ZZZZZ %>%
    dplyr::filter(x > .875 * Q & x <  Q) %>%
    dplyr::filter(y > .125 * P & y < .25 * P)
  
  A<-mean(T1$value)-mean(T8$value)
  B<-mean(T4$value)-mean(T9$value)
  C<-mean(T2$value)-mean(T10$value)
  D<-mean(T3$value)-mean(T11$value)
  E<-mean(T5$value)-mean(T12$value)
  G<-B+C+((D+E)/2)/3
  H<-A+((B+C)/2)+((D+E)/2)/3

  symmetry_report<<-data.frame(horiz, sd_top, sd_bottom, vert, sd_left, sd_right, 
                        central_diagonal=A, corners_diagonal=G,
                        diagonal_overall=H)
}

# thirds function
#' Gets image symmetry
#'
#' @title Rule of Thirds
#'
#' @param x Folder where images are stored
#'
#' @details Detects intensity of the use of the thirds on a traditional photographic layout
#' @details Positive means there is more activity in the third versus the full image, negative means less
#' @return Returns a dataframe with scores for each third versus the image and a discrete with which third is dominant 
#' @export
#'
#' @examples
#' symmetry(here("Images/"))
thirds <- function(x) {
  library(dplyr)
  # this routine segments the image into 16 regions and calculates symmetry
  rudy <- magick::image_read(x$local_path)
  rudy2 <- magick::image_canny(rudy)
  ZZZZ <- imager::magick2cimg(rudy2)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$y)
  P <- max(ZZZZZ$x)
  
  
  #vert 1
  V1 <- ZZZZZ %>%
    dplyr::filter(x > .16666 * Q & x < .5 * Q)
  V2 <-ZZZZZ %>%
    dplyr::filter(x > .5 * Q & x < Q)
  V3 <-ZZZZZ %>%
    dplyr::filter(x > 0 & x < .16666 * Q)
  
  
  #vert 2
  V4 <- ZZZZZ %>%
    dplyr::filter(x > .5 & x < .83333 * Q)
  V5 <-ZZZZZ %>%
    dplyr::filter(x > 0 & x < .5* Q)
  V6 <-ZZZZZ %>%
    dplyr::filter(x > .83333 * Q & x < Q)
  
  
  #horz 1
  H1 <- ZZZZZ %>%
    dplyr::filter(y > .16666 * P & y < .5 * P)
  H2 <-ZZZZZ %>%
    dplyr::filter(y > .5 * P & y < P)
  H3 <-ZZZZZ %>%
    dplyr::filter(y > 0 & y < .16666 * P)
  
  
  #horz 2
  H4 <- ZZZZZ %>%
    dplyr::filter(y > .5 & y < .83333 * P)
  H5 <-ZZZZZ %>%
    dplyr::filter(y > 0 & y < .5* P)
  H6 <-ZZZZZ %>%
    dplyr::filter(y > .83333 * P & y < P)
  

L<-mean(H5$value)
W<-mean(H6$value)
Q<-mean(H4$value)
I<-((Q+Q+Q+W)/4)
low_hor<-L-I

L<-mean(H1$value)
W<-mean(H2$value)
Q<-mean(H3$value)
I<-((Q+Q+Q+W)/4)
high_hor<-L-I

L<-mean(V1$value)
W<-mean(V2$value)
Q<-mean(V3$value)
I<-((Q+Q+Q+W)/4)
left_vert<-L-I

L<-mean(V4$value)
W<-mean(V5$value)
Q<-mean(V6$value)
I<-((Q+Q+Q+W)/4)
right_vert<-L-I

thirds<<-data.frame(low_hor, high_hor, left_vert, right_vert)

if(left_vert > right_vert){
  if(left_vert > high_hor){
    if(left_vert > low_hor){
      focal<-"left_vert"
    }else{
      focal<-"low_hor"
    }
  }
}

if(right_vert > left_vert){
  if(right_vert > low_hor){
    if(right_vert > high_hor){
      focal<-"right_vert"
    }else{
      focal<-"high_hor"
    }
  }
}


thirds_report<<-data.frame(low_hor, high_hor, left_vert, right_vert, focal)

}



# edge analysis
#' Performs edge analysis
#' 
#' @title Edge analysis
#' 
#' @param x Folder where images are stored
#' 
#' @return Returns a dataframe (edge_report) consisting of images, PQ, ST
#' @export
#' 
#' @examples
#' load_images(here("Images/"))
edge_analysis <- function(x = images$local_path) {
  # SEGMENTATION PROCESS
  # edgesdataframe
  rudy <- magick::image_read(x)
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
    filter(y > .75 * P & y < P)
  
  # sums of each region for canny edge
  R1_edge <- sum(R1$value)
  R2_edge <- sum(R2$value)
  R3_edge <- sum(R3$value)
  R4_edge <- sum(R4$value)
  R5_edge <- sum(R5$value)
  R6_edge <- sum(R6$value)
  R7_edge <- sum(R7$value)
  R8_edge <- sum(R8$value)
  R9_edge <- sum(R9$value)
  R10_edge <- sum(R10$value)
  R11_edge <- sum(R11$value)
  R12_edge <- sum(R12$value)
  R13_edge <- sum(R13$value)
  R14_edge <- sum(R14$value)
  R15_edge <- sum(R15$value)
  R16_edge <- sum(R16$value)
  
  # deviation for each region
  R1_edge_dev <- sd(R1$value)
  R2_edge_dev <- sd(R2$value)
  R3_edge_dev <- sd(R3$value)
  R4_edge_dev <- sd(R4$value)
  R5_edge_dev <- sd(R5$value)
  R6_edge_dev <- sd(R6$value)
  R7_edge_dev <- sd(R7$value)
  R8_edge_dev <- sd(R8$value)
  R9_edge_dev <- sd(R9$value)
  R10_edge_dev <- sd(R10$value)
  R11_edge_dev <- sd(R11$value)
  R12_edge_dev <- sd(R12$value)
  R13_edge_dev <- sd(R13$value)
  R14_edge_dev <- sd(R14$value)
  R15_edge_dev <- sd(R15$value)
  R16_edge_dev <- sd(R16$value)
  
  PQ <- data.frame(
    images, R1_edge, R1_edge_dev,
    R2_edge, R2_edge_dev,
    R3_edge, R3_edge_dev,
    R4_edge, R4_edge_dev,
    R5_edge, R5_edge_dev,
    R6_edge, R6_edge_dev,
    R7_edge, R7_edge_dev,
    R8_edge, R8_edge_dev,
    R9_edge, R9_edge_dev,
    R10_edge, R10_edge_dev,
    R11_edge, R11_edge_dev,
    R12_edge, R12_edge_dev,
    R13_edge, R13_edge_dev,
    R14_edge, R14_edge_dev,
    R15_edge, R15_edge_dev,
    R16_edge, R16_edge_dev
  ) 
  
  ST <- data.frame(
    kurtosis(R1$value),
    kurtosis(R2$value),
    kurtosis(R3$value),
    kurtosis(R4$value),
    kurtosis(R5$value),
    kurtosis(R6$value),
    kurtosis(R7$value),
    kurtosis(R8$value),
    kurtosis(R9$value),
    kurtosis(R10$value),
    kurtosis(R11$value),
    kurtosis(R12$value),
    kurtosis(R13$value),
    kurtosis(R14$value),
    kurtosis(R15$value),
    kurtosis(R16$value),
    skewness(R1$value),
    skewness(R2$value),
    skewness(R3$value),
    skewness(R4$value),
    skewness(R5$value),
    skewness(R6$value),
    skewness(R7$value),
    skewness(R8$value),
    skewness(R9$value),
    skewness(R10$value),
    skewness(R11$value),
    skewness(R12$value),
    skewness(R13$value),
    skewness(R14$value),
    skewness(R15$value),
    skewness(R16$value)
  )
  edge_report <<- data.frame(images, PQ, ST)
}

#' Function to extract image colors
#' Splits image into rgb values and standard deviation of rbg and returns a string with dominant color of image.
#' 
#' @title Image plotter
#' 
#' @param X,Y
#' 
#' 
#' @return returns a dataframe of images
#' @export
#' 
#' @examples
#' colors() # This is unclear
colors <- function(X, Y) {
  loader <- loadImage(images$local_path, sample.size = 5000)
  plot <- loader$filtered.rgb.2d
  plot2 <- data.frame(plot)
  mean_red <- mean(plot2$r * 255)
  deviation_red <- sd(plot2$r * 255)
  mean_blue <- mean(plot2$b * 255)
  deviation_blue <- sd(plot2$b * 255)
  mean_green <- mean(plot2$g * 255)
  deviation_green <- sd(plot2$g * 255)
  
  # hsv colorset - im skeptical
  
  plot3 <- loader$filtered.hsv.2d
  plot4 <- data.frame(plot3)
  mean_hue <- mean(plot4$h)
  deviation_hue <- sd(plot4$h)
  mean_saturation <- mean(plot4$s)
  deviation_saturation <- sd(plot4$s)
  mean_value <- mean(plot4$v)
  deviation_value <- sd(plot4$v)
  
  # six hue algor - processes from the colors in process
  # functions pass by FALSE only report on true
  if (mean_blue > mean_red) {
    if (mean_red >= mean_green) {
      hue_region <- "Violet"
    }
    else {
      if (mean_blue < mean_green) {
        hue_region <- "Spring Green"
      }
    }
  }
  if (mean_green > mean_red) {
    if (mean_red >= mean_blue) {
      hue_region <- "Chartreuse"
    }
    else {
      if (mean_green < mean_blue) {
        hue_region <- "Azure"
      }
    }
  }
  if (mean_red >= mean_green) {
    if (mean_green >= mean_blue) {
      hue_region <- "Orange"
    }
    else {
      if (mean_red < mean_blue) {
        hue_region <- "Violet"
      }
    }
  }
  
  # luminance
  luminance <- (mean_red + mean_blue + mean_green) / 3
  # brightness with deviation of brightness
  lum_contrast <- (deviation_red + deviation_blue + deviation_green) / 3
  
  
  # push to global environment
  color_report <<- data.frame(
    mean_red, deviation_red, mean_blue, deviation_blue, mean_green, deviation_green,
    mean_hue, deviation_hue, mean_saturation, deviation_saturation, mean_value,
    hue_region, deviation_saturation, luminance, lum_contrast
  )
}
