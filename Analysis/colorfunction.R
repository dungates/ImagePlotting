# color distance
library(colordistance)
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
  Y <<- data.frame(
    mean_red, deviation_red, mean_blue, deviation_blue, mean_green, deviation_green,
    mean_hue, deviation_hue, mean_saturation, deviation_saturation, mean_value,
    hue_region, deviation_saturation, luminance, lum_contrast
  )
}

colors(images$local_path)
