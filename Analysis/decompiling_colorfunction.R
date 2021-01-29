#original function call
plot2 <- function (img, n = 10000, lower = c(0, 0.55, 0), upper = c(0.25, 
                                                           1, 0.25), color.space = "rgb", ref.white = NULL, pch = 20, 
          main = "default", from = "sRGB", xlim = "default", ylim = "default", 
          zlim = "default", ...) 
{
  if (is.character(img)) {
    if (file.exists(img)) {
      
      #cleans the filename
      if (tolower(color.space) == "lab") {
        CIELab <- TRUE
        hsv <- FALSE
      }
      else if (tolower(color.space) == "hsv") {
        CIELab <- FALSE
        hsv <- TRUE
      }
      else {
        CIELab <- FALSE
        hsv <- FALSE
      }
      
      
      #loads the image
      img <- loadImage(img, upper = upper, lower = lower, 
                       hsv = hsv, CIELab = CIELab, sample.size = n, 
                       ref.white = ref.white)
    }
  }
  
  #error control
  else if (!is.list(img)) {
    stop("'img' must be either a valid filepath to an image or a loadImage\n         object")
  }
  if (main == "default") {
    main <- paste(basename(img$path), ",", n, "points")
  }
  if (tolower(color.space) == "lab") {
    if (!("filtered.lab.2d" %in% names(img))) {
      
      #converts color space
      pix <- convertColorSpace(img$filtered.rgb.2d, from = from, 
                               to = "Lab", sample.size = n, from.ref.white = ref.white)
    }
    else {
      pix <- img$filtered.lab.2d
    }
    xlab <- "Luminance"
    ylab <- "a (green-red)"
    zlab <- "b (blue-yellow)"
    xb <- c(0, 100)
    yb <- c(-128, 127)
    zb <- c(-128, 127)
    if (is.numeric(n) & n < nrow(pix)) {
      pix <- pix[sample(nrow(pix), n), ]
    }
    else {
      n <- "all"
    }
    colExp <- grDevices::rgb(suppressMessages(convertColorSpace(from = "Lab", 
                                                                to = "sRGB", color.coordinate.matrix = pix, sample.size = "all", 
                                                                from.ref.white = img$ref.white)))
    colExp2<<-colExp
  }
  else {
    xb <- c(0, 1)
    yb <- c(0, 1)
    zb <- c(0, 1)
    if (tolower(color.space) == "hsv") {
      pix <- img$filtered.hsv.2d
      xlab <- "Hue"
      ylab <- "Saturation"
      zlab <- "Value"
      if (is.numeric(n) & n < nrow(pix)) {
        pix <- pix[sample(nrow(pix), n), ]
      }
      else {
        n <- "all"
      }
      colExp <- apply(pix, 1, function(x) grDevices::hsv(x[1], 
                                                         x[2], x[3]))
    }
    else {
      pix <- img$filtered.rgb.2d
      if (is.numeric(n) & n < dim(pix)[1]) {
        pix <- pix[sample(nrow(pix), n), ]
      }
      else {
        n <- "all"
      }
      colExp <- apply(pix, 1, function(x) grDevices::rgb(x[1], 
                                                         x[2], x[3]))
      xlab <- "Red"
      ylab <- "Green"
      zlab <- "Blue"
    }
  }
  if (xlim[1] == "default") {
    xlim <- xb
  }
  if (ylim[1] == "default") {
    ylim <- yb
  }
  if (zlim[1] == "default") {
    zlim <- zb
  }
  scatterplot3d::scatterplot3d(pix, pch = 20, color = colExp, 
                               xlab = xlab, ylab = ylab, zlab = zlab, main = main, xlim = xlim, 
                               ylim = ylim, zlim = zlim, ...)
  return(pix)
}