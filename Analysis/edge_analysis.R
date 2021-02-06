edge_analysis <- function(x) {
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
  library(dplyr)

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

  library(moments)

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

edge_analysis(images)
