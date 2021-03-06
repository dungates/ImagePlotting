micro_image_segment<-function(x){
  library(dplyr)
  rudy <- magick::image_read(x)
  ZZZZ <- imager::magick2cimg(rudy)
  ZZZZZ <- as.data.frame(ZZZZ)
  ZZZZZ <- ZZZZZ %>%
    mutate(color = value * 255)
  # segmentation task
  Q <- max(ZZZZZ$x)
  P <- max(ZZZZZ$y)
  
#top row
R1 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > 0 & y <  .125 * P)

R2 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > 0 & y < .125 * P)

R3 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > 0 & y < .125 * P)

R4 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > 0 & y < .125 * P)

R5 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > 0 & y < .125 * P)

R6 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > 0 & y < .125 * P)

R7 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > 0 & y < .125 * P)

R8 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > 0 & y < .125 * P)

#second row
R9 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R10 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R11 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R12 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R13 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R14 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R15 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .125 *P & y <  .25 * P)

R16 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .125 *P & y <  .25 * P)

#third row
R17 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R18 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R19 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R20 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R21 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R22 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R23 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .25 *P & y <  .375 * P)

R24 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .25 *P & y <  .375 * P)

#row four
R25 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R26 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R27 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R28 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R29 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R30 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R31 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .375 *P & y <  .5 * P)

R32 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .375 *P & y <  .5 * P)

#row five
R33 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R34 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R35 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R36 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R37 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R38 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R39 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .5 *P & y <  .625 * P)

R40 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .5 *P & y <  .625 * P)

#line six
R41 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R42 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R43 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R44 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R45 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R46 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R47 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .625 *P & y <  .75 * P)

R48 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .625 *P & y <  .75 * P)

#line seven
R49 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R50 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R51 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R52 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R53 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R54 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R55 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .75 *P & y <  .875 * P)

R56 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .75 *P & y <  .875 * P)

#eighth row
R57 <- ZZZZZ %>%
  filter(x > 0 & x < .125 * Q) %>%
  filter(y > .875 *P & y <   P)

R58 <- ZZZZZ %>%
  filter(x > .125 * Q & x < .25 * Q) %>%
  filter(y > .875 *P & y <   P)

R59 <- ZZZZZ %>%
  filter(x > .25 * Q & x < .375 * Q) %>%
  filter(y > .875 *P & y <   P)

R60 <- ZZZZZ %>%
  filter(x > .375 * Q & x < .5 * Q) %>%
  filter(y > .875 *P & y <   P)

R61 <- ZZZZZ %>%
  filter(x > .5 * Q & x < .625 * Q) %>%
  filter(y > .875 *P & y <   P)

R62 <- ZZZZZ %>%
  filter(x > .625 * Q & x < .75 *Q) %>%
  filter(y > .875 *P & y <   P)

R63 <- ZZZZZ %>%
  filter(x > .75 * Q & x < .875 * Q) %>%
  filter(y > .875 *P & y <   P)

R64 <- ZZZZZ %>%
  filter(x > .875 * Q & x < Q) %>%
  filter(y > .875 *P & y <   P)

micro_segmented_images<<-data.frame(R1,
           R2,
           R3,
           R4,
           R5,
           R6,
           R7,
           R8,
           R9,
           R10,
           R11,
           R12,
           R13,
           R14,
           R15,
           R16,
           R17,
           R18,
           R19,
           R20,
           R21,
           R22,
           R23,
           R24,
           R25,
           R26,
           R27,
           R28,
           R29,
           R30,
           R31,
           R32,
           R33,
           R34,
           R35,
           R36,
           R37,
           R38,
           R39,
           R40,
           R41,
           R42,
           R43,
           R44,
           R45,
           R46,
           R47,
           R48,
           R49,
           R50,
           R51,
           R52,
           R53,
           R54,
           R55,
           R56,
           R57,
           R58,
           R59,
           R60,
           R61,
           R62,
           R63,
           R64)
         }


micro_image_segment(Images2)



