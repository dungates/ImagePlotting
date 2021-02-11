# functions that can get information from each of the major libraries
library(imagefluency)
# this is horribly slow
fluency <- function(x) {
  t <- img_read(images$local_path) %>%
  result <<- data.frame(a = img_contrast(t),
         d = img_complexity(t))
}
