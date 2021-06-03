# Profiling 
# https://support.rstudio.com/hc/en-us/articles/218221837-Profiling-with-RStudio
# 

# use devtools::load_all() to load a package from sources on disk 
# install.packages("profvis") 

# https://adv-r.hadley.nz/perf-improve.html
# General R performance notes


# Built under R v4.0.5
library(profvis)

profvis({
  data(diamonds, package = "ggplot2")
  
  plot(price ~ carat, data = diamonds)
  m <- lm(price ~ carat, data = diamonds)
  abline(m, col = "red")
})




