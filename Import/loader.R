# calls the working directory
library(here)
# perhaps add another function to name the output dataframe
load_images <- function(y) {
  # x in this case is the name of the directory with the images
  # images would be a great example
  working <- here()
  # return file list, full.names allows passage of the entire file paths
  return1 <- list.files(y, full.names = TRUE)
  # prints proof of concept
  print(return1)
  # full paths, if needed
  return2 <- paste(working, return1, sep = "")
  # assign back to global environment
  images <<- data.frame("local_path" = return1, "global_path" = return2)
}

# test code
load_images("Images")


