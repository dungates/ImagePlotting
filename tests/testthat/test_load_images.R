context("load_images")

# check_api <- function() {
#   if (not_working()) {
#     skip("API not available")
#   }
# }
#test_that("foo api returns bar when given baz", {
#  check_api()
#
# })


#` Error testing, verify return of errors
#` TBD 

#testthat::test_that("errors", {
#  testthat::expect_error(
#    report_p(-1),
#    "p cannot be less than 0"
#  )
  
#  testthat::expect_error(
#    report_p(2),
#    "p cannot be greater than 1"
#  )
#})

#test_that("multiplication works", {
#  load_images(here("Images/"))
#  expect_equal(2 * 2, 4)
#})



#` Check Default values    
#`

#testthat::test_that("errors", {
#  testthat::expect_equal(
#    report_p(p = 0.0222),
#    "p = .022"
#  )
#})

#` Load example image and check values 
#`

#` Load image 
testthat::test_that("Verify no errors on loading test image", {
  
  load_images("Images/tests/LoadTestImage")
  
  testthat::expect_output()
  #testthat::expect_equal(images, )

})
                         
#` Image was created as expected
testthat::test_that("image was created as a data.frame list", {
  
  testthat::expect_type(images, "list")
  testthat::expect_equal(class(images), "data.frame")
  #testthat::expect_equal(nrow(images), "data.frame")
  
})

#` 
testthat::test_that("image data.frame is the right size", {
  
  testthat::expect_equal(ncol(images), 2)
  testthat::expect_equal(nrow(images), 1)
  
})

#` Only verifying local path, since each user's global path is unique
testthat::test_that("Verify test image local path is correct", {
  #testthat::expect_equal(images[1], )
})

                         
#` Delete and cleanup the loading test
testthat::test_that("data.frame images deletes without error", {
  #::expect_equal(images[1]   , )
  #images deletes without error 
  rm("images")
  testthat::expect_success(TRUE)  
  #trying to access the deleted images data.frame returns error 
  #testthat::expect_
    
})

