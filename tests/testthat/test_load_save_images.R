library(imager)
library(Rcpp)

test_that("load_and_save_images",{
    im <- imfill(10,10,val=c(0,0,0))
     
    ff <- tempfile(fileext=".png")
    save.image(im,ff)
    im2 <- load.image(ff)
    unlink(ff)
    expect_equal(im,im2)
})