library(imager)
library(Rcpp)

test_that("load_and_save_videos",{
    if (imager:::has.ffmpeg())
    {
        im <- imfill(10,10,3,val=c(0,0,0))
        
        ff <- tempfile(fileext=".mp4")
        save.video(im,ff)
        im2 <- load.video(ff)
        unlink(ff)
        expect_equal(im,im2)
    }
    else
    {
        TRUE
    }
}
)
