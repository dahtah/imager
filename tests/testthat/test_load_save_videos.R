library(imager)
library(Rcpp)

test_that("load_and_save_videos",{
    if (imager:::has.ffmpeg())
    {
        if (!is.na(system("ffmpeg -version", intern = TRUE)[1])) 
        {
            im <- imfill(10,10,3,val=c(0,0,0))
        
            ff <- tempfile(fileext=".mp4")
            save.video(im,ff)
            expect(file_test("-f",ff),TRUE)
            im2 <- load.video(ff)
            unlink(ff)
            expect_equal(im,im2)
        } 
    }
    else
    {
        TRUE
    }
}
)
