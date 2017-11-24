library(imager)
library(Rcpp)

test_that("cpp_plugin",{

    foo.inline <- "
NumericVector foo(NumericVector inp)
{
    CImg<double> img = as<CImg<double> >(inp);
    img.blur(3).erode(2);
    return wrap(img);
} "
    Sys.setenv("R_TESTS" = "") #Workaround for obscure bug in testthat
cppFunction(foo.inline,depends="imager")
im <- grayscale(boats)
expect_equal(im %>% isoblur(3,gaussian=FALSE) %>% erode_square(2),foo(im))
})
