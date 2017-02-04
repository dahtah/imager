library(imager)

test_that("add_average_mult",
{
    im <- imnoise(5,5)
    l <- imrep(im,2)
    expect_equal(add(l),2*im)
    expect_equal(average(l),im)
    expect_equal(average(l),wsum(l,c(.5,.5)))
    expect_equal(mult(l),im^2)

    l[[1]] <- imfill(5,5,val=NA)
    expect_equal(add(l,na.rm=TRUE),im)
    M <- matrix(runif(50),10,5)
    colProd <- function(M,na.rm=FALSE) apply(M,2,prod,na.rm=na.rm)
    l <- as.cimg(M) %>% imsplit("x")
    expect_equal(add(l) %>% as.vector,colSums(M))
    expect_equal(average(l) %>% as.vector,colMeans(M))
    expect_equal(mult(l) %>% as.vector,colProd(M))

    M[9,] <- NA
    l <- as.cimg(M) %>% imsplit("x")
    expect_equal(add(l) %>% as.vector,colSums(M))
    expect_equal(average(l) %>% as.vector,colMeans(M))
    expect_equal(mult(l) %>% as.vector,colProd(M))

    expect_equal(add(l,na.rm=TRUE) %>% as.vector,colSums(M,na.rm=TRUE))
    expect_equal(average(l,na.rm=TRUE) %>% as.vector,colMeans(M,na.rm=TRUE))
    expect_equal(mult(l,na.rm=TRUE) %>% as.vector,colProd(M,na.rm=TRUE))
    
})

test_that("parmed",
{
    M <- matrix(runif(50),10,5)
    colMed <- function(M,na.rm=FALSE) apply(M,2,median,na.rm=na.rm)
    l <- as.cimg(M) %>% imsplit("x")
    expect_equal(parmed(l) %>% as.vector,colMed(M))
    M[9,] <- NA
    l <- as.cimg(M) %>% imsplit("x")
    expect_equal(parmed(l) %>% as.vector,colMed(M))
    expect_equal(parmed(l,na.rm=TRUE) %>% as.vector,colMed(M,na.rm=TRUE))

    
})
