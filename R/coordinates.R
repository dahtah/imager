##' Return the pixel grid for an image
##'
##' The pixel grid for image im gives the (x,y,z,c) coordinates of each successive pixel as a data.frame. The c coordinate has been renamed 'cc' to avoid conflicts with R's c function.
##' NB: coordinates start at (x=1,y=1), corresponding to the top left corner of the image, unless standardise == TRUE, in which case we use the usual Cartesian coordinates with origin at the center of the image and scaled such that x varies between -.5 and .5, and a y arrow pointing up
##'
##' 
##' @param im an image
##' @param standardise If TRUE use a centered, scaled coordinate system. If FALSE use standard image coordinates (default FALSE)
##' @param drop.unused if TRUE ignore empty dimensions, if FALSE include them anyway (default TRUE)
##' @param dim a vector of image dimensions (optional, may be used instead of "im")
##' @return a data.frame
##' @examples
##' im <- as.cimg(array(0,c(10,10))) #A 10x10 image
##' pixel.grid(im) %>% head
##' pixel.grid(dim=dim(im)) %>% head #Same as above
##' pixel.grid(dim=c(10,10,3,2)) %>% head 
##' pixel.grid(im,standardise=TRUE) %>% head
##' pixel.grid(im,drop.unused=FALSE) %>% head
##' @export
pixel.grid <- function(im,standardise=FALSE,drop.unused=TRUE,dim=NULL)
    {
        if (!missing(im))
            {
                d <- dim(im)

            }
        else if (!is.null(dim))
        {
            if (!is.vector(dim) | length(dim) != 4)
                {
                    stop('Argument dim must be a vector of length 4')
                }
            else
            {
                d <- dim
            }
        }
        else
        {
            stop('You must provide either an image or a vector of dimensions')
        }
        if (standardise)
            {
                dy <- d[2]/d[1]
                dz <- d[3]/d[1]
                res <- expand.grid(x=seq(-.5,.5,l=d[1]),y=seq(dy/2,-dy/2,l=d[2]),z=seq(-dz/2,dz/2,l=d[3]),cc=1:d[4])
            }
        else
            {
                res <- expand.grid(x=1:d[1],y=1:d[2],z=1:d[3],cc=1:d[4])
            }
        if (drop.unused)
            {
                res[,d > 1]
            }
        else
            {
                res
            }
    }


##' Linear index in internal vector from pixel coordinates
##'
##' Pixels are stored linearly in (x,y,z,c) order. This function computes the vector index of a pixel given its coordinates
##' @param im an image
##' @param coords a data.frame with values x,y,z (optional), c (optional)
##' @return a vector of indices (NA if the indices are invalid)
##' @examples
##' im <- as.cimg(function(x,y) x+y,100,100)
##' px <- pixel.index(im,data.frame(x=c(3,3),y=c(1,2)))
##' im[px] #Values should be 3+1=4, 3+2=5
##' @author Simon Barthelme
##' @export
pixel.index <- function(im,coords)
    {
        d <- dim(im)
        if ("c" %in% names(im))
            {
                coords <- rename(coords,list("c"="cc")) #Safer
            }
        if (setequal(names(coords),c("x","y")))
            {
                if (depth(im) > 1)
                    {
                        stop("Coordinates are ambiguous, must specify frame")
                    }
                else if (spectrum(im) > 1)
                    {
                        stop("Coordinates are ambiguous, must specify channel")
                    }
                else
                    {
                        out <- check.x(im,coords$x)+d[1]*(check.y(im,coords$y)-1)
                    }
            }
        else if (setequal(names(coords),c("x","y","z")))
            {
                if (spectrum(im) > 1)
                    {
                        stop("Coordinates are ambiguous, must specify channel")
                    }
                else
                    {
                        out <- check.x(im,coords$x)+d[1]*(check.y(im,coords$y)-1)+(d[1]*d[2])*(check.z(im,coords$z)-1)
                    }
            }
        else if (setequal(names(coords),c("x","y","cc")))
            {
                if (depth(im) > 1)
                    {
                        stop("Coordinates are ambiguous, must specify frame")
                    }
                else
                    {
                        out <- check.x(im,coords$x)+d[1]*(check.y(im,coords$y)-1)+(d[1]*d[2])*(check.cc(im,coords$cc)-1)
                    }
            }
        else if (setequal(names(coords),c("x","y","cc","z")))
            {
                out <- check.x(im,coords$x)+d[1]*(check.y(im,coords$y)-1)+(d[1]*d[2])*(check.z(im,coords$z)-1)+prod(d[1:3])*(check.cc(im,coords$cc)-1)
            }
        else
            {
                stop("Unrecognised coordinates")
            }
        out[out < 0] <- NA
        out[out > prod(dim(im))] <- NA
        out
    }

coord.index <- function(im,index)
    {
        index <- index-1
        d <- dim(im)
        dr <- c(cumprod(d)[3:1],1)
        V <- matrix(NA,nrow=length(index),ncol=4)
        rem <- index
        for (ind in 1:4)
            {
                n <- rem %/% dr[ind]
                rem <- rem %% dr[ind]
                V[,5-ind] <- n
            }
        maxIndex <- prod(dim(im))
        V[index >= maxIndex,] <- NA
                colnames(V) <- c("x","y","z","cc")
        as.data.frame(V+1)
    }


##' Return coordinates of subset of pixels
##'
##' Typical use case: you want the coordinates of all pixels with a value above a certain threshold
##'
##' @param im the image
##' @param condition a function that takes scalars and returns logicals
##' @return coordinates of all pixels such that condition(pixel) == TRUE
##' @examples
##' im <- as.cimg(function(x,y) x+y,10,10)
##' get.locations(im,function(v) v < 4)
##' get.locations(im,function(v) v^2 + 3*v - 2 < 30)
##' 
##' @author Simon Barthelme
##' @export
get.locations <- function(im,condition)
    {
        if (!is.function(condition))
            {
                stop("condition should be a function")
            }
        coord.index(im,which(condition(im)))
    }
