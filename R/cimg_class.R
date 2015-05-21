#' imager: an R library for image processing, based on CImg
#'
#' CImg by David Tschumperlé is a C++ library for image processing. It provides most common functions for image manipulation and filtering, as well as some advanced algorithms. imager makes these functions accessible from R and adds some basic plotting and subsetting. 
#' You should install ImageMagick if you want support for common image formats (png, jpg, etc.)
#' @docType package
#' @name imager
NULL

#' @useDynLib imager
#' @importFrom Rcpp sourceCpp
NULL



##' Create a cimg object 
##'
##' cimg is a class for storing image or video/hyperspectral data.  It is designed to provide easy interaction with the CImg library, but in order to use it you need to be aware of how CImg wants its image data stored. 
##' Images have up to 4 dimensions, labelled x,y,z,c. x and y are the usual spatial dimensions, z is a depth dimension (which would correspond to time in a movie), and c is a colour dimension. Images are stored linearly in that order, starting from the top-left pixel and going along *rows* (scanline order).
##' A colour image is just three R,G,B channels in succession. A sequence of N images is encoded as R1,R2,....,RN,G1,...,GN,B1,...,BN where R_i is the red channel of frame i.
##' 
##' @title Create a cimg object 
##' @param X a four-dimensional numeric array
##' @return an object of class cimg
##' @author simon
##' @export
cimg <- function(X)
    {
        class(X) <-c("cimg","numeric")
        X
    }

##' @export
cimg.gs <- function(X)
    {
        dim(X) <- c(dim(X),1,1)
        cimg(X)
    }

##' @export
cimg.colour <- function(X)
    {
        dim(X) <- c(dim(X)[1:2],1,dim(X)[3])
        cimg(X)
    }

##' @export
plot.cimg <- function(X,frame,rescale.color=TRUE,...)
    {
        w <- width(X)
        h <- height(X)
        if (rescale.color & !all(X==0))  X <- (X-min(X))/diff(range(X))
        if (dim(X)[3] == 1) #Single image (depth=1)
            {
                
                dim(X) <- dim(X)[-3]
                if (dim(X)[3] == 1) #BW
                    {
                        dim(X) <- dim(X)[1:2]
                        X <- t(X)
                        class(X) <- "matrix"
                    }
                else{
                    X <- aperm(X,c(2,1,3))
                    class(X) <- "array"
                }
                plot(c(1,w),c(1,h),type="n",xlab="x",ylab="y",...)
                
                rasterImage(X,1,1,w,h)
            }
        else
            {
                if (missing(frame))
                    {
                        warning("Showing first frame")
                        frame <- 1
                    }
                plot.cimg(X[,,frame,],rescale.color=rescale.color,...)
            }
    }

##' Convert a pixel image to a data.frame
##'
##' This function combines the output of pixel.grid with the actual values (stored in $value)
##' 
##' @param im an image of class cimg
##' @return a data.frame
##' @author Simon Barthelme
##' @export
as.data.frame.cimg <- function(im)
    {
        gr <- pixel.grid(im)
        gr$value <- c(im)
        gr
    }


##' Convert a cimg object to a raster object
##'
##' raster objects are used by R's base graphics for plotting
##' @param im a cimg object 
##' @param frames which frames to extract (in case depth > 1)
##' @param rescale.color rescale so that pixel values are in [0,1]? (subtract min and divide by range). default TRUE
##' @return a raster object
##' @seealso plot.cimg, rasterImage
##' @author Simon Barthelme
##' @export
as.raster.cimg <- function(im,frames,rescale.color=TRUE)
    {
        w <- width(im)
        h <- height(im)

        if (dim(im)[3] == 1)
            {
                if (rescale.color & !all(im==0))  im <- (im-min(im))/diff(range(im))
                dim(im) <- dim(im)[-3]
                if (dim(im)[3] == 1) #BW
                    {
                        dim(im) <- dim(im)[1:2]
                        im <- t(im)
                        class(im) <- "matrix"
                    }
                else{
                    im <- aperm(im,c(2,1,3))
                    class(im) <- "array"
                }
                as.raster(im)
            }
        else
            {
                if (missing(frames)) frames <- 1:depth(im)
                imager::frames(im,frames) %>% llply(as.raster.cimg)
            }
    }

##' @export
print.cimg <- function(im)
    {
        d <- dim(im)
        msg <- sprintf("Image. Width: %i pix Height %i pix Depth %i Colour channels %i\n",d[1],d[2],d[3],d[4])
        print(msg)
    }

chan.index <- list("x"=1,"y"=2,"z"=3,"c"=4)

labind <- function(lst,d)
    {
        do.call(function(...) abind(...,along=d),lst)
    }

##' @export
width <- function(im) dim(im)[1]

##' @export
height <- function(im) dim(im)[2]

##' @export
spectrum <- function(im) dim(im)[4]

##' @export
depth <- function(im) dim(im)[3]

iiply <- function(im,d,fun)
    {
        if (is.character(d)) d <- chan.index[[d]]
        if (dim(im)[d] == 1)
            {
                res <- fun(im)
            }
        if (spectrum(im)==1 & depth(im)==1)
            {

                
            }
        else if (depth(im)==1)
            {
                res <- alply(im,d,fun) %>% labind(d-1)
                browser()
                dim(res) <- c(dim(res)[1:2],1,dim(res)[3])
            }
        else if (spectrum(im)==1)
            {
                res <- alply(im,d,fun) %>% labind(d-1)
                dim(res) <- c(dim(res)[1:3],1)
            }
        cimg(res)
    }

renormalise.channels <- function(im)
    {
        rn <- function(x) (x-min(x))/diff(range(x))
    }

ldim <- function(v)
    {
        if (is.vector(v)) length(v) else dim(v)
    }

##' @export
frames <- function(im,index,drop=FALSE)
    {
        if (missing(index))
            {
                index <- 1:depth(im)
            }
        res <- imsplit(im[,,index,],"z")
        nm <- paste('Frame',index)
        names(res) <- nm
        if (drop)
            {
                res <- llply(res,. %>% as.array %>% squeeze)
            }
        res
    }

##' @export
channels <- function(im,index,drop=FALSE)
    {
        if (missing(index))
            {
                index <- 1:spectrum(im)
            }
        res <- imsplit(im[,,,index],"c")
        nm <- paste('Channel ',index)
        names(res) <- nm
        if (drop)
            {
                res <- llply(res,. %>% as.array %>% squeeze)
            }
        res
    }

squeeze <- function(V)
    {
        d <- dim(V)
        dim(V) <- d[d!=1]
        V
    }



##' @export
`[.cimg` <- function(x,...)
    {
        y <- NextMethod("[",drop=FALSE)
        cimg(y)
    }



##' Load image from file
##'
##' You'll need ImageMagick for some formats. 
##' 
##' @param file path to file
##' @return an object of class 'cimg'
##' @export
load.image <- function(file)
    {
        load_image(path.expand(file))
    }

##' Save image
##'
##' You'll need ImageMagick for some formats. 
##'
##' @param im an image (of class cimg)
##' @param file path to file. The format is determined by the file's name
##' @return nothing
##' @export
save.image <- function(im,file)
    {
        save_image(im,path.expand(file))
    }


##' Returns the pixel grid for an image
##'
##' The pixel grid for image im gives the (x,y,z,c) coordinates of each successive pixel as a data.frame. The c coordinate has been renamed 'cc' to avoid conflicts with R's c function.
##' NB: coordinates start at (x=1,y=1), corresponding to the top left corner of the image
##'
##' 
##' @param im 
##' @return a data.frame
##' @export
pixel.grid <- function(im)
    {
        expand.grid(x=1:width(im),y=1:height(im),z=1:depth(im),cc=1:spectrum(im))
    }

##' @export
as.cimg <- function(x,...) UseMethod("as.cimg")


##' Create an image by sampling a function
##'
##' Similar to as.im.function from the spatstat package, but simpler. Creates a grid of pixel coordinates x=1:width,y=1:height and (optional) z=1:depth, and evaluate the input function at these values. 
##' 
##' @param fun a function with arguments (x,y) or (x,y,z). Must be vectorised. 
##' @param width 
##' @param height 
##' @param depth
##' @param normalise.coord coordinates are normalised so that x,y,z are in (0,1) (default FALSE)
##' @return an object of class cimg
##' @author Simon Barthelmé
##' @examples
##' im = as.cimg(function(x,y) cos(sin(x*y/100)),100,100)
##' plot(im)
##' im = as.cimg(function(x,y) cos(sin(x*y/100)),100,100,normalise.coord=TRUE)
##' plot(im)
##' @export
as.cimg.function <- function(fun,width,height,depth=1,normalise.coord=FALSE)
    {
        args <- formals(fun) %>% names
        if (depth == 1)
            {
                if (!setequal(args,c("x","y")))
                    {
                        stop("Input must be a function with arguments x,y")
                    }
                if (normalise.coord)
                    {
                        gr <- expand.grid(x=seq(0,1,l=width),y=seq(0,1,l=height))
                    }
                else
                    {
                        gr <- expand.grid(x=1:width,y=1:height)
                    }
               
                z <- fun(x=gr$x,y=gr$y)

                dim(z) <- c(width,height,1,1)
                cimg(z)
            }
        else 
            {
                if (!setequal(args,c("x","y","z")))
                    {
                        stop("Input must be a function with arguments x,y,z")
                    }
                if (normalise.coord)
                    {
                         gr <- expand.grid(x=seq(0,1,l=width),y=seq(0,1,l=height),z=seq(0,1,l=depth))
                    }
                else
                    {
                        gr <- expand.grid(x=1:width,y=1:height,z=1:depth)
                    }
               
                val <- fun(x=gr$x,y=gr$y,z=gr$z)
                dim(val) <- c(width,height,depth,1)
                cimg(val)
            }
        
    }

##' @export
as.cimg.array <- function(x)
    {
        if (length(dim(x))==4)
            {
                cimg(x)
            }
        else
            {
                stop('Array needs to have four dimensions')
            }
    }

##' @export
as.array.cimg <- function(x) {
    class(x) <- "array"
    x
}

##' Remove empty dimensions from an array
##'
##' Works just like Matlab's squeeze function: if anything in dim(x) equals one the corresponding dimension is removed
##' 
##' @param x an array
##' @export
squeeze <- function(x) {
    d <- dim(x)
    dim(x) <- d[d>1]
    x
}

##' @export
as.matrix.cimg <- function(x) {
    d <- dim(x)
    if (sum(d==1) == 2)
        {
            x <- squeeze(x)
            class(x) <- "matrix"
            x
        }
    else
        {
            stop("Too many non-empty dimensions")
        }
}

##' Add colour channels to an grayscale image
##'
##' @param im 
##' @return an image of class cimg
##' @author Simon Barthelme
##' @export
add.colour <- function(im)
{
    if (spectrum(im)!=1) stop('Image already has colour channels')
    imappend(list(0*im,0*im,im),"c") %>% HSLtoRGB
}

inda <- list('x'=1,'y'=2,'z'=3,'c'=4)

##' Pad image with n pixels along specified axis
##'
##' 
##' @param im the input image
##' @param nPix how many pixels to pad with 
##' @param axis which axis to pad along 
##' @param pos -1: prepend 0: center 1: append
##' @param val value to fill the padding with (default 0)
##' @return a padded image
##' @author Simon Barthelme
##' @export
pad <- function(im,nPix,axis,pos=0,val=0)
    {
        if (pos==0)
            {
                d <- rep(1,4)
                d[inda[[axis]]] <- round(nPix/2)
                pdIm <- cimg(array(val,d))
                imappend(list(pdIm,im,pdIm),axis)
            }
        else if (pos == -1)
            {
                d <- rep(1,4)
                d[inda[[axis]]] <- nPix
                pdIm <- cimg(array(val,d))
                imappend(list(pdIm,im),axis)
            }
        else if (pos == 1)
            {
                d <- rep(1,4)
                d[inda[[axis]]] <- nPix
                pdIm <- cimg(array(val,d))
                imappend(list(im,pdIm),axis)
            }
        
    }


.onUnload <- function (libpath) {
  library.dynam.unload("imager", libpath)
}
