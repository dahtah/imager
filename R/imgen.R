#Image generators (parametric, noise, etc.)

##' Create an image of custom size by filling in repeated values
##'
##' This is a convenience function for quickly creating blank images, or images filled with a specific colour. See examples. 
##' 
##' @param x width (default 1)
##' @param y height (default 1)
##' @param z depth (default 1)
##' @param val fill-in values. Either a single value (for grayscale), or RGB values for colour
##' @param dim dimension vector (optional, alternative to specifying x,y,z)
##' @return an image object (class cimg)
##' @examples
##' 
##' imfill(20,20) %>% plot #Blank image of size 20x20
##' imfill(20,20,val=c(1,0,0)) %>% plot #All red image
##' imfill(dim=dim(boats)) #Blank image of the same size as the boats image
##' @author Simon Barthelmé
##' @export
imfill <- function(x=1,y=1,z=1,val=0,dim=NULL)
    {
        if (!is.null(dim))
        {
            x <- dim[1];y <- dim[2];z <- dim[3]
        }
        if (length(val) == 1)
            {
                array(val,c(x,y,z,1)) %>% cimg
            }
        else
            {
                llply(val,function(v) imfill(x,y,z,val=v)) %>% imappend("c")
            }
    }

##' Generate (Gaussian) white-noise image
##'
##' A white-noise image is an image where all pixel values are drawn IID from a certain distribution. Here they are drawn from a Gaussian.
##' 
##' @param x width
##' @param y height
##' @param z depth
##' @param cc spectrum
##' @param mean mean pixel value (default 0)
##' @param sd std. deviation of pixel values (default 1)
##' @param dim dimension vector (optional, alternative to specifying x,y,z,cc)
##' @return a cimg object
##' @examples
##' imnoise(100,100,cc=3) %>% plot(main="White noise in RGB")
##' imnoise(100,100,cc=3) %>% isoblur(5) %>% plot(main="Filtered (non-white) noise")
##' imnoise(dim=dim(boats)) #Noise image of the same size as the boats image
##' @author Simon Barthelmé
##' @export
imnoise <- function(x=1,y=1,z=1,cc=1,mean=0,sd=1,dim=NULL)
    {
        if (is.null(dim))
        {
            dim <- c(x,y,z,cc)
        }
        rnorm(prod(dim),mean=mean,sd=sd) %>% array(dim=dim) %>% cimg
    }

##' Create an image by sampling a function
##'
##' Similar to as.im.function from the spatstat package, but simpler. Creates a grid of pixel coordinates x=1:width,y=1:height and (optional) z=1:depth, and evaluates the input function at these values. 
##' 
##' @param obj a function with arguments (x,y) or (x,y,z). Must be vectorised. 
##' @param width width of the image (in pixels)
##' @param height height of the image (in pixels)
##' @param depth depth of the image (in pixels)
##' @param standardise coordinates are scaled and centered (see doc for pixel.grid)
##' @param ... ignored
##' @return an object of class cimg
##' @author Simon Barthelme
##' @examples
##' im = as.cimg(function(x,y) cos(sin(x*y/100)),100,100)
##' plot(im)
##' im = as.cimg(function(x,y) cos(sin(x*y/100)),100,100,normalise.coord=TRUE)
##' plot(im)
##' @export
as.cimg.function <- function(obj,width,height,depth=1,standardise=FALSE,dim=NULL,...)
    {
        if (!is.null(dim))
        {
            width <- dim[1]
            height <- dim[2]
            depth <- dim[3]
        }
        fun <- obj
        args <- formals(fun) %>% names
        if (depth == 1)
            {
                if (!setequal(args,c("x","y")))
                    {
                        stop("Input must be a function with arguments x,y")
                    }
                gr <- pixel.grid(dim=c(width,height,1,1),standardise=standardise,drop.unused=TRUE)
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
                gr <- pixel.grid(dim=c(width,height,depth,1),standardise=standardise,drop.unused=TRUE)
                val <- fun(x=gr$x,y=gr$y,z=gr$z)
                dim(val) <- c(width,height,depth,1)
                cimg(val)
            }
        
    }
