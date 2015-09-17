#' imager: an R library for image processing, based on CImg
#'
#' CImg by David Tschumperle is a C++ library for image processing. It provides most common functions for image manipulation and filtering, as well as some advanced algorithms. imager makes these functions accessible from R and adds many utilities for accessing and working with image data from R.
#' You should install ImageMagick if you want support for image formats beyond PNG and JPEG, and ffmpeg if you need to work with videos (in which case you probably also want to take a look at experimental package imagerstreams on github).
#' Package documentation is available at http://dahtah.github.io/imager/. 
#' @docType package
#' @name imager
NULL

#' @useDynLib imager
#' @importFrom grDevices as.raster col2rgb dev.capture
#' @importFrom graphics axis plot rasterImage
#' @importFrom stats quantile rnorm
#' @importFrom plyr llply laply ldply ddply dlply ldply rename mutate
#' @importFrom png readPNG writePNG
#' @importFrom jpeg writeJPEG readJPEG
#' @importFrom stringr str_match str_split str_sub
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr "%>%"
NULL

names.coords <- c('x','y','z','c','cc')
index.coords <- list("x"=1,"y"=2,"z"=3,"c"=4,"cc"=4)
utils::globalVariables(c(".", "%>%"))

##' cimg is a class for storing image or video/hyperspectral data.  It is designed to provide easy interaction with the CImg library, but in order to use it you need to be aware of how CImg wants its image data stored. 
##' Images have up to 4 dimensions, labelled x,y,z,c. x and y are the usual spatial dimensions, z is a depth dimension (which would correspond to time in a movie), and c is a colour dimension. Images are stored linearly in that order, starting from the top-left pixel and going along *rows* (scanline order).
##' A colour image is just three R,G,B channels in succession. A sequence of N images is encoded as R1,R2,....,RN,G1,...,GN,B1,...,BN where R_i is the red channel of frame i.
##' The number of pixels along the x,y,z, and c axes is called (in that order), width, height, depth and spectrum. 
##' 
##' @title Create a cimg object 
##' @param X a four-dimensional numeric array
##' @return an object of class cimg
##' @author Simon Barthelme
##' @examples
##' cimg(array(1,c(10,10,5,3)))
##' @export
cimg <- function(X)
    {
        class(X) <-c("cimg","numeric")
        X
    }

##' Various shortcuts for extracting colour channels, frames, etc
##'
##' @param im an image
##' @param index frame index 
##' @name cimg.extract
NULL

##' Colour space conversions in imager
##' 
##' All functions listed here assume the input image has three colour channels (spectrum(im) == 3)
##' @name imager.colourspaces
##' @param im an image
NULL

##' Display an image using base graphics
##'
##' @param x the image 
##' @param frame which frame to display, if the image has depth > 1
##' @param rescale.color rescale channels so that the values are in [0,1] 
##' @param ... other parameters to be passed to plot.default (eg "main")
##' @seealso display, which is much faster
##' @export
plot.cimg <- function(x,frame,rescale.color=TRUE,...)
    {
        im <- x
        w <- width(im)
        h <- height(im)
        if (rescale.color & (diff(range(im)) > 0))  im <- (im-min(im))/diff(range(im))
        if (dim(im)[3] == 1) #Single image (depth=1)
            {
                
                
                plot(c(1,w),c(1,h),type="n",xlab="x",ylab="y",...,ylim=c(h,1))
                as.raster(im) %>% rasterImage(1,height(im),width(im),1)
#                rasterImage(im,1,1,w,h)
            }
        else
            {
                if (missing(frame))
                    {
                        warning("Showing first frame")
                        frame <- 1
                    }
                plot.cimg(im[,,frame,],rescale.color=rescale.color,...)
            }
    }

##' Convert a pixel image to a data.frame
##'
##' This function combines the output of pixel.grid with the actual values (stored in $value)
##' 
##' @param x an image of class cimg
##' @param ... arguments passed to pixel.grid
##' @return a data.frame
##' @author Simon Barthelme
##' @examples
##' im <- matrix(1:16,4,4) %>% as.cimg
##' as.data.frame(im) %>% head
##' @export
as.data.frame.cimg <- function(x,...)
    {
        gr <- pixel.grid(x,...)
        gr$value <- c(x)
        gr
    }


##' Convert a cimg object to a raster object
##'
##' raster objects are used by R's base graphics for plotting
##' @param x an image (of class cimg)
##' @param frames which frames to extract (in case depth > 1)
##' @param rescale.color rescale so that pixel values are in [0,1]? (subtract min and divide by range). default TRUE
##' @param ... ignored
##' @return a raster object
##' @seealso plot.cimg, rasterImage
##' @author Simon Barthelme
##' @export
as.raster.cimg <- function(x,frames,rescale.color=TRUE,...)
    {
        im <- x
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
print.cimg <- function(x,...)
    {
        d <- dim(x)
        msg <- sprintf("Image. Width: %i pix Height %i pix Depth %i Colour channels %i",d[1],d[2],d[3],d[4])
        print(msg)
    }

##' Image dimensions
##' @name cimg.dimensions
##' @param im an image
NULL

##' @describeIn cimg.dimensions Width of the image (in pixels)
##' @export
width <- function(im) dim(im)[1]

##' @describeIn cimg.dimensions Height of the image (in pixels)
##' @export
height <- function(im) dim(im)[2]

##' @describeIn cimg.dimensions Number of colour channels
##' @export
spectrum <- function(im) dim(im)[4]

##' @describeIn cimg.dimensions Depth of the image/number of frames in a video
##' @export
depth <- function(im) dim(im)[3]

##' @describeIn cimg.dimensions Total number of pixels (prod(dim(im)))
##' @export
nPix <- function(im) prod(dim(im))

rn <- function(x) (x-min(x))/diff(range(x))

ldim <- function(v)
    {
        if (is.vector(v)) length(v) else dim(v)
    }

##' Split a video into separate frames
##'
##' @param im an image 
##' @param index which channels to extract (default all)
##' @param drop if TRUE drop extra dimensions, returning normal arrays and not cimg objects
##' @seealso channels
##' @return a list of frames 
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
                res <- llply(res,function(v)  as.array(v) %>% squeeze)
            }
        res
    }

##' Extract one frame out of a 4D image/video
##'
##' @describeIn cimg.extract
##' @author Simon Barthelme
##' @export
frame <- function(im,index)
    {
        im[,,index,]
    }


##' Split a colour image into a list of separate channels
##'
##' @param im an image 
##' @param index which channels to extract (default all)
##' @param drop if TRUE drop extra dimensions, returning normal arrays and not cimg objects
##' @seealso frames
##' @return a list of channels
##' @examples
##' channels(boats)
##' channels(boats,1:2)
##' channels(boats,1:2,drop=TRUE) %>% str #A list of 2D arrays
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
                res <- llply(res,function(v) { as.array(v) %>% squeeze})
            }
        res
    }

##' @describeIn cimg.extract Extract an image channel
##' @param ind channel index
##' @export
channel <- function(im,ind)
    {
        im[,,,ind] 
    }

##' @describeIn cimg.extract Extract red channel
##' @export
R <- function(im) { channel(im,1) }

##' @describeIn cimg.extract Extract green channel
##' @export
G <- function(im) { channel(im,2) }

##' @describeIn cimg.extract Extract blue channel
##' @export
B <- function(im) { channel(im,3) }

##' Return pixel value at coordinates
##'
##' @param im an image (cimg object)
##' @param x x coordinate (vector)
##' @param y y coordinate (vector)
##' @param z z coordinate (vector, default 1)
##' @param cc colour coordinate (vector, default 1)
##' @return pixel values
##' @author Simon Barthelme
##' @examples
##' im <- as.cimg(function(x,y) x+y,50,50)
##' at(im,10,1)
##' at(im,10:12,1)
##' at(im,10:12,1:3)
##' @export
at <- function(im,x,y,z=1,cc=1)
    {
        as.array(im)[x,y,z,cc]
    }


##' @describeIn at return value of all colour channels at a location
##' @examples
##' color.at(boats,x=10,y=10)
##' @export
color.at <- function(im,x,y,z=1)
    {
        at(im,x,y,z,cc=1:spectrum(im))
    }

all.names <- function(cl)
    {
        if (length(cl) == 0)
            {
                NULL
            }
        else
            {
                el <- cl[[1]]
                if (is.name(el))
                    {
                        c(as.character(el),all.names(cl[-1]))
                    }
                else if (is.call(el))
                    {
                        c(all.names(el),all.names(cl[-1]))
                    }
                else
                    {
                        all.names(cl[-1])
                    }
            }
    }

##' Select part of an image
##'
##' subim selects an image part based on coordinates: it allows you to select a subset of rows, columns, frames etc. Refer to the examples to see how it works
##' 
##' @param im an image
##' @param ... various conditions defining a rectangular image region
##' @return an image with some parts cut out
##' @author Simon Barthelme
##' @examples
##' parrots <- load.image(system.file('extdata/parrots.png',package='imager'))
##' subim(parrots,x < 30) #Only the first 30 columns
##' subim(parrots,y < 30) #Only the first 30 rows
##' subim(parrots,x < 30,y < 30) #First 30 columns and rows
##' subim(parrots, sqrt(x) > 8) #Can use arbitrary expressions
##' subim(parrots,x > height/2,y > width/2)  #height and width are defined based on the image
##' subim(parrots,cc==1) #Colour axis is "cc" not "c" here because "c" is an important R function
##' ##Not run
##' ##subim(parrots,x+y==1)
##' ##can't have expressions involving interactions between variables (domain might not be square)
##' @export
subim <- function(im,...)
    {
        l <- as.list(substitute(list(...))[-1])
        consts <- data.frame(width=width(im),height=height(im),depth=depth(im),spectrum=spectrum(im))
        consts <- mutate(consts,cx=width/2,cy=height/2,cz=depth/2)
        Reduce(function(a,b) subs(a,b,consts),l,init=im)
    }

subs <- function(im,cl,consts)
    {
        if (missing(consts))
            {
               consts <- data.frame(width=width(im),height=height(im),depth=depth(im),spectrum=spectrum(im))
            }
        vl <- intersect(all.names(cl),c("x","y","z","cc"))
        if (length(vl)>1)
            {
                stop('Use only one of x,y,z,cc at a time')
            }
        else
            {
                vname <- vl
                mval <- list(x=width(im),y=height(im),z=depth(im),cc=spectrum(im))
                maxval <- mval[[vl]]
                df <- data.frame(v=1:maxval)
                df <- cbind(df,consts)
                names(df)[1] <- vname
                inds <- eval(cl,df)
                #Should find a better way to implement what follows
                if (vname == "x")
                    {
                        (as.array(im)[inds,,,,drop=FALSE]) %>% cimg
                    }
                else if (vname == "y")
                    {
                        (as.array(im)[,inds,,,drop=FALSE]) %>% cimg
                    }
                else if (vname == "z")
                    {
                        (as.array(im)[,,inds,,drop=FALSE]) %>% cimg
                    }
                else 
                    {
                        (as.array(im)[,,,inds,drop=FALSE]) %>% cimg
                    }
            }
    }

##' Array subset operator for cimg objects
##'
##' Works mostly just like the regular array version of x[...], the only difference being that it returns cimg objects when it makes sense to do so. For example im[,,,1] is just like as.array(im)[,,,1] except it returns a cimg object (containing only the first colour channel)
##' 
##' @param x an image (cimg object)
##' @param ... subsetting arguments
##' @seealso imsub, which provides a more convenient interface, crop
##' @export
`[.cimg` <- function(x,...)
    {
        y <- NextMethod("[",drop=FALSE)
        if (is.vector(y))
            {
                y
            }
        else
            {
                cimg(y)
            }
    }



##' Load image from file or URL
##'
##' You'll need ImageMagick for formats other than PNG and JPEG. If the image is actually a video, you'll need ffmpeg. If the path is actually a URL, it should start with http(s) or ftp(s). 
##' 
##' @param file path to file
##' @return an object of class 'cimg'
##' @examples
##' #Find path to example file from package
##' fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
##' im <- load.image(fpath) 
##' plot(im)
##' @export
load.image <- function(file)
    {
        has.magick <- Sys.which("convert") %>% { length(.) > 0 }
        if (has.magick)
            {
                if (grepl("^(http|ftp)s?://", file)) #URL, regex from libXML2 package
                    {
                        load_image(file)
                    }
                else
                    {
                        normalizePath(file,mustWork=TRUE) %>% load_image
                    }
            }
        else
            {
                ftype <- stringr::str_match(file,"\\.(.+)$")[1,2]
                if (ftype == "png")
                    {
                        load.png(file)
                    }
                else if (ftype == "jpeg" | ftype == "jpg")
                    {
                        load.jpeg(file)
                    }
                else
                    {
                        stop("Unsupported file format. Please convert to jpg/png or install image magick")
                    }
            }

    }

convert.im.fromPNG <- function(A)
    {
        A <- A*255
        d <- dim(A)
        if (length(d) == 3)
            {
                dim(A) <- c(d[1:2],1,d[3])
            }
        else
            {
                dim(A) <- c(d[1:2],1,1)
            }
        mirror(A,"x") %>% imrotate(-90)
    }

load.png <- function(file)
    {
        png::readPNG(file) %>% convert.im.fromPNG
    }

load.jpeg <- function(file)
    {
        jpeg::readJPEG(file) %>% convert.im.fromPNG
    }



##' Save image
##'
##' You'll need ImageMagick for formats other than PNG and JPEG. If the image is actually a video, you'll need ffmpeg.
##'
##' @param im an image (of class cimg)
##' @param file path to file. The format is determined by the file's name
##' @return nothing
##' @export
save.image <- function(im,file)
    {
        has.magick <- Sys.which("convert") %>% { length(.) > 0 }
        if (has.magick)
            {
                save_image(im,path.expand(file))
            }
        else
            {
                ftype <- stringr::str_match(file,"\\.(.+)$")[1,2]
                if (ftype == "png")
                    {
                        save.png(im,file)
                    }
                else if (ftype == "jpeg" | ftype == "jpg")
                    {
                        save.jpeg(im,file)
                    }
                else
                    {
                        stop("Unsupported file format. Please convert to jpg/png or install image magick")
                    }
            }
    }

save.png <- function(im,file)
    {
        convert.im.toPNG(im) %>% png::writePNG(file) 
    }

save.jpeg <- function(im,file)
    {
        convert.im.toPNG(im) %>% jpeg::writeJPEG(file)
    }

convert.im.toPNG <- function(A)
    {
        if (any(A > 1) | any(A < 0))
            {
                A <-  rn(A)
            }
        A <- imrotate(A,90) %>% mirror("x") 
        dim(A) <- dim(A)[-3]
        A
    }

##' Returns the pixel grid for an image
##'
##' The pixel grid for image im gives the (x,y,z,c) coordinates of each successive pixel as a data.frame. The c coordinate has been renamed 'cc' to avoid conflicts with R's c function.
##' NB: coordinates start at (x=1,y=1), corresponding to the top left corner of the image, unless standardise == TRUE, in which case we use the usual Cartesian coordinates with origin at the center of the image and scaled such that x varies between -.5 and .5, and a y arrow pointing up
##'
##' 
##' @param im an image
##' @param standardise If TRUE use a centered, scaled coordinate system. If FALSE use standard image coordinates (default FALSE)
##' @param drop.unused if TRUE ignore empty dimensions, if FALSE include them anyway (default TRUE)
##' @return a data.frame
##' @examples
##' im <- as.cimg(array(0,c(10,10))) #A 10x10 image
##' pixel.grid(im) %>% head
##' pixel.grid(im,standardise=TRUE) %>% head
##' pixel.grid(im,drop.unused=FALSE) %>% head
##' @export
pixel.grid <- function(im,standardise=FALSE,drop.unused=TRUE)
    {
        if (standardise)
            {
                dy <- height(im)/width(im)
                dz <- depth(im)/width(im)
                res <- expand.grid(x=seq(-.5,.5,l=width(im)),y=seq(dy/2,-dy/2,l=height(im)),z=seq(-dz/2,dz/2,l=depth(im)),cc=1:spectrum(im))
            }
        else
            {
                res <- expand.grid(x=1:width(im),y=1:height(im),z=1:depth(im),cc=1:spectrum(im))
            }
        if (drop.unused)
            {
                res[,dim(im) > 1]
            }
        else
            {
                res
            }
    }

##' Convert to cimg object
##'
##' Imager implements various converters that turn your data into cimg objects. If you convert from a vector (which only has a length, and no dimension), either specify dimensions explicitly or some guesswork will be involved. See examples for clarifications. 
##' 
##' @param obj an object
##' @param x width
##' @param y height
##' @param z depth
##' @param cc spectrum
##' @param ... optional arguments
##' @seealso as.cimg.array, as.cimg.function, as.cimg.data.frame
##' @export
##' @examples
##' as.cimg(1:100,x=10,y=10) #10x10, grayscale image
##' as.cimg(rep(1:100,3),x=10,y=10,cc=3) #10x10 RGB
##' as.cimg(1:100) #Guesses dimensions, warning is issued
##' as.cimg(rep(1:100,3)) #Guesses dimensions, warning is issued
##' @author Simon Barthelme
as.cimg <- function(obj,...) UseMethod("as.cimg")


##' @describeIn as.cimg convert numeric
##' @export
as.cimg.numeric <- function(obj,...) as.cimg.vector(obj,...)

##' @describeIn as.cimg convert double
##' @export
as.cimg.double <- function(obj,...) as.cimg.vector(obj,...)

##' @describeIn as.cimg convert vector
##' @export
as.cimg.vector <- function(obj,x=NA,y=NA,z=NA,cc=NA,...)
    {
        args <- list(x=x,y=y,z=z,cc=cc)
        if (any(!is.na(args)))
            {
                args[is.na(args)] <- 1
                d <- do.call("c",args)
                if (prod(d)==length(obj))
                    {
                        array(obj,d)%>% cimg
                    }
                else
                    {
                        stop("Dimensions are incompatible with input length")
                    }
            }
        else
            {
                l <- length(obj)
                is.whole <- function(v) isTRUE(all.equal(round(v), v))
                if (is.whole(sqrt(l)))
                    {
                        warning("Guessing input is a square 2D image")
                        d <- sqrt(l)
                        array(obj,c(d,d,1,1)) %>% cimg
                    }
                else if (is.whole(sqrt(l/3)))
                    {
                        warning("Guessing input is a square 2D RGB image")
                        d <- sqrt(l/3)
                        array(obj,c(d,d,1,3))%>% cimg
                    }
                else if (is.whole((l)^(1/3))) 
                    {
                        warning("Guessing input is a cubic 3D image")
                        d <- l^(1/3)
                        array(obj,c(d,d,d,1))%>% cimg
                    }
                else if (is.whole((l/3)^(1/3))) 
                    {
                        warning("Guessing input is a cubic 3D RGB image")
                        d <- (l/3)^(1/3)
                        array(obj,c(d,d,d,3))%>% cimg
                    }
                else
                    {
                        stop("Please provide image dimensions")
                    }
            }
    }


##' Create an image by sampling a function
##'
##' Similar to as.im.function from the spatstat package, but simpler. Creates a grid of pixel coordinates x=1:width,y=1:height and (optional) z=1:depth, and evaluates the input function at these values. 
##' 
##' @param obj a function with arguments (x,y) or (x,y,z). Must be vectorised. 
##' @param width width of the image (in pixels)
##' @param height height of the image (in pixels)
##' @param depth depth of the image (in pixels)
##' @param normalise.coord coordinates are normalised so that x,y,z are in (0,1) (default FALSE)
##' @param ... ignored
##' @return an object of class cimg
##' @author Simon Barthelme
##' @examples
##' im = as.cimg(function(x,y) cos(sin(x*y/100)),100,100)
##' plot(im)
##' im = as.cimg(function(x,y) cos(sin(x*y/100)),100,100,normalise.coord=TRUE)
##' plot(im)
##' @export
as.cimg.function <- function(obj,width,height,depth=1,normalise.coord=FALSE,...)
    {
        fun <- obj
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

##' Create an image of custom size by filling in repeated values
##'
##' This is a convenience function for quickly creating blank images, or images filled with a specific colour. See examples. 
##' 
##' @param x width (default 1)
##' @param y height (default 1)
##' @param z depth (default 1)
##' @param val fill-in values. Either a single value (for grayscale), or RGB values for colour
##' @return an image object (class cimg)
##' @examples
##' imfill(20,20) %>% plot #Blank image of size 20x20
##' imfill(20,20,val=c(1,0,0)) %>% plot #All red image
##' @author simon
##' @export
imfill <- function(x=1,y=1,z=1,val=0)
    {
        if (length(val) == 1)
            {
                array(val,c(x,y,z,1)) %>% cimg
            }
        else
            {
                llply(val,function(v) imfill(x,y,z,v)) %>% imappend("c")
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
##' @return a cimg object
##' @examples
##' imnoise(100,100,cc=3) %>% plot(main="White noise in RGB")
##' imnoise(100,100,cc=3) %>% isoblur(5) %>% plot(main="Filtered (non-white) noise")
##' @author Simon Barthelme
##' @export
imnoise <- function(x=1,y=1,z=1,cc=1,mean=0,sd=1)
    {
        rnorm(x*y*z*cc,mean=mean,sd=sd) %>% array(dim=c(x,y,z,cc)) %>% cimg
    }

##' Turn an numeric array into a cimg object
##'
##' If the array has two dimensions, we assume it's a grayscale image. If it has three dimensions we assume it's a video, unless the third dimension has a depth of 3, in which case we assume it's a colour image,
##' 
##' @export
##' @param obj an array
##' @param ... ignored
##' @examples
##' as.cimg(array(1:9,c(3,3)))
##' as.cimg(array(1,c(10,10,3))) #Guesses colour image
##' as.cimg(array(1:9,c(10,10,4))) #Guesses video
as.cimg.array <- function(obj,...)
    {
        d <- dim(obj)
        if (length(d)==4)
            {
                cimg(obj)
            }
        else if (length(d) == 2)
            {
                as.cimg.matrix(obj)
            }
        else if (length(d) == 3)
        {
            if (d[3] == 3)
                    {
                        warning('Assuming third dimension corresponds to colour')
                        dim(obj) <- c(d[1:2],1,d[3])
                        cimg(obj)
                    }
            else {
                warning('Assuming third dimension corresponds to time/depth')
                dim(obj) <- c(d,1)
                cimg(obj)
            }
        }
        else
            {
                stop("Array must have at most 4 dimensions ")
            }
    }

##' @export
as.array.cimg <- function(x,...) {
    class(x) <- "array"
    x
}

##' Remove empty dimensions from an array
##'
##' Works just like Matlab's squeeze function: if anything in dim(x) equals one the corresponding dimension is removed
##' 
##' @param x an array
##' @export
##' @examples
##' A <- array(1:9,c(3,1,3)) #3D array with one flat dimension
##' A %>% squeeze #flat dimension removed
squeeze <- function(x) {
    d <- dim(x)
    dim(x) <- d[d>1]
    x
}

##' @export
as.matrix.cimg <- function(x,...) {
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
##' @param im a grayscale image
##' @param simple if TRUE just stack three copies of the grayscale image, if FALSE treat the image as the L channel in an HSL representation. Default TRUE.
##' @return an image of class cimg
##' @author Simon Barthelme
##' @examples
##' grayscale(boats) #No more colour channels
##' add.colour(grayscale(boats)) #Image has depth = 3 (but contains only grays)
##' @export
add.colour <- function(im,simple=TRUE)
{
    if (spectrum(im)!=1) stop('Image already has colour channels')
    if (simple)
        {
            imappend(list(im,im,im),"c")
        }
    else
        {
            imappend(list(0*im,0*im,im),"c") %>% HSLtoRGB
        }
}

inda <- list('x'=1,'y'=2,'z'=3,'c'=4)

##' Pad image with n pixels along specified axis
##'
##' 
##' @param im the input image
##' @param nPix how many pixels to pad with 
##' @param axes which axes to pad along 
##' @param pos -1: prepend 0: center 1: append
##' @param val value to fill the padding with (default 0)
##' @return a padded image
##' @author Simon Barthelme
##' @examples
##' pad(boats,20,"xy") %>% plot
##' pad(boats,20,pos=-1,"xy") %>% plot
##' pad(boats,20,pos=1,"xy") %>% plot
##' @export
pad <- function(im,nPix,axes,pos=0,val=0)
    {
        if (nchar(axes) > 1)
            {
                im <- pad(im,nPix,str_sub(axes,2),pos,val)
                axes <- str_sub(axes,1,1)
            }
        if (pos==0)
            {
                d <- rep(1,4)
                d[inda[[axes]]] <- round(nPix/2)
                pdIm <- cimg(array(val,d))
                imappend(list(pdIm,im,pdIm),axes)
            }
        else if (pos == -1)
            {
                d <- rep(1,4)
                d[inda[[axes]]] <- nPix
                pdIm <- cimg(array(val,d))
                imappend(list(pdIm,im),axes)
            }
        else if (pos == 1)
            {
                d <- rep(1,4)
                d[inda[[axes]]] <- nPix
                pdIm <- cimg(array(val,d))
                imappend(list(im,pdIm),axes)
            }
        
    }


.onUnload <- function (libpath) {
  library.dynam.unload("imager", libpath)
}

##' @describeIn as.cimg
##' @export
as.cimg.matrix <- function(obj,...)
    {
        dim(obj) <- c(dim(obj),1,1)
        cimg(obj)
    }

##' Create an image from a data.frame
##'
##' The data frame must be of the form (x,y,value) or (x,y,z,value), or (x,y,z,cc,value). The coordinates must be valid image coordinates (i.e., positive integers). 
##' 
##' @param obj a data.frame
##' @param v.name name of the variable to extract pixel values from (default "value")
##' @param dims a vector of length 4 corresponding to image dimensions. If missing, a guess will be made.
##' @param ... ignored
##' @return an object of class cimg
##' @examples
##' #Create a data.frame with columns x,y and value
##' df <- expand.grid(x=1:10,y=1:10) %>% mutate(value=x*y)
##' #Convert to cimg object (2D, grayscale image of size 10*10
##' as.cimg(df,dims=c(10,10,1,1)) %>% plot
##' @author Simon Barthelme
##' @export
as.cimg.data.frame <- function(obj,v.name="value",dims,...)
    {
        which.v <- (names(obj) == v.name) %>% which
        col.coord <- (names(obj) %in% names.coords) %>% which
        coords <- names(obj)[col.coord]
        if (length(which.v) == 0)
            {
                sprintf("Variable %s is missing",v.name) %>% stop
            }
        if (any(sapply(obj[,-which.v],min) <= 0))
            {
                stop('Indices must be positive')
            }
        if (missing(dims))
            {
                warning('Guessing dimension from maximum coordinate values')
                dims <- rep(1,4)
                for (n in coords)
                    {
                        dims[index.coords[[n]]] <- max(obj[[n]])
                    }
            }
        im <- as.cimg(array(0,dims))
        ind <- pixel.index(im,obj[,col.coord])
        im[ind] <- obj[[v.name]]
        im
    }


##' Convert cimg to spatstat im object
##'
##' The spatstat library uses a different format for images, which have class "im". This utility converts a cimg object to an im object. spatstat im objects are limited to 2D grayscale images, so if the image has depth or spectrum > 1 a list is returned for the separate frames or channels (or both, in which case a list of lists is returned, with frames at the higher level and channels at the lower one).
##' 
##' @param img an image of class cimg
##' @param W a spatial window (see spatstat doc). Default NULL
##' @return an object of class im, or a list of objects of class im, or a list of lists of objects of class im
##' @author Simon Barthelme
##' @seealso im, as.im
##' @export
cimg2im <- function(img,W=NULL)
    {
        
        if (requireNamespace("spatstat",quietly=TRUE))
            {
                if (depth(img) > 1)
                    {
                        l <- ilply(img,axis="z",cimg2im,W=W)
                        l
                    }
                else if (spectrum(img) > 1)
                    {
                        l <- ilply(img,axis="c",cimg2im,W=W)
                        l
                    }
                else
                    {
                        imrotate(img,90) %>% as.array %>% squeeze %>% spatstat::as.im(W=W)
                    }
            }
        else
            {
                stop("The spatstat package is required")
            }
    }

##' Convert an image in spatstat format to an image in cimg format
##'
##' as.cimg.im is an alias for the same function
##' 
##' @param img a spatstat image
##' @return a cimg image
##' @author Simon Barthelme
##' @export
im2cimg <- function(img)
    {
        if (requireNamespace("spatstat",quietly=TRUE))
            {
                spatstat::as.matrix.im(img) %>% as.cimg %>% imrotate(-90)
            }
        else
            {
                stop("The spatstat package is required")
            }
    }

as.cimg.im <- im2cimg

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
        ## if (nrow(V) == 1) {
        ##     V <- as.vector(V)
        ##     names(V) <- c("x","y","z","cc")
        ##     V <- t(V)
        ## }
        ## else
        ##     {
                colnames(V) <- c("x","y","z","cc")
##            }                           
     
        
        as.data.frame(V+1)
    }

get.mask <- function(im,expr)
    {
        expr <- substitute(expr)
        df <- pixel.grid(im)
        width <- width(im)
        height <- height(im)
        depth <- depth(im)
        spectrum <- spectrum(im)
        cx <- width/2
        cy <- height/2
        cz <- depth/2
        eval(expr,df)
    }

##' Center stencil at a location
##' 
##' @param stencil a stencil (data.frame with coordinates dx,dy,dz,dc)
##' @param ... centering locations (e.g. x=4,y=2)
##' @examples
##' stencil <- data.frame(dx=seq(-2,2,1),dy=seq(-2,2,1))
##' center.stencil(stencil,x=10,y=20)
##' @export
center.stencil <- function(stencil,...)
    {
        coords <- list(...)
        nms <- names(coords)
        
        for (v in nms)
            {
                nv <- paste0("d",v)
                if (!is.null(stencil[[nv]]))
                    {
                        stencil[[v]] <- stencil[[nv]]+coords[[v]]
                    }
                else
                    {
                        stencil[[v]] <- coords[[v]]
                    }
            }
        nms <- names(stencil)
        stencil[,!stringr::str_detect(nms,"^d.")]
    }

##' Return pixel values in a neighbourhood defined by a stencil
##'
##' A stencil defines a neighbourhood in an image (for example, the four nearest neighbours in a 2d image). This function centers the stencil at a certain pixel and returns the values of the neighbourhing pixels.
##' @param im an image
##' @param stencil a data.frame with values dx,dy,[dz],[dcc] defining the neighbourhood
##' @param ... where to center, e.g. x = 100,y = 10,z=3,cc=1
##' @return pixel values in neighbourhood
##' @examples
##' #The following stencil defines a neighbourhood that
##' #includes the next pixel to the left (delta_x = -1) and the next pixel to the right (delta_x = 1)
##' stencil <- data.frame(dx=c(-1,1),dy=c(0,0))
##' im <- as.cimg(function(x,y) x+y,w=100,h=100)
##' get.stencil(im,stencil,x=50,y=50)
##'
##' #A larger neighbourhood that includes pixels upwards and
##' #downwards of center (delta_y = -1 and +1)
##' stencil <- stencil.cross()
##' im <- as.cimg(function(x,y) x,w=100,h=100)
##' get.stencil(im,stencil,x=5,y=50)
##' @author Simon Barthelme
##' @export
get.stencil <- function(im,stencil,...)
    {
        center.stencil(stencil,...) %>% pixel.index(im,.) %>% im[.]
    }

check.x <- function(im,x)
    {
        x[x < 1] <- NA
        x[x > width(im)] <- NA
        x
    }

check.y <- function(im,y)
    {
        y[y < 1] <- NA
        y[y > height(im)] <- NA
        y
    }

check.z <- function(im,z)
    {
        z[z < 1] <- NA
        z[z > depth(im)] <- NA
        z
    }

check.cc <- function(im,cc)
    {
        cc[cc < 1] <- NA
        cc[cc > spectrum(im)] <- NA
        cc
    }

##' A cross-shaped stencil 
##'
##' Returns a stencil corresponding to all nearest-neighbours of a pixel
##' @param z include neighbours along the z axis
##' @param cc include neighbours along the cc axis
##' @param origin include center pixel (default false)
##' @return a data.frame defining a stencil
##' @seealso get.stencil
##' @author Simon Barthelme
##' @export
stencil.cross <- function(z=FALSE,cc=FALSE,origin=FALSE)
    {
        if (z & cc)
            {
                A <- c()
                for (ind in 1:4)
                    {
                        B <- matrix(0,2,4)
                        B[,ind] <- c(-1,1)
                        A <- rbind(A,B)
                    }
                v <- as.data.frame(A)
                names(v) <- c("dx","dy","dz","dcc")
            }
        else if (z)
            {
                A <- matrix(c(-1,1,rep(0,4)),2,3)
                v <- as.data.frame(rbind(A,A[,c(2,1,3)],A[,c(3,2,1)]))
                names(v) <- c("dx","dy","dz")
            }
        else if (cc)
            {
                A <- matrix(c(-1,1,rep(0,4)),2,3)
                v <- as.data.frame(rbind(A,A[,c(2,1,3)],A[,c(3,2,1)]))
                names(v) <- c("dx","dy","dcc")

            }
        else
            {
                A <- matrix(c(-1,1,0,0),2,2)
                v <- as.data.frame(rbind(A,A[,2:1]))
                names(v) <- c("dx","dy")
            }
        if (origin) v <- rbind(0,v)
        v
    }

##' Generates a "dirac" image, i.e. with all values set to 0 except one.
##'
##' This small utility is useful to examine the impulse response of a filter
##' 
##' @param dims a vector of image dimensions, or an image whose dimensions will be used. If dims has length < 4 some guesswork will be used (see examples and ?as.cimg.array)
##' @param x where to put the dirac (x coordinate)
##' @param y y coordinate
##' @param z  z coordinate (default 1)
##' @param cc colour coordinate (default 1)
##' @return an image
##' @examples
##' #Explicit settings of all dimensions 
##' imdirac(c(50,50,1,1),20,20) 
##' imdirac(c(50,50),20,20) #Implicit
##' imdirac(c(50,50,3),20,20,cc=2) #RGB
##' imdirac(c(50,50,7),20,20,z=2) #50x50 video with 7 frames
##' #Impulse response of the blur filter
##' imdirac(c(50,50),20,20) %>% isoblur(sigma=2)  %>% plot
##' #Impulse response of the first-order Deriche filter
##' imdirac(c(50,50),20,20) %>% deriche(sigma=2,order=1,axis="x")  %>% plot
##' ##NOT RUN, interactive only
##' ##Impulse response of the blur filter in space-time
##' ##resp <- imdirac(c(50,50,100),x=25,y=25,z=50)  %>%  isoblur(16)
##' ###Normalise to 0...255 and play as video
##' ##renorm(resp) %>% play(normalise=FALSE)
##' @author Simon Barthelme
##' @export
imdirac <- function(dims,x,y,z=1,cc=1)
    {
        if (class(dims) == "cimg")
            {
                dims <- dim(dims)
            }
        else if (is.numeric(dims))
            {
                if (length(dims) == 2)
                    {
                        dims <- c(dims,1,1)
                    }
                if (length(dims) == 3)
                    {
                        if (dims[3] == 3)
                            {
                                warning('Guessing you want an RGB image')
                                dims <- c(dims[1:2],1,dims[3])
                            }
                        else
                            {
                                dims <- c(dims,1)
                            }
                    }
            }
        else
            {
                stop("dims should a vector of image dimensions or a cimg object")
            }
        A <- array(0,dims)
        A<-as.cimg(A)
        if(x>0 && y>0 && z>0 && cc>0) {
            A[x,y,z,cc] <- 1
            A
        }
        else{
            stop("dirac coordinates must be positive integers ")
        }
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

##' Threshold grayscale image 
##'
##' Thresholding corresponding to setting all values below a threshold to 0, all above to 1. If you call threshold with a string argument of the form "XX%" (e.g., "98%"), the threshold will be set at percentil XX. Computing quantiles is expensive for large images, so if approx == TRUE threshold will skip pixels if the total number of pixels is above 10,000
##'
##'
##' 
##' @param im the image
##' @param thr a threshold, either numeric, or a string with format "XX\%". In the case of the latter, XX will be interpreted as a percentile (set the lower XX\% of the pixels to 0, the rest to 1)
##' @param approx Skip pixels when computing quantiles in large images (default TRUE)
##' @return a thresholded image
##' @examples
##' im <- load.image(system.file('extdata/Leonardo_Birds.jpg',package='imager'))
##' grayscale(im) %>% threshold("15%") %>% plot
##' @author Simon Barthelme
##' @export
threshold <- function(im,thr,approx=TRUE)
    {
        if (is.character(thr))
            {
                regexp.num <- "\\d+(\\.\\d*)?|\\.\\d+"
                qt <- stringr::str_match(thr,regexp.num)[,1] %>% as.numeric
                n <- prod(dim(im))
                if (n > 1e3 & approx)
                    {
                        thr <- quantile(im[round(seq(1,n,l=1e3))],qt/100)
                    }
                else
                    {
                        thr <- quantile(im,qt/100)
                    }
            }
        a <- im > thr
        b <- im <= thr
        im[a] <- 1
        im[b] <- 0
        im
    }


##' Capture the current R plot device as a cimg image
##'
##' @return a cimg image corresponding to the contents of the current plotting window
##' @author Simon Barthelme
##' @examples
##' ##interactive only:
##' ##plot(1:10)
##' ###Make a plot of the plot
##' ##capture.plot() %>% plot 
##' @export
capture.plot <- function()
    {   
        rst <- dev.capture(native=FALSE)
        if(is.null(rst)){
          stop("dev.capture failed (no compatible device found)")
        }
        else{
            d <- dim(rst)
            v <- rst %>% col2rgb %>% t %>% as.numeric
            array(v,c(d,1,3)) %>% cimg %>% mirror("x") %>% imrotate(-90)
        }
    }


##' Compute image gradient 
##'
##' Light interface for get_gradient. Refer to get_gradient for details on the computation.
##' 
##' @param im an image of class cimg
##' @param axes direction along which to compute the gradient. Either a single character (e.g. "x"), or multiple characters (e.g. "xyz")
##' @param scheme numerical scheme (default '3')
##' @return an image or a list of images, depending on the value of "axes" 
##' @author Simon Barthelme
##' @export
##' @examples
##' grayscale(boats) %>% imgradient("x") %>% plot
##' imgradient(boats,"xy") #Returns a list 
imgradient <- function(im,axes,scheme=3)
    {
        gr <- get_gradient(im,axes,scheme)
        if (length(gr) == 1)
            {
                gr[[1]]
            }
        else
            {
                names(gr) <- str_split(axes,"")[[1]]
                gr
            }
    }

##' Image warping
##'
##' Image warping consists in remapping pixels, ie. you define a function 
##' M(x,y,z) -> (x',y',z')
##' that displaces pixel content from (x,y,z) to (x',y',z'). 
##' Actual implementations rely on either the forward transformation M, or the backward (inverse) transformation M^-1. 
##' In CImg the forward implementation will go through all source (x,y,z) pixels and "paint" the corresponding pixel at (x',y',z'). This will result in unpainted pixels in the output if M is expansive (for example in the case of a scaling M(x,y,z) = 5*(x,y,z)).
##' The backward implementation will go through every pixel in the destination image and look for ancestors in the source, meaning that every pixel will be painted.
##' There are two ways of specifying the map: absolute or relative coordinates. In absolute coordinates you specify M or M^-1 directly. In relative coordinates you specify an offset function D:
##' M(x,y) = (x,y) + D(x,y) (forward)
##' M^-1(x,y) = (x,y) - D(x,y) (backward)
##'
##' Note that 3D warps are possible as well.
##' The mapping should be specified via the "map" argument, see examples. 
##' 
##' @param im an image
##' @param map a function that takes (x,y) or (x,y,z) as arguments and returns a named list with members (x,y) or (x,y,z) 
##' @param direction "forward" or "backward" (default "forward")
##' @param coordinates "absolute" or "relative" (default "relative")
##' @param boundary boundary conditions: "dirichlet", "neumann", "periodic". Default "dirichlet"
##' @param interpolation "nearest", "linear", "cubic" (default "linear")
##' @return a warped image
##' @examples
##' im <- load.image(system.file('extdata/parrots.png',package='imager'))
##' #Shift image
##' map.shift <- function(x,y) list(x=x+10,y=y+30)
##' imwarp(im,map=map.shift) %>% plot
##' #Shift image (backward transform)
##' imwarp(im,map=map.shift,dir="backward") %>% plot
##'
##' #Shift using relative coordinates
##' map.rel <- function(x,y) list(x=10+0*x,y=30+0*y)
##' imwarp(im,map=map.rel,coordinates="relative") %>% plot
##'
##' #Scaling
##' map.scaling <- function(x,y) list(x=1.5*x,y=1.5*y)
##' imwarp(im,map=map.scaling) %>% plot #Note the holes
##' map.scaling.inv <- function(x,y) list(x=x/1.5,y=y/1.5)
##' imwarp(im,map=map.scaling.inv,dir="backward") %>% plot #No holes
##'
##' #Bending
##' map.bend.rel <- function(x,y) list(x=50*sin(y/10),y=0*y)
##' imwarp(im,map=map.bend.rel,coord="relative",dir="backward") %>% plot #No holes
##' @author Simon Barthelme
##' @seealso warp for direct access to the CImg function
##' @export
imwarp <- function(im,map,direction="forward",coordinates="absolute",boundary="dirichlet",interpolation="linear")
    {
        gr <- pixel.grid(im)
        args <- formals(map)%>%names
        if (length(args)==2)
            {
                out <- map(gr$x,gr$y)
            }
        else if (length(args)==3)
            {
                out <- map(gr$x,gr$y,gr$z)
            }
        else
            {
                stop("Map should be a function with arguments x,y or x,y,z")
            }
        wf <- llply(out,function(v) array(v,c(dim(im)[1:3],1))) %>% imappend("c")
        mode <- (direction=="forward")*2+(coordinates=="relative")
        warp(im,wf-1,mode=mode,interpolation=switch(interpolation,nearest=0,linear=1,cubic=2),boundary_conditions=switch(boundary,dirichlet=0,neumann=1,periodic=2))
    }

##' Apply function to each element of a list, then combine the result as an image by appending along specified axis
##'
##' This is just a shortcut for llply followed by imappend
##' @param lst a list
##' @param fun function to apply
##' @param axis which axis to append along (e.g. "c" for colour)
##' @param ... further arguments to be passed to fun
##' @examples
##' build.im <- function(size) as.cimg(function(x,y) (x+y)/size,size,size)
##' liply(c(10,50,100),build.im,"y") %>% plot
##' @export
liply <- function(lst,fun,axis,...)
    {
        llply(lst,fun,...) %>% imappend(axis=axis)
    }


##' Split an image along axis, apply function, return a list
##'
##' Shorthand for imsplit followed by llply
##' @param im image
##' @param axis axis for the split (e.g "c")
##' @param fun function to apply
##' @param ... extra arguments for function fun
##' @examples
##' parrots <- load.image(system.file('extdata/parrots.png',package='imager'))
##' ilply(parrots,"c",mean) #mean luminance per colour channel
##' @export
ilply <- function(im,axis,fun,...)
    {
        imsplit(im,axis) %>% llply(fun,...) 
    }
##' Split an image along axis, apply function, return a data.frame
##'
##' Shorthand for imsplit followed by ldply
##' @param im image
##' @param axis axis for the split (e.g "c")
##' @param fun function to apply
##' @param ... extra arguments to function fun
##' @examples
##' parrots <- load.image(system.file('extdata/parrots.png',package='imager'))
##' idply(parrots,"c",mean) #mean luminance per colour channel
##' @export
idply <- function(im,axis,fun,...)
    {
        imsplit(im,axis) %>% ldply(fun,...) 
    }

##' Split an image, apply function, recombine the results as an image
##'
##' This is just imsplit followed by llply followed by imappend
##' 
##' @param im image 
##' @param axis axis for the split (e.g "c")
##' @param fun function to apply
##' @param ... extra arguments to function fun
##' @examples
##' parrots <- load.image(system.file('extdata/parrots.png',package='imager'))
##' #Normalise colour channels separately, recombine
##' iiply(parrots,"c",function(v) (v-mean(v))/sd(v)) %>% plot 
##' 
##' @export
iiply <- function(im,axis,fun,...)
    {
        imsplit(im,axis) %>% llply(fun,...) %>% imappend(axis=axis)
    }

##' Split an image along a certain axis (producing a list)
##'
##' Use this if you need to process colour channels separately, or frames separately, or rows separately, etc. You can also use it to chop up an image into blocks.
##' 
##' @param im an image 
##' @param axis the axis along which to split (for example 'c')
##' @param nb number of objects to split into. 
##' if nb=-1 (the default) the maximum number of splits is used ie. split(im,"c") produces a list containing all individual colour channels
##' @seealso imappend (the reverse operation)
##' @examples
##' im <- as.cimg(function(x,y,z) x+y+z,10,10,5)
##' imsplit(im,"z") #Split along the z axis into a list with 5 elements
##' imsplit(im,"z",2) #Split along the z axis into two groups
##' imsplit(im,"z",2) %>% imappend("z") #Split and reshape into a single image
##' @export
imsplit <- function(im,axis,nb=-1)
    {
        l <- im_split(im,axis,nb)
        d.ind <- index.coords[[axis]]
        d <- dim(im)
        if (nb!=-1)
            {
                b.end <- laply(l,function(v) dim(v)[d.ind]) %>% cumsum
                b.start <- c(1,b.end[-length(l)]+1)
                b.str <- sprintf("= %i - %i",b.start,b.end)
                names(l) <- paste(axis,b.str)
            }
        else
            {
                names(l) <- paste(axis,1:length(l),sep=" = ")
            }
        l
    }


imsplit.recur <- function(im,spl,nb=-1)
    {
        if (length(spl) > 1)
            {
                imsplit.recur(im,spl[[1]]) %>% llply(imsplit.recur,spl=spl[-1])
            }
        else
            {
                l <- im_split(im,axis,nb)
                d.ind <- index.coords[[axis]]
                d <- dim(im)
                if (nb!=-1)
                    {
                        b.end <- laply(l,function(v) dim(v)[d.ind]) %>% cumsum
                        b.start <- c(1,b.end[-length(l)]+1)
                        b.str <- sprintf("= %i - %i",b.start,b.end)
                        names(l) <- paste(axis,b.str)
                    }
                else
                    {
                        names(l) <- paste(axis,1:length(l),sep=" = ")
                    }
                l
            }
    }


##' Compute the periodic part of an image, using the periodic/smooth decomposition of Moisan (2009)
##'
##' Moisan (2009) defines an additive image decomposition
##' im = periodic + smooth
##' where the periodic part shouldn't be too far from the original image. The periodic part can be used in frequency-domain analyses, to reduce the artifacts induced by non-periodicity.
##' 
##' @param im an image
##' @return an image
##' @examples
##' imname <- system.file('extdata/parrots.png',package='imager')
##' im <- load.image(imname) %>% subim(x <= 512)
##' layout(t(1:3))
##' plot(im,main="Original image")
##' periodic.part(im) %>% plot(main="Periodic part")
##' #The smooth error is the difference between
##' #the original image and its periodic part
##' (im-periodic.part(im)) %>% plot(main="Smooth part")
##' 
##' @references  L. Moisan, Periodic plus Smooth Image Decomposition,J. Math. Imaging Vision, vol. 39:2, pp. 161-179, 2011
##' @author Simon Barthelme
##' @export
periodic.part <- function(im)
    {
        if (spectrum(im) > 1)
            {
                iiply(im,"c",periodic.part)
            }
        else
            {
                periodic_part(im)
            }
        
    }


##' Compute the Discrete Fourier Transform of an image
##'
##' This function is equivalent to R's builtin fft, up to normalisation (R's version is unnormalised, this one is). It calls CImg's implementation.
##' Important note: FFT will compute a multidimensional Fast Fourier Transform, using as many dimensions as you have in the image, meaning that if you have a colour video, it will perform a 4D FFT. If you want to compute separate FFTs across channels, use imsplit.
##' 
##' @param im.real The real part of the input (an image)
##' @param im.imag The imaginary part (also an image. If missing, assume the signal is real). 
##' @param inverse If true compute the inverse FFT (default: FALSE)
##' @return a list with components "real" (an image) and "imag" (an image), corresponding to the real and imaginary parts of the transform
##' @examples
##' 
##' im <- as.cimg(function(x,y) sin(x/5)+cos(x/4)*sin(y/2),128,128)
##' ff <- FFT(im)
##' plot(ff$real,main="Real part of the transform")
##' plot(ff$imag,main="Imaginary part of the transform")
##' sqrt(ff$real^2+ff$imag^2) %>% plot(main="Power spectrum")
##' #Check that we do get our image back
##' check <- FFT(ff$real,ff$imag,inverse=TRUE)$real #Should be the same as original
##' mean((check-im)^2)
##'
##' @author Simon Barthelme
##' @export
FFT <- function(im.real,im.imag,inverse=FALSE)
    {
        if (missing(im.imag)) #Assume it's zero
            {
                FFT_realim(im.real,inverse=inverse)
            }
        else
            {
                FFT_complex(im.real,im.imag,inverse=inverse)
            }
    }


##' Resize image uniformly 
##'
##' Resize image by a single scale factor. For non-uniform scaling and a wider range of options, see resize. 
##'
##' @name resize_uniform
##' @param im an image
##' @param scale a scale factor
##' @return an image
##' @references
##' For double-scale, half-scale, triple-scale, etc. uses an anisotropic scaling algorithm described in: \url{http://scale2x.sourceforge.net/algorithm.html}.
##' @seealso resize
##' @examples
##' imname <- system.file('extdata/parrots.png',package='imager')
##' im <- load.image(imname)
##' imresize(im,1/4) #Quarter size
##' liply(2:4,function(ind) imresize(im,1/ind),"x") %>%  plot
##' @author Simon Barthelme
NULL

##' @describeIn resize_uniform resize by scale factor
##' @export
imresize <- function(im,scale=1)
    {
        
        if (depth(im) == 1 & ((1/scale)%%2)==0) #Half-size, quarter-size, etc.
            {
                nTimes <- -log2(scale)
                iterate(resize_halfXY,nTimes)(im)
            }
        else if (depth(im) == 1 & ((scale)%%2)==0) #Double-size, Quadruple-size, etc.
            {
                nTimes <- log2(scale)
                iterate(resize_doubleXY,nTimes)(im)
            }
        else if (scale == 3)
            {
                resize_tripleXY(im)
            }
        else
            {
                resize(im,-scale*100,-scale*100,-scale*100,interpolation_type=3)
            }
    }

#' Compute image hessian.
#' @param im an image
#' @param axes Axes considered for the hessian computation, as a character string (e.g "xy" corresponds to d/(dx*dy)). Can be a list of axes. Default: xx,xy,yy
#' @return an image, or a list of images
#' @examples
#' imhessian(boats,"xy") %>% plot(main="Second-derivative, d/(dx*dy)")
#' @export
imhessian <- function(im,axes=c("xx","xy","yy"))
    {
        pax <- paste0(axes,collapse="")
        if (length(axes)==1)
            {
                get_hessian(im,axes)[[1]]
            }
        else
            {
                l <- get_hessian(im,pax)
                names(l) <- axes
                l
            }
    }

##' Draw image on another image
##'
##' @param im background image
##' @param sprite sprite to draw on background image
##' @param x location 
##' @param y location 
##' @param z location 
##' @param opacity transparency level (default 1)
##' @author Simon Barthelme
##' @examples
##' imname <- system.file('extdata/parrots.png',package='imager')
##' im <- load.image(imname)
##' boats.small <- imresize(boats,.5)
##' #I'm aware the result is somewhat ugly 
##' imdraw(im,boats.small,x=400,y=10,opacity=.7) %>% plot
##' @export
##' @seealso imager.combine, for different ways of combining images
imdraw <- function(im,sprite,x=1,y=1,z=1,opacity=1)
    {
        if (spectrum(im) == 3 & (spectrum(sprite)==1))
            {
                warning("Converting sprite to colour image")
                sprite <- add.colour(sprite)
            }
        else if (spectrum(sprite) == 3 & (spectrum(im)==1))
            {
                warning("Converting image to colour")
                im <- add.colour(im)
            }
        if (spectrum(sprite) != spectrum(im))
            {
                stop("Image and sprite have incompatible spectra")
            }
        else
            {
                draw_image(im,sprite,x-1,y-1,z-1,opacity=opacity)
            }
    }

##' Combining images
##'
##' These functions take a list of images and combine them by adding, multiplying, taking the parallel min or max, etc.
##' @name imager.combine
##' @param x a list of images
##' @examples
##' im1 <- as.cimg(function(x,y) x,100,100)
##' im2 <- as.cimg(function(x,y) y,100,100)
##' im3 <- as.cimg(function(x,y) cos(x/10),100,100)
##' l <- list(im1,im2,im3)
##' add(l) %>% plot #Add the images
##' average(l) %>% plot #Average the images
##' mult(l) %>% plot #Multiply
##' parmax(l) %>% plot #Parallel max
##' parmin(l) %>% plot #Parallel min
##' #Pseudo-artistic effects
##' llply(seq(1,35,5),function(v) boxblur(boats,v)) %>% parmin %>% plot
##' llply(seq(1,35,5),function(v) boxblur(boats,v)) %>% average %>% plot
##' @author Simon Barthelme
##' @seealso imsplit,Reduce
NULL

##' @describeIn imager.combine
##' @export
add <- function(x) Reduce("+", x)

##' @describeIn imager.combine
##' @export
average <- function(x) Reduce("+", x)/length(x)

##' @describeIn imager.combine
##' @export
mult <- function(x) Reduce("*", x)

##' @describeIn imager.combine
##' @export
parmax <- function(x) Reduce(pmax, x)

##' @describeIn imager.combine
##' @export
parmin <- function(x) Reduce(pmin, x)


##' Renormalise image 
##'
##' Pixel data is usually expressed on a 0...255 scale for displaying. This function performs a linear renormalisation to range min...max 
##' @param x numeric data
##' @param min min of the range
##' @param max max of the range
##' @author Simon Barthelme
##' @export
##' @examples
##' renorm(0:10)
##' renorm(-5:5) #Same as above
renorm <- function(x,min=0,max=255)
    {
        r <- diff(range(x))
        if (r!=0)
            {
                min+(max-min)*(x-min(x))/diff(range(x))
            }
        else
            {
                if (x[1] > max)
                    {
                        x*0 + max
                    }
                else if (x[1] < min)
                    {
                        x*0 + min
                    }
                else
                    {
                        x
                    }
            }
    }
