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

names.coords <- c('x','y','z','c','cc')
index.coords <- list("x"=1,"y"=2,"z"=3,"c"=4,"cc"=4)

##' Create a cimg object 
##'
##' cimg is a class for storing image or video/hyperspectral data.  It is designed to provide easy interaction with the CImg library, but in order to use it you need to be aware of how CImg wants its image data stored. 
##' Images have up to 4 dimensions, labelled x,y,z,c. x and y are the usual spatial dimensions, z is a depth dimension (which would correspond to time in a movie), and c is a colour dimension. Images are stored linearly in that order, starting from the top-left pixel and going along *rows* (scanline order).
##' A colour image is just three R,G,B channels in succession. A sequence of N images is encoded as R1,R2,....,RN,G1,...,GN,B1,...,BN where R_i is the red channel of frame i.
##' The number of pixels along the x,y,z, and c axes is called (in that order), width, height, depth and spectrum. 
##' 
##' @title Create a cimg object 
##' @param X a four-dimensional numeric array
##' @return an object of class cimg
##' @author Simon Barthelme
##' @export
cimg <- function(X)
    {
        class(X) <-c("cimg","numeric")
        X
    }






##' Display an image using base graphics
##'
##' @param im the image 
##' @param frame which frame to display, if the image has depth > 1
##' @param rescale.color rescale channels so that the values are in [0,1] 
##' @param ... other parameters to be passed to plot.default (eg "main")
##' @seealso display, which is much faster
##' @export
plot.cimg <- function(im,frame,rescale.color=TRUE,...)
    {
        w <- width(im)
        h <- height(im)
        if (rescale.color & !all(im==0))  im <- (im-min(im))/diff(range(im))
        if (dim(im)[3] == 1) #Single image (depth=1)
            {
                
                ## dim(im) <- dim(im)[-3]
                ## if (dim(im)[3] == 1) #BW
                ##     {
                ##         dim(im) <- dim(im)[1:2]
                ##         im <- t(im)
                ##         class(im) <- "matrix"
                ##     }
                ## else{
                ##     im <- aperm(im,c(2,1,3))
                ##     class(im) <- "array"
                ## }
                
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



##' @export
width <- function(im) dim(im)[1]


##' @export
height <- function(im) dim(im)[2]


##' @export
spectrum <- function(im) dim(im)[4]


##' @export
depth <- function(im) dim(im)[3]


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
                res <- llply(res,. %>% as.array %>% squeeze)
            }
        res
    }

##' Extract one frame out of a 4D image/video
##'
##' @param im an image
##' @param index frame index 
##' @return an image (class cimg)
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

##' Extract an image channel
##' @param im an image (cimg object)
##' @export
channel <- function(im,ind)
    {
        im[,,,ind] 
    }

##' Extract red channel
##' @param im an image (cimg object)
##' @export
R <- function(im) { channel(im,1) }

##' Extract green channel
##' @param im an image (cimg object)
##' @export
G <- function(im) { channel(im,2) }

##' Extract blue
##' @param im an image (cimg object)
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
##' @param im 
##' @param ... 
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



##' Load image from file
##'
##' You'll need ImageMagick for formats other than PNG and JPEG. If the image is actually a video, you'll need ffmpeg.
##' 
##' @param file path to file
##' @return an object of class 'cimg'
##' @examples
##' fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') #Path to example file from package
##' im <- load.image(fpath)
##' plot(im)
##' @export
load.image <- function(file)
    {
        has.magick <- Sys.which("convert") %>% { length(.) > 0 }
        if (has.magick)
            {
                path.expand(file) %>% load_image
            }
        else
            {
                ftype <- str_match(file,"\\.(.+)$")[1,2]
                if (ftype == "png")
                    {
                        load.png(file)
                    }
                else if (ftype == "jpeg" | ftype == "jpg")
                    {
                        load.jpg(file)
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
        readPNG(file) %>% convert.im.fromPNG
    }

load.jpeg <- function(file)
    {
        readJPEG(file) %>% convert.im.fromPNG
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
                ftype <- str_match(file,"\\.(.+)$")[1,2]
                if (ftype == "png")
                    {
                        save.png(im,file)
                    }
                else if (ftype == "jpeg" | ftype == "jpg")
                    {
                        save.jpg(im,file)
                    }
                else
                    {
                        stop("Unsupported file format. Please convert to jpg/png or install image magick")
                    }
            }
    }

save.png <- function(im,file)
    {
        convert.im.toPNG(im) %>% writePNG(file) 
    }

save.jpeg <- function(im,file)
    {
        convert.im.toPNG(im) %>% writeJPEG(file)
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
##' @param standardise. If TRUE use a centered, scaled coordinate system. If FALSE use standard image coordinates (default FALSE)
##' @return a data.frame
##' @export
pixel.grid <- function(im,standardise=FALSE)
    {
        if (standardise)
            {
                dy <- height(im)/width(im)
                dz <- depth(im)/width(im)
                expand.grid(x=seq(-.5,.5,l=width(im)),y=seq(dy/2,-dy/2,l=height(im)),z=seq(-dz/2,dz/2,l=depth(im)),cc=1:spectrum(im))
            }
        else
            {
                expand.grid(x=1:width(im),y=1:height(im),z=1:depth(im),cc=1:spectrum(im))
            }
    }

##' @export
as.cimg <- function(x,...) UseMethod("as.cimg")


##' Create an image by sampling a function
##'
##' Similar to as.im.function from the spatstat package, but simpler. Creates a grid of pixel coordinates x=1:width,y=1:height and (optional) z=1:depth, and evaluate the input function at these values. 
##' 
##' @param fun a function with arguments (x,y) or (x,y,z). Must be vectorised. 
##' @param width width of the image (in pixels)
##' @param height height of the image (in pixels)
##' @param depth depth of the image (in pixels)
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

##' Turn an numeric array into a cimg object
##'
##' If the array has two dimensions, we assume it's a grayscale image. If it has three dimensions we assume it's a video, unless the third dimension has a depth of 3, in which case we assume it's a colour image,
##' 
##' @export
##' @param X an array
as.cimg.array <- function(X)
    {
        d <- dim(X)
        if (length(d)==4)
            {
                cimg(X)
            }
        else if (length(d) == 2)
            {
                as.cimg.matrix(X)
            }
        else if (length(d) == 3)
        {
            if (d[3] == 3)
                    {
                        warning('Assuming third dimension corresponds to colour')
                        dim(X) <- c(d[1:2],1,d[3])
                        cimg(X)
                    }
            else {
                warning('Assuming third dimension corresponds to time/depth')
                dim(X) <- c(d,1)
                cimg(X)
            }
        }
        else
            {
                stop("Array must have at most 4 dimensions ")
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
##' @param im a grayscale image
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

##' @export
as.cimg.matrix <- function(X)
    {
        dim(X) <- c(dim(X),1,1)
        cimg(X)
    }

##' Create an image from a data.frame
##'
##' The data frame must be of the form (x,y,value) or (x,y,z,value), or (x,y,z,cc,value). The coordinates must be valid image coordinates (i.e., positive integers). 
##' 
##' @param df a data.frame
##' @param v.name name of the variable to extract pixel values from (default "value")
##' @param dims a vector of length 4 corresponding to image dimensions. If missing, a guess will be made. 
##' @return an object of class cimg
##' @author Simon Barthelme
##' @export
as.cimg.data.frame <- function(df,v.name="value",dims)
    {
        which.v <- (names(df) == v.name) %>% which
        col.coord <- (names(df) %in% names.coords) %>% which
        coords <- names(df)[col.coord]
        if (length(which.v) == 0)
            {
                sprintf("Variable %s is missing",v.name) %>% stop
            }
        if (any(sapply(df[,-which.v],min) <= 0))
            {
                stop('Indices must be positive')
            }
        if (missing(dims))
            {
                warning('Guessing dimension from maximum coordinate values')
                dims <- rep(1,4)
                for (n in coords)
                    {
                        dims[index.coords[[n]]] <- max(df[[n]])
                    }
            }
        im <- as.cimg(array(0,dims))
        ind <- pixel.index(im,df[,col.coord])
        im[ind] <- df[[v.name]]
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
        require(spatstat)
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
                imrotate(img,90) %>% as.array %>% squeeze %>% as.im(W=W)
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
        require(spatstat)
        as.matrix(img) %>% as.cimg %>% imrotate(-90) 
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
##' @author Simon Barthelmé
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
        if (nrow(V) == 1) {
            V <- as.vector(V)
            names(V) <- c("x","y","z","cc")
        }
        else
            {
                colnames(V) <- c("x","y","z","cc")
            }
        
        
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
        stencil[,!str_detect(nms,"^d.")]
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
##' @param dims a vector of image dimensions, or an image whose dimensions will be used 
##' @param x where to put the dirac (x coordinate)
##' @param y y coordinate
##' @param z  z coordinate (default 1)
##' @param cc colour coordinate (default 1)
##' @return an image
##' @examples
##' #Impulse response of the blur filter
##' imdirac(c(50,50,1,1),20,20) %>% isoblur(sigma=2)  %>% plot
##' #Impulse response of the first-order Deriche filter
##' imdirac(c(50,50,1,1),20,20) %>% deriche(sigma=2,order=1,axis="x")  %>% plot
##' @author Simon Barthelme
##' @export
imdirac <- function(dims,x,y,z=1,cc=1)
    {
        if (class(dims) == "cimg")
            {
                dims <- dim(dims)
        }
        
  A <- array(0,dims)
  A<-as.cimg(A)
  if(x>0 && y>0 && z>0 && cc>0) {
  A[x,y,z,cc] <- 1
  A}
  else{
    stop("dirac coordonates must be positive integers ")
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
##' Thresholding corresponding to setting all values below a threshold to 0, all above to 1. 
##' 
##' @param im the image
##' @param thr a threshold, either numeric, or a string with format "XX\%". In the case of the latter, XX will be interpreted as a percentile (set the lower XX\% of the pixels to 0, the rest to 1)
##' @return a thresholded image
##' @examples
##' im <- load.image(system.file('extdata/Leonardo_Birds.jpg',package='imager'))
##' grayscale(im) %>% threshold("15%") %>% plot
##' @author Simon Barthelme
##' @export
threshold <- function(im,thr)
    {
        if (is.character(thr))
            {
                qt <- str_match(thr,"(\\d+)%")[,2] %>% as.numeric
                thr <- quantile(im,qt/100)
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
          stop("capture.plot does not work when no R device is openned")
        }
        else{
        d <- dim(rst)
        v <- rst %>% col2rgb %>% t %>% as.numeric
<<<<<<< HEAD
        array(v,c(d,1,3)) %>% cimg %>% mirror("x") %>% imrotate(-90)
=======
        array(v,c(d,1,3)) %>% cimg %>% mirror("x") %>% rotate(-90)}
>>>>>>> 797518a77d857371e64f083fd29d76379bdaa006
    }

##' Compute image gradient 
##'
##' Light interface for get_gradient. Refer to get_gradient for details on the computation.
##' 
##' @param im an image of class cimg
##' @param axes: direction along which to compute the gradient. Either a single character (e.g. "x"), or multiple characters (e.g. "xyz")
##' @param scheme numerical scheme (default '3')
##' @return an image or a list of images, depending on the value of "axes" 
##' @author Simon Barthelmé
##' @export
imgradient <- function(im,axes,scheme=3)
    {
        gr <- get_gradient(im,axes,scheme)
        if (length(gr) == 1)
            {
                gr[[1]]
            }
        else
            {
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
        warp(im,wf-1,mode=mode,interpolation=switch(interpolation,nearest=0,linear=1,cubic=2),boundary=switch(boundary,dirichlet=0,neumann=1,periodic=2))
    }

##' Apply function to each element of a list, then combine the result as an image by appending along specified axis
##'
##' This is just a shortcut for llply followed by imappend
##' @param lst a list
##' @param axis which axis to append along (e.g. "c" for colour)
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


##' Compute the periodic part of an image, using the periodic/smooth decomposition of Moisan (2009)
##'
##' Moisan (2009) defines an additive image decomposition
##' im = periodic + smooth
##' where the periodic part shouldn't be too far from the original image. The periodic part can be used in frequency-domain analyses, to reduce the artifacts induced by non-periodicity.
##' 
##' @param im an image
##' @return an image
##' @examples
##' im <- load.image(system.file('extdata/parrots.png',package='imager')) %>% subim(x <= 512)
##' layout(t(1:3))
##' plot(im,main="Original image")
##' periodic.part(im) %>% plot(main="Periodic part")
##' #The smooth error is the difference between the original image and its periodic part
##' (im-periodic.part(im)) %>% plot(main="Smooth part")
##' 
##' @references  L. Moisan, Periodic plus Smooth Image Decomposition,J. Math. Imaging Vision, vol. 39:2, pp. 161-179, 2011
##' @author Simon Barthelmé
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
