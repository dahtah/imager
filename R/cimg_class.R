#' imager: an R library for image processing, based on CImg
#'
#' CImg by David Tschumperle is a C++ library for image processing. It provides most common functions for image manipulation and filtering, as well as some advanced algorithms. imager makes these functions accessible from R and adds many utilities for accessing and working with image data from R.
#' You should install ImageMagick if you want support for image formats beyond PNG and JPEG, and ffmpeg if you need to work with videos (in which case you probably also want to take a look at experimental package imagerstreams on github).
#' Package documentation is available at http://dahtah.github.io/imager/. 
#' @docType package
#' @name imager
NULL

#' @useDynLib imager, .registration=TRUE
#' @importFrom grDevices as.raster col2rgb dev.capture gray rgb contourLines xy.coords
#' @importFrom igraph as.igraph
#' @importFrom utils file_test
#' @importFrom graphics axis plot rasterImage layout lines plot.new plot.window title abline polygon
#' @importFrom stats quantile rnorm kmeans setNames
#' @importFrom purrr map map_dbl map_lgl map_df map2 map_int pmap reduce keep
#' @importFrom png readPNG writePNG
#' @importFrom jpeg writeJPEG readJPEG
#' @importFrom readbitmap read.bitmap
#' @importFrom methods is
#' @importFrom stringr str_match str_split str_sub
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr "%>%"
NULL

names.coords <- c('x','y','z','c','cc')
index.coords <- list("x"=1,"y"=2,"z"=3,"c"=4,"cc"=4)

## CRAN sometimes issues spurious warnings about undefined variables
utils::globalVariables(c(".", "%>%","x","y","z","cc","value"))

##' cimg is a class for storing image or video/hyperspectral data.  It is designed to provide easy interaction with the CImg library, but in order to use it you need to be aware of how CImg wants its image data stored. 
##' Images have up to 4 dimensions, labelled x,y,z,c. x and y are the usual spatial dimensions, z is a depth dimension (which would correspond to time in a movie), and c is a colour dimension. Images are stored linearly in that order, starting from the top-left pixel and going along *rows* (scanline order).
##' A colour image is just three R,G,B channels in succession. A sequence of N images is encoded as R1,R2,....,RN,G1,...,GN,B1,...,BN where R_i is the red channel of frame i.
##' The number of pixels along the x,y,z, and c axes is called (in that order), width, height, depth and spectrum. 
##' NB: Logical and integer values are automatically converted to type double. NAs are not supported by CImg, so you should manage them on the R end of things. 
##' @title Create a cimg object 
##' @param X a four-dimensional numeric array
##' @return an object of class cimg
##' @author Simon Barthelme
##' @examples
##' cimg(array(1,c(10,10,5,3)))
##' @export
cimg <- function(X)
    {
        if (is.logical(X) | is.integer(X))
            {
                X <- X+0.0
            }
        class(X) <-c("cimg","imager_array","numeric")
        X
    }

##' Checks that an object is a cimg object
##' @param x an object
##' @return logical
##' @export
is.cimg <- function(x) is(x,"cimg")


##' Various shortcuts for extracting colour channels, frames, etc
##'
##' @param im an image
##' @param index frame index 
##' @name cimg.extract
##' @examples
##' #Extract the red channel from the boats image, then the first row, plot
##' rw <- R(boats) %>% imrow(10)
##' plot(rw,type="l",xlab="x",ylab="Pixel value")
##' #Note that R(boats) returns an image
##' R(boats)
##' #while imrow returns a vector or a list
##' R(boats) %>% imrow(1) %>% str
##' imrow(boats,1) %>% str
##'
NULL

##' Colour space conversions in imager
##' 
##' All functions listed here assume the input image has three colour channels (spectrum(im) == 3)
##' @name imager.colourspaces
##' @param im an image
NULL

##' Display an image using base graphics
##'
##' If you want to control precisely how numerical values are turned into colours for plotting, you need to specify a colour scale using the colourscale argument (see examples). Otherwise the default is "gray" for grayscale images, "rgb" for colour. These expect values in [0..1], so the default is to rescale the data to [0..1]. If you wish to over-ride that behaviour, set rescale=FALSE.
##' See examples for an explanation.
##' If the image is one dimensional (i.e., a simple row or column image), then pixel values will be plotted as a line.
##' @param x the image 
##' @param frame which frame to display, if the image has depth > 1
##' @param rescale rescale pixel values so that their range is [0,1]
##' @param colourscale,colorscale an optional colour scale (default is gray or rgb)
##' @param xlim x plot limits (default: 1 to width)
##' @param ylim y plot limits (default: 1 to height)
##' @param xlab x axis label
##' @param ylab y axis label
##' @param interpolate should the image be plotted with antialiasing (default TRUE)
##' @param asp aspect ratio. The default value (1) means that the aspect ratio of the image will be kept regardless of the dimensions of the plot. A numeric value other than one changes the aspect ratio, but it will be kept the same regardless of dimensions. Setting asp="varying" means the aspect ratio will depend on plot dimensions (this used to be the default in versions of imager < 0.40)
##' @param xaxs The style of axis interval calculation to be used for the x-axis. See ?par
##' @param yaxs The style of axis interval calculation to be used for the y-axis. See ?par
##' @param axes Whether to draw axes (default TRUE)
##' @param col.na which colour to use for NA values, as R rgb code. The default is "rgb(0,0,0,0)", which corresponds to a fully transparent colour.
##' @param main Main title
##' @param ... other parameters to be passed to plot.default (eg "main")
##' @seealso display, which is much faster, as.raster, which converts images to R raster objects
##' @export
##' @examples
##' plot(boats,main="Boats") 
##' plot(boats,axes=FALSE,xlab="",ylab="")
##'
##' #Pixel values are rescaled to 0-1 by default, so that the following two plots are identical
##' plot(boats)
##' plot(boats/2,main="Rescaled")
##' #If you don't want that behaviour, you can set rescale to FALSE, but
##' #then you need to make sure values are in [0,1]
##' try(plot(boats,rescale=FALSE)) #Error!
##' try(plot(boats/255,rescale=FALSE)) #Works
##' #You can specify a colour scale if you don't want the default one.
##' #A colour scale is a function that takes pixels values and return an RGB code,
##' #like R's rgb function,e.g.
##' rgb(0,1,0)
##' #Let's switch colour channels
##' cscale <- function(r,g,b) rgb(b,g,r)
##' plot(boats/255,rescale=FALSE,colourscale=cscale)
##' #Display slice of HSV colour space
##' im <- imfill(255,255,val=1)
##' im <- list(Xc(im)/255,Yc(im)/255,im) %>% imappend("c")
##' plot(im,colourscale=hsv,rescale=FALSE,
##'      xlab="Hue",ylab="Saturation")
##' #In grayscale images, the colourscale function should take in a single value
##' #and return an RGB code
##' boats.gs <- grayscale(boats)
##' #We use an interpolation function from package scales
##' cscale <- scales::gradient_n_pal(c("red","purple","lightblue"),c(0,.5,1))
##' plot(boats.gs,rescale=FALSE,colourscale=cscale)
##' #Plot a one-dimensional image
##' imsub(boats,x==1) %>% plot(main="Image values along first column")
##' #Plotting with and without anti-aliasing:
##' boats.small <- imresize(boats,.3)
##' plot(boats.small,interp=TRUE)
##' plot(boats.small,interp=FALSE)
plot.cimg <- function(x,frame,xlim=c(1,width(x)),ylim=c(height(x),1),xlab="x",ylab="y",rescale=TRUE,colourscale=NULL,colorscale=NULL,interpolate=TRUE,axes=TRUE,main="",xaxs="i",yaxs="i",asp=1,col.na=rgb(0,0,0,0),...)
{
    if (nPix(x) == 0) stop("Empty image")
    im <- x
    if (depth(im) > 1)
    {
        if (missing(frame))
        {
            warning("Showing first frame")
            frame <- 1
        }
        im <- frame(x,frame)
    }
    if (1 %in% dim(im)[1:2]) #Image has a single spatial dimension
    {
        plot.singleton(im,...)
        }
    else
    {
        if (is.character(asp) && asp == "varying")
            {
                plot(1,1,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,type="n",xaxs=xaxs,yaxs=yaxs,axes=axes,...)
                as.raster(im,rescale=rescale,colorscale=colorscale,colourscale=colourscale,col.na=col.na) %>% rasterImage(1,height(im),width(im),1,interpolate=interpolate)
                title(main)
            }
        else if (is.numeric(asp))
        {
            plot.new()
            plot.window(xlim = xlim, ylim = ylim,asp=asp,xaxs=xaxs,yaxs=yaxs,...)
            rst <- as.raster(im,rescale=rescale,colorscale=colorscale,colourscale=colourscale,col.na=col.na)
            rasterImage(rst, 1, nrow(rst), ncol(rst), 1,interpolate=interpolate)
            title(main)
            if (axes) { axis(1); axis(2) }
        }
        else
        {
            stop("Invalid value for parameter asp")
        }
        
    }
    invisible(x)
}

#Plots one-dimensional images
plot.singleton <- function(im,...)
{
    varying <- if (width(im) == 1) "y" else "x"
    l <- max(dim(im)[1:2])
    if (spectrum(im) == 1)
    {
        plot(1:l,as.vector(im),xlab=varying,ylab="Pixel value",type="l",...)
    }
    else if (spectrum(im) ==3)
    {
        ylim <- range(im)
        
        plot(1:l,1:l,type="n",xlab=varying,ylim=ylim,ylab="Pixel value",...)
        cols <- c("red","green","blue")

        for (i in 1:3)
        {

            graphics::lines(1:l,as.vector(channel(im,i)),type="l",col=cols[i])
        }
    }
    else
    {
        stop("Unsupported image format")
    }
}

##' @export
print.cimg <- function(x,...)
    {
        d <- dim(x)
        msg <- sprintf("Image. Width: %i pix Height: %i pix Depth: %i Colour channels: %i \n",d[1],d[2],d[3],d[4])
        cat(msg)
        invisible(x)
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

##' Grayscale dimensions of image
##'
##' Shortcut, returns the dimensions of an image if it had only one colour channel
##' @param im an image
##' @return returns c(dim(im)[1:3],1)
##' @author Simon Barthelme
##' @examples
##' imnoise(dim=gsdim(boats))
##' @export
gsdim <- function(im)
{
    c(dim(im)[1:3],1)
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
        res <- imsplit(im[,,index,,drop=FALSE],"z")
        nm <- paste('d.',index,sep=".")
        names(res) <- nm
        if (drop)
            {
                res <- purrr::map(res,function(v)  as.array(v) %>% squeeze)
            }
        res
    }

##' Extract one frame out of a 4D image/video
##'
##' @describeIn cimg.extract Extract frame
##' @author Simon Barthelme
##' @export
frame <- function(im,index)
    {
        im[,,index,,drop=FALSE]
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
        res <- imsplit(im,"c")[index]
        nm <- paste('c',index,sep=".")
        names(res) <- nm
        if (drop)
            {
                res <- purrr::map(res,function(v) { as.array(v) %>% squeeze})
            }
        res
    }

##' @describeIn cimg.extract Extract a particular column from an image
##' @param x x coordinate of the row
##' @export
imcol <- function(im,x)
{
    if (x > width(im) | x < 1)
    {
        stop('Invalid index')
    }
    else if (depth(im) > 1)
    {
        frames(im) %>% purrr::map(function(v) imcol(v,x))
    }
    else if (spectrum(im) > 1)
    {
        channels(im) %>% purrr::map(function(v) imcol(v,x))
    }
    else
    {
        im[x,,1,1] %>% c
    }
}

##' @describeIn cimg.extract Extract a particular row from an image
##' @param y y coordinate of the row
##' @export
imrow <- function(im,y)
{
    if (y > height(im) | y < 1)
    {
        stop('Invalid index')
    }
    else if (depth(im) > 1)
    {
        frames(im) %>% purrr::map(function(v) imrow(v,y))
    }
    else if (spectrum(im) > 1)
    {
        channels(im) %>% purrr::map(function(v) imrow(v,y))
    }
    else
    {
        im[,y,1,1] %>% c
    }
}


##' @describeIn cimg.extract Extract an image channel
##' @param ind channel index
##' @export
channel <- function(im,ind)
    {
        im[,,,ind,drop=FALSE] 
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

##' Return or set pixel value at coordinates
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
##' at(im,1,2) <- 10
##' at(im,1,2)
##' @export
at <- function(im,x,y,z=1,cc=1)
    {
        as.array(im)[x,y,z,cc]
    }

##' @describeIn at set value of pixel at a location
##' @param value replacement
##' @export
`at<-` <- function(im,x,y,z=1,cc=1,value)
    {
        im[x,y,z,cc] <- value
        im
    }

##' @describeIn at return value of all colour channels at a location
##' @examples
##' color.at(boats,x=10,y=10)
##' @export
color.at <- function(im,x,y,z=1)
    {
        at(im,x,y,z,cc=1:spectrum(im))
    }

##' @describeIn at set value of all colour channels at a location
##' @examples
##' im <- boats
##' color.at(im,x=10,y=10) <- c(255,0,0)
##' #There should now be a red dot
##' imsub(im, x %inr% c(1,100), y %inr% c(1,100)) %>% plot
##' @export
`color.at<-` <- function(im,x,y,z=1,value)
    {
        at(im,x,y,z,cc=1:spectrum(im)) <- value
        im
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
##' imsub selects an image part based on coordinates: it allows you to select a subset of rows, columns, frames etc. Refer to the examples to see how it works
##'
##' subim is an alias defined for backward-compatibility.
##' 
##' @param im an image
##' @param ... various conditions defining a rectangular image region
##' @return an image with some parts cut out
##' @author Simon Barthelme
##' @examples
##' parrots <- load.example("parrots")
##' imsub(parrots,x < 30) #Only the first 30 columns
##' imsub(parrots,y < 30) #Only the first 30 rows
##' imsub(parrots,x < 30,y < 30) #First 30 columns and rows
##' imsub(parrots, sqrt(x) > 8) #Can use arbitrary expressions
##' imsub(parrots,x > height/2,y > width/2)  #height and width are defined based on the image
##' #Using the %inr% operator, which is like %in% but for a numerical range
##' all.equal(imsub(parrots,x %inr% c(1,10)),
##'   imsub(parrots,x >= 1,x <= 10))
##' imsub(parrots,cc==1) #Colour axis is "cc" not "c" here because "c" is an important R function
##' ##Not run
##' ##imsub(parrots,x+y==1)
##' ##can't have expressions involving interactions between variables (domain might not be square)
##' @export
imsub <- function(im,...)
    {
        l <- as.list(substitute(list(...))[-1])
        consts <- list(width=width(im),height=height(im),depth=depth(im),spectrum=spectrum(im))
        consts <- mutate_plyr(consts,cx=width/2,cy=height/2,cz=depth/2)
        env <- new.env(parent = parent.frame())
        Reduce(function(a,b) subs(a,b,consts,envir=env),l,init=im)
    }

##' @describeIn imsub alias for imsub 
##' @export
subim <- imsub

subs <- function(im,cl,consts,envir=parent.frame())
    {
        if (missing(consts))
            {
               consts <- list(width=width(im),height=height(im),depth=depth(im),spectrum=spectrum(im))
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
                consts[[vname]] <- 1:maxval
                inds <- eval(cl,list2env(consts,envir))
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


##' Add colour channels to a grayscale image or pixel set
##'
##' @param im a grayscale image
##' @param simple if TRUE just stack three copies of the grayscale image, if FALSE treat the image as the L channel in an HSL representation. Default TRUE. For pixel sets this option makes no sense and is ignored. 
##' @return an image of class cimg
##' @author Simon Barthelme
##' @examples
##' grayscale(boats) #No more colour channels
##' add.colour(grayscale(boats)) #Image has depth = 3 (but contains only grays)
##' @export
add.colour <- function(im,simple=TRUE)
{
    if (is.cimg(im))
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
    else if (is.pixset(im))
        {
            if (spectrum(im)!=1) stop('Image already has colour channels')
            rep(im,3) %>% array(dim=c(dim(im)[1:3],3)) %>% pixset
        }
}

##' @export
##' @describeIn add.colour Alias for add.colour
add.color <- function(im,simple=TRUE) add.colour(im,simple)

inda <- list('x'=1,'y'=2,'z'=3,'c'=4)

##' Pad image with n pixels along specified axis
##'
##' 
##' @param im the input image
##' @param nPix how many pixels to pad with 
##' @param axes which axes to pad along 
##' @param pos -1: prepend 0: center 1: append
##' @param val colour of the padded pixels (default 0 in all channels). Can be a string for colour images, e.g. "red", or "black". 
##' @return a padded image
##' @author Simon Barthelme
##' @examples
##' pad(boats,20,"xy") %>% plot
##' pad(boats,20,pos=-1,"xy") %>% plot
##' pad(boats,20,pos=1,"xy") %>% plot
##' pad(boats,20,pos=1,"xy",val="red") %>% plot
##' @export
pad <- function(im,nPix,axes,pos=0,val)
{
    if (missing(val))
    {
        val <- if (is.pixset(im)) FALSE else 0
        val <- rep(val,dim(im)[4])
    }
    if (is.character(val))
    {
        val <- col2rgb(val)[,1]/255
    }
    if (length(val) != spectrum(im))
    {
        stop("val must be the same length as the number of colour channels in the image")
    }
    if (nPix > 0)
    {
        if (nchar(axes) > 1)
        {
                    im <- pad(im,nPix,str_sub(axes,2),pos,val)
                    axes <- str_sub(axes,1,1)
        }
        d <- dim(im)
        if (pos==0)
        {
            d[inda[[axes]]] <- round(nPix/2)
            pdIm <- imfill(dim=d,val=val)
            imappend(list(pdIm,im,pdIm),axes)
        }
        else if (pos == -1)
        {
            d[inda[[axes]]] <- nPix
            pdIm <- imfill(dim=d,val=val)
            imappend(list(pdIm,im),axes)
        }
        else if (pos == 1)
        {
            d[inda[[axes]]] <- nPix
            pdIm <- imfill(dim=d,val=val)
            imappend(list(im,pdIm),axes)
        }
    }
    else
    {
        im
    }
}


.onUnload <- function (libpath) {
  library.dynam.unload("imager", libpath)
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





##' Replace part of an image with another
##'
##' These replacement functions let you modify part of an image (for example, only the red channel).
##' Note that cimg objects can also be treated as regular arrays and modified using the usual [] operator. 
##' @name imager.replace
##' @param x an image to be modified 
##' @param value the image to insert
##' @param ind an index
##' @seealso imdraw
##' @examples
##' boats.cp <- boats
##' #Set the green channel in the boats image to 0
##' G(boats.cp) <- 0
##' #Same thing, more verbose
##' channel(boats.cp,2) <- 0
##' #Replace the red channel with noise
##' R(boats.cp) <- imnoise(width(boats),height(boats))
##' #A new image with 5 frames
##' tmp <- imfill(10,10,5)
##' #Fill the third frame with noise
##' frame(tmp,3) <- imnoise(10,10)
NULL

##' @describeIn imager.replace Replace image channel
##' @export
`channel<-` <- function(x, ind,value) {
    if (ind <= spectrum(x))
    {
        if (length(value)==1)
        {
            x[,,,ind] <- value
            x
        }
        else
        {
            if (all(dim(value)[1:3] == dim(x)[1:3]) & spectrum(value) == 1)
            {
                x[,,,ind] <- value
                x
            }
            else
            {
                stop("Dimensions are incompatible")
            }
        }
    }
    else
    {
        spectrum(x) %>% sprintf('Image only has %i channel(s)',.) %>% stop()
    }
}


##' @describeIn imager.replace Replace red channel
##' @export
`R<-` <- function(x, value) {
    channel(x,1) <- value
    x
}


##' @describeIn imager.replace Replace green channel
##' @export
`G<-` <- function(x, value) {
    channel(x,2) <- value
    x
}

##' @describeIn imager.replace Replace blue channel
##' @export
`B<-` <- function(x, value) {
    channel(x,3) <- value
    x
}

##' @describeIn imager.replace Replace image frame
##' @export
`frame<-` <- function(x, ind,value) {
    if (ind <= depth(x))
    {
        if (length(value) == 1)
        {
            x[,,ind,] <- value
            x
        }
        else
        {
            imdraw(x,value,z=ind)
        }
    }
        else
        {
            depth(x) %>% sprintf('Image only has %i frame(s)',.) %>% stop()
        }
}

##' Array subset operator for cimg objects
##'
##' Internally cimg objects are 4D arrays (stored in x,y,z,c mode) but often one doesn't need all dimensions. This is the case for instance when working on grayscale images, which use only two. The array subset operator works like the regular array [] operator, but it won't force you to use all dimensions.
##' There are easier ways of accessing image data, for example imsub, channels, R, G, B, and the like. 
##' @param x an image (cimg object)
##' @param drop if true return an array, otherwise return an image object (default FALSE)
##' @param ... subsetting arguments
##' @name imager.subset
##' @seealso imsub, which provides a more convenient interface, autocrop, imdraw
##' @examples
##' im <- imfill(4,4)
##' dim(im) #4 dimensional, but the last two ones are singletons
##' im[,1,,] <- 1:4 #Assignment the standard way
##' im[,1] <- 1:4 #Shortcut
##' as.matrix(im)
##' im[1:2,]
##' dim(boats)
##' #Arguments will be recycled, as in normal array operations
##' boats[1:2,1:3,] <- imnoise(2,3) #The same noise array is replicated over the three channels
NULL

##' @export
`[.cimg` <- function(x,...) {
    args <- as.list(substitute(list(...)))[-1L];
    drop <- TRUE
    
    hasdrop <- ("drop"%in%names(args))
    if (hasdrop)
    {
        drop <- args$drop
    }
    l <- length(args) -hasdrop


        #Call default method for arrays
    if (l==1 | l ==4)
    {
        out <- NextMethod()
    }
    else if (l<=4)
    {
        d <- dim(x)
        
        ar <- list(1,1,1,1)
        nsd <- which(dim(x) > 1)
        if (l == length(nsd))
        {
            ar[nsd] <- args[1:length(nsd)]
            if (!drop) ar$drop <- FALSE
            out <- do.call('[',c(list(x),c(ar)))
        }
        else
        {
            stop('Ambiguous call to .subset')
        }
    }
    else
    {
        stop('Too many arguments')
    }
    if (hasdrop)
    {
        if (!args$drop)
        {
            cimg(out)
        }
    }
    else
    {
        out
    }
}

##' @export
`[<-.cimg` <- function(x,...,value) {
    args <- as.list(substitute(list(...)))[-1L];
    l <- length(args) 

    #Call default method for arrays
    if (l==1 | l ==4)
    {
            out <- NextMethod()
    }
    else if (l<=4)
    {
        d <- dim(x)
        
        ar <- list(1,1,1,1)
        nsd <- which(dim(x) > 1)
        if (l == length(nsd))
        {
            ar[nsd] <- args[1:length(nsd)]
            ar$value <- value
#            browser()
            out <- do.call('[<-',c(list(x),c(ar)))
        }
        else
        {
            stop('Ambiguous call to .subset')
        }
    }
    else
    {
        stop('Too many arguments')
    }
    cimg(out)
}

#' Display image using CImg library
#'
#' Press escape or close the window to exit.
#'
#' @param x an image (cimg object)
#' @param rescale if true pixel values are rescaled to [0-1] (default TRUE)
#' @param ... ignored
#' @export
#' @examples
#' ##Not run: interactive only 
#' ##display(boats,TRUE) #Normalisation on 
#' ##display(boats/2,TRUE) #Normalisation on, so same as above
#' ##display(boats,FALSE) #Normalisation off
#' ##display(boats/2,FALSE) #Normalisation off, so different from above
display.cimg <- function(x,...,rescale=TRUE)
{
    display_(x*255,rescale)
}

##' Display object using CImg library
##'
##' CImg has its own functions for fast, interactive image plotting. Use this if you get frustrated with slow rendering in RStudio.
##' @param x an image or a list of images
##' @param ... ignored
##' @seealso display.cimg, display.imlist
##' @export
display <- function (x, ...) {
   UseMethod("display", x)
 }
