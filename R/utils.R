#Misc. utilities

##' Compute the periodic part of an image, using the periodic/smooth decomposition of Moisan (2011)
##'
##' Moisan (2011) defines an additive image decomposition
##' im = periodic + smooth
##' where the periodic part shouldn't be too far from the original image. The periodic part can be used in frequency-domain analyses, to reduce the artifacts induced by non-periodicity.
##'
##' @param im an image
##' @return an image
##' @examples
##' im <- load.example("parrots") %>% subim(x <= 512)
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
##' @param interpolation interpolation method to use (see doc for resize). Default 3, linear. Set to 5 for cubic, 6 for Lanczos (higher quality).
##' @return an image
##' @references
##' For double-scale, triple-scale, etc. uses an anisotropic scaling algorithm described in: \url{http://scale2x.sourceforge.net/algorithm.html}. For half-scaling uses what the CImg doc describes as an "optimised filter", see resize_halfXY in CImg.h.
##' @seealso resize
##' @examples
##' im <- load.example("parrots")
##' imresize(im,1/4) #Quarter size
##' map_il(2:4,~ imresize(im,1/.)) %>% imappend("x") %>% plot
##' @author Simon Barthelme
NULL

##' @describeIn resize_uniform resize by scale factor
##' @export
imresize <- function(im,scale=1,interpolation=3)
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
                scale.z <- if (depth(im) > 1) -scale*100 else -100
                resize(im,-scale*100,-scale*100,scale.z,interpolation_type=interpolation)
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
##' im <- load.example("parrots")
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
        rg <- range(x,na.rm=TRUE)
        ## if (any(!is.finite(rg)))
        ##     {
        ##         stop('Image contains non-finite values or NAs')
        ##     }
        dr <- diff(rg)
        if (dr!=0)
            {
                min+(max-min)*(x-rg[1])/dr
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


iterate <- function(f,k)
    {
        function(x)
            {
                for (ind in 1:k)
                    {
                        x <- f(x)
                    }
                x
            }
    }

##' Compute image gradient
##'
##' Light interface for get_gradient. Refer to get_gradient for details on the computation.
##'
##' @param im an image of class cimg
##' @param axes direction along which to compute the gradient. Either a single character (e.g. "x"), or multiple characters (e.g. "xyz"). Default: "xy"
##' @param scheme numerical scheme (default '3', rotation invariant)
##' @return an image or a list of images, depending on the value of "axes"
##' @author Simon Barthelme
##' @export
##' @examples
##' grayscale(boats) %>% imgradient("x") %>% plot
##' imgradient(boats,"xy") #Returns a list
imgradient <- function(im,axes="xy",scheme=3)
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
##' im <- load.example("parrots")
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
        wf <- purrr::map(out,function(v) array(v,c(dim(im)[1:3],1))) %>% { purrr::map(.,as.cimg) } %>% imappend("c")
        mode <- (direction=="forward")*2+(coordinates=="relative")
        warp(im,wf,mode=mode,interpolation=switch(interpolation,nearest=0,linear=1,cubic=2),boundary_conditions=switch(boundary,dirichlet=0,neumann=1,periodic=2))
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



##' Threshold grayscale image
##'
##' Thresholding corresponding to setting all values below a threshold to 0, all above to 1.
##' If you call threshold with thr="auto" a threshold will be computed automatically using kmeans (ie., using a variant of Otsu's method).
##' This works well if the pixel values have a clear bimodal distribution. If you call threshold with a string argument of the form "XX\%" (e.g., "98\%"), the threshold will be set at percentile XX.
##' Computing quantiles or running kmeans is expensive for large images, so if approx == TRUE threshold will skip pixels if the total number of pixels is above 10,000. Note that thresholding a colour image will threshold all the colour channels jointly, which may not be the desired behaviour! Use iiply(im,"c",threshold) to find optimal values for each channel separately.
##'
##' @param im the image
##' @param thr a threshold, either numeric, or "auto", or a string for quantiles
##' @param approx Skip pixels when computing quantiles in large images (default TRUE)
##' @param adjust use to adjust the automatic threshold: if the auto-threshold is at k, effective threshold will be at adjust*k (default 1)
##' @return a pixset with the selected pixels
##' @examples
##' im <- load.example("birds")
##' im.g <- grayscale(im)
##' threshold(im.g,"15%") %>% plot
##' threshold(im.g,"auto") %>% plot
##' threshold(im.g,.1) %>% plot
##' #If auto-threshold is too high, adjust downwards or upwards
##' #using "adjust"
##' threshold(im,adjust=.5) %>% plot
##' threshold(im,adjust=1.3) %>% plot
##' @author Simon Barthelme
##' @export
threshold <- function(im,thr="auto",approx=TRUE,adjust=1)
    {
        if (is.character(thr))
        {
            if (nPix(im) > 1e3 & approx)
            {
                v <- im[round(seq(1,nPix(im),l=1e3))]
            }
            else
            {
                v <- im
            }

            if (thr=="auto")
            {
                thr <- cut.kmeans(c(v))*adjust
            }
                else
                {
                    regexp.num <- "\\d+(\\.\\d*)?|\\.\\d+"
                    qt <- stringr::str_match(thr,regexp.num)[,1] %>% as.numeric
                    thr <- quantile(v,qt/100)
                }
        }
        a <- im > thr
        attr(a,"threshold") <- thr
        a
    }

#Find a cut-off point for a bimodal distribution using kmeans (similar to Otsu's method)
cut.kmeans <- function(x)
{
    km <- kmeans(x,2)
    m <- which.min(km$centers)
    max(x[km$cluster==m])
}

##' Return information on image file
##'
##' This function calls ImageMagick's "identify" utility on an image file to get some information. You need ImageMagick on your path for this to work.
##' @param fname path to a file
##' @return a list with fields name, format, width (pix.), height (pix.), size (bytes)
##' @author Simon Barthelme
##' @examples
##' \dontrun{
##' someFiles <- dir("*.png") #Find all PNGs in directory
##' iminfo(someFiles[1])
##' #Get info on all files, as a data frame
##' info <- purrr::map_df(someFiles,function(v) iminfo(v) %>% as.data.frame)
##'}
##' @export
iminfo <- function(fname)
{
    if (!is.character(fname))
    {
        stop('Please provide a filename')
    }
    else if (!file.exists(fname))
    {
        stop('File does not exist')
    }

    if (has.magick())
    {
        cmd <- "identify -format  \"%f;%m;%w;%h;%b  \""
        fname <- paste0("\"",fname,"\"")
        out <- try(system(paste(cmd,fname),intern=TRUE))
        if (class(out) != "try-error")
        {
            if (length(out) > 0)
            {
                dat <- stringr::str_trim(out) %>% stringr::str_split(";")
                dat <- dat[[1]]
                names(dat) <- c("name","format","width","height","size")
                size <- NULL #pointless, only here to make CRAN happy
                dplyr::mutate(as.list(dat),width=as.numeric(width),height=as.numeric(height),size=as.numeric(stringr::str_sub(size,end=-2)))
            }
            else
            {
                warning("identify failed")
                NULL
            }
        }
        else
        {
            browser()
            NULL
        }
    }
    else
    {
        stop("You don't appear to have ImageMagick on your path. Please install")
    }
}


##' Crop the outer margins of an image
##'
##' This function crops pixels on each side of an image. This function is a kind of inverse (centred) padding, and is useful e.g. when you want to get only the valid part of a convolution
##' @param im an image
##' @param nx number of pixels to crop along horizontal axis
##' @param ny number of pixels to crop along vertical axis
##' @param nz number of pixels to crop along depth axis
##' @param nPix optional: crop the same number of pixels along all dimensions
##' @return an image
##' @author Simon Barthelme
##' @examples
##' #These two versions are equivalent
##' imfill(10,10) %>% crop.borders(nx=1,ny=1)
##' imfill(10,10) %>% crop.borders(nPix=1)
##'
##' #Filter, keep valid part
##' correlate(boats,imfill(3,3)) %>% crop.borders(nPix=2)
##' @export
crop.borders <- function(im,nx=0,ny=0,nz=0,nPix)
{
    if (!missing(nPix))
    {
        nx <- nPix
        ny <- nPix
        if (depth(im) > 1) nz <- nPix
    }
    if (depth(im) > 1)
    {
        imsub(im,(x > nx) & (x <= width - nx),(y > ny) & (y <= height - ny),(z > nz) & (z <= depth - nz))
    }
    else
    {
        imsub(im,(x > nx) & (x <= width - nx),(y > ny) & (y <= height - ny))
    }
}


patchmatch <- function(im1,im2,sx=1,sy=1,sz=1,nIter=10,nRad=10,occPenal,init)
{
    if (missing(init)) #Initialise to identity mapping
    {
        if (depth(im1)==1)
        {
            init <- list(Xc(im1),Yc(im1)) %>% imappend("c")
        }
        else
        {
            init <- list(Xc(im1),Yc(im1),Zc(im1)) %>% imappend("c")
        }
    }
    do_patchmatch(im1,im2,sx,sy,sz,nIter,nRad,occPenal,init)
}

#' Return image patch summary
#'
#' Patches are rectangular image regions centered at cx,cy with width wx and height wy. This function provides a fast way of extracting a statistic over image patches (for example, their mean).
#' Supported functions: sum,mean,min,max,median,var,sd, or any valid CImg expression.
#' WARNINGS:
#' - values outside of the image region are considered to be 0.
#' - widths and heights should be odd integers (they're rounded up otherwise).
#' @param im an image
#' @param expr statistic to extract. a string, either one of the usual statistics like "mean","median", or a CImg expression.
#' @param cx vector of x coordinates for patch centers
#' @param cy vector of y coordinates for patch centers
#' @param wx vector of patch widths (or single value)
#' @param wy vector of patch heights (or single value)
#' @return a numeric vector
#' @examples
#' im <- grayscale(boats)
#' #Mean of an image patch centered at (10,10) of size 3x3
#' patchstat(im,'mean',10,10,3,3)
#' #Mean of image patches centered at (10,10) and (20,4) of size 2x2
#' patchstat(im,'mean',c(10,20),c(10,4),5,5)
#' #Sample 10 random positions
#' ptch <- pixel.grid(im) %>% dplyr::sample_n(10)
#' #Compute median patch value
#' with(ptch,patchstat(im,'median',x,y,3,3))
#' @export
#' @seealso extract_patches
patchstat <- function(im,expr,cx,cy,wx,wy)
    {
        expr.predef <- c('sum','mean','min','max','median','var','sd')
        cx <- cx-1
        cy <- cy-1
        if (length(cx) != length(cy))
            {
                stop('cx and cy must have equal length')
            }
        if (length(cx) != length(wx))
            {
                if (length(wx) == 1)
                {
                    wx <- rep(wx,length(cx))
                }
                else
                {
                    stop('cx and wx have incompatible lengths')
                }
            }
        if (length(cy) != length(wy))
            {
                if (length(wy) == 1)
                {
                    wy <- rep(wy,length(cy))
                }
                else
                {
                    stop('cy and wy have incompatible lengths')
                }
            }
        if (expr %in% expr.predef)
            {
                extract_fast(im,which(expr==expr.predef)-1,cx,cy,wx,wy)
            }
        else
            {
                patch_summary_cimg(im,expr,cx,cy,wx,wy)
            }
    }

##' Check that value is in a range
##'
##' A shortcut for x >= a | x <= b.
##' @param x numeric values
##' @param range a vector of length two, of the form c(a,b)
##' @return a vector of logicals
##' 1:10 %inr% c(0,5)
##' @author Simon Barthelme
##' @export
`%inr%` <- function(x,range)
{
    if (!is.numeric(range) || length(range) != 2)
    {
        stop("Range must be a vector of 2 numeric values")
    }
    if (!is.numeric(x))
    {
        stop("x must be numeric")
    }
    else
    {
        if (diff(range) < 0)
        {
            stop('Range must be increasing')
        }

        x >= range[1] & x <= range[2]
    }
}

#' Autocrop image region
#'
#' @param im an image
#' @param color Colour used for the crop. If missing, the colour is taken from the top-left pixel. Can also be a colour name (e.g. "red", or "black")
#' @param axes Axes used for the crop.
#' @export
#' @examples
#' #Add pointless padding
#' padded <- pad(boats,30,"xy")
#' plot(padded)
#' #Remove padding
#' autocrop(padded) %>% plot
#' #You can specify the colour if needs be
#'autocrop(padded,"black") %>% plot
#' #autocrop has a zero-tolerance policy: if a pixel value is slightly different from the one you gave
#' #the pixel won't get cropped. A fix is to do a bucket fill first
#' padded <- isoblur(padded,10)
#' autocrop(padded) %>% plot
#' padded2 <- bucketfill(padded,1,1,col=c(0,0,0),sigma=.1)
#' autocrop(padded2) %>% plot
#'
autocrop <- function(im,color=color.at(im,1,1),axes="zyx")
{
    if (is.character(color))
    {
        color <- col2rgb(color)[,1]/255
    }
    autocrop_(im,color,axes)
}

##' Control CImg's parallelisation
##'
##' On supported architectures CImg can parallelise many operations using OpenMP.
##' Use this function to turn parallelisation on or off.
##'
##' @param mode Either "adaptive","always" or "none". The default is adaptive (parallelisation for large images only).
##' @return NULL (function is used for side effects)
##' @author Simon Barthelme
##' @examples
##' cimg.use.openmp("never") #turn off parallelisation
##' @export
cimg.use.openmp <- function(mode="adaptive")
{
    if (mode=="never")
        {
            set_cimg_omp(0)
        }
    else if (mode=="always")
        {
            set_cimg_omp(1)
        }
    else if (mode=="adaptive")
        {
            set_cimg_omp(2)
        }
    else
        {
            stop("Unknown mode, should be one of 'never','adaptive', or 'always'")
        }
    NULL
}

##' Return contours of image/pixset
##'
##' This is just a light interface over contourLines. See help for contourLines for details.
##' If the image has more than one colour channel, return a list with the contour lines in each channel.
##' Does not work on 3D images.
##' @param x an image or pixset
##' @param nlevels number of contour levels. For pixsets this can only equal two.
##' @param ... extra parameters passed to contourLines
##' @return a list of contours
##' @author Simon Barthelme
##' @seealso highlight
##' @examples
##' boats.gs <- grayscale(boats)
##' ct <- contours(boats.gs,nlevels=3)
##' plot(boats.gs)
##' #Add contour lines
##' purrr::walk(ct,function(v) lines(v$x,v$y,col="red"))
##' #Contours of a pixel set
##' px <- boats.gs > .8
##' plot(boats.gs)
##' ct <- contours(px)
##' #Highlight pixset
##' purrr::walk(ct,function(v) lines(v$x,v$y,col="red"))
##' @export
contours <- function(x,nlevels, ...) {
   UseMethod("contours", x)
 }

##' @export
contours.cimg <- function(x,nlevels=10,...)
{
    if (spectrum(x) > 1)
    {
        channels(x) %>% map(function(v) contours(v,nlevels=nlevels,...))
    }
    else if (depth(x) > 1)
    {
        stop("This function only works on 2D images")
    }
    else
        {
            out <- as.matrix(x) %>% contourLines(1:width(x),1:height(x),.,nlevels=nlevels,...)
            out
        }
}


##' @export
contours.pixset <- function(x,nlevels=NULL,...)
{
    if (spectrum(x) > 1)
    {
        channels(x) %>% map(as.pixset) %>% map(function(v) contours(v,...))
    }
    else if (depth(x) > 1)
    {
        stop("This function only works on 2D pixel sets")
    }
    else
        {
            out <- as.matrix(x) %>% contourLines(1:width(x),1:height(x),.,nlevels=2,levels=c(0,1))
            out
        }
}

##' Remove alpha channel and store as attribute
##'
##' @param im an image with 4 RGBA colour channels
##' @return an image with only three RGB channels and the alpha channel as attribute
##' @examples
##' #An image with 4 colour channels (RGBA)
##' im <- imfill(2,2,val=c(0,0,0,0))
##' #Remove fourth channel
##' rm.alpha(im)
##' attr(rm.alpha(im),"alpha")
##' @author Simon Barthelme
##' @seealso flatten.alpha
##' @export
rm.alpha <- function(im)
{
    if (spectrum(im)==4)
    {
        alpha <- imsub(im,cc==4)
        im <- imsub(im,cc<=3)
        attr(im,"alpha") <- alpha
    }
    im
}

##' Flatten alpha channel
##'
##' @param im an image (with 4 RGBA colour channels)
##' @param bg background: either an RGB image, or a vector of colour values, or a string (e.g. "blue"). Default: white background.
##' @return a blended image
##' @seealso rm.alpha
##' @examples
##' #Add alpha channel
##' alpha <- Xc(grayscale(boats))/width(boats)
##' boats.a <- imlist(boats,alpha) %>% imappend("c")
##' flatten.alpha(boats.a) %>% plot
##' flatten.alpha(boats.a,"darkgreen") %>% plot
##' @author Simon Barthelme
##' @export
flatten.alpha <- function(im,bg="white")
{
    if (spectrum(im)==4)
    {
        a <- channel(im,4) %>% add.colour
        im <- rm.alpha(im)
        if (is.vector(bg) || is.character(bg))
        {
            bg <- imfill(dim=dim(im),val=bg)
        }
        else
        {
            stop("Unrecognised format for bg argument")
        }
        im*a + bg*(1-a)
    }
    else
    {
        im
    }
}

#' Mutate a data frame by adding new or replacing existing columns.
#'
#' This function copied directly from plyr, and modified to use a different
#' name to avoid namespace collisions with dplyr/tidyverse functions.
#'
#' This function is very similar to \code{\link{transform}} but it executes
#' the transformations iteratively so that later transformations can use the
#' columns created by earlier transformations.  Like transform, unnamed
#' components are silently dropped.
#'
#' Mutate seems to be considerably faster than transform for large data
#' frames.
#'
#' @param .data the data frame to transform
#' @param ... named parameters giving definitions of new columns.
mutate_plyr <- function(.data, ...) {
  stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))

  cols <- as.list(substitute(list(...))[-1])
  cols <- cols[names(cols) != ""] # Silently drop unnamed columns

  for (col in names(cols)) {
    .data[[col]] <- eval(cols[[col]], .data, parent.frame())
  }
  .data
}
