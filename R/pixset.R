
#' Pixel sets (pixsets)
#'
#' Pixel sets represent sets of pixels in images (ROIs, foreground, etc.). From an implementation point of view, they're just a thin layer over arrays of logical values, just like the cimg class is a layer over arrays of numeric values.
#' Pixsets can be turned back into logical arrays, but they come with a number of generic functions that should make your life easier.
#' They are created automatically whenever you run a test on an image (for example im > 0 returns a pixset). 
#' @param x an array of logical values
#' @export
#' @examples
#' #A test on an image returns a pixset
#' boats > 250
#' #Pixsets can be combined using the usual Boolean operators
#' (boats > 230) & (Xc(boats) < width(boats)/2)
#' #Subset an image using a pixset
#' boats[boats > 250]
#' #Turn a pixset into an image
#' as.cimg(boats > 250)
#' #Equivalently:
#' (boats > 250) + 0
pixset <- function(x)
    {
        if (is.logical(x) && is.array(x) && (length(dim(x)) == 4 ))
            {
                class(x) <-c("pixset","imager_array","logical")
                x
            }
        else
            {
                stop("A pixset must be a logical array with four dimensions")
            }
    }

##' Check that an object is a pixset object
##' @param x an object
##' @return logical
##' @export
is.pixset <- function(x) is(x,"pixset")

#' Methods to convert various objects to pixsets
#'
#' @param x object to convert to pixset
#' @param ... ignored
#' @examples
#' #When converting an image to a pixset, the default is to include all pixels with non-zero value 
#' as.pixset(boats)
#' #The above is equivalent to:
#' boats!=0
#' 
#' @export
as.pixset <- function(x,...) UseMethod("as.pixset")

#' @export
as.pixset.array <- function(x,...) pixset(x)

#' @export
as.pixset.logical <- function(x,...) pixset(x)

#' @describeIn as.pixset convert cimg to pixset
#' @export
as.pixset.cimg <- function(x,...) pixset(x!=0)

#' @describeIn as.pixset convert pixset to cimg
#' @param obj pixset to convert
#' @export
as.cimg.pixset <- function(obj,...) obj+0

#' @export
as.pixset.pixset <- function(x,...) x

#' @export
as.logical.pixset <- function(x,...) { class(x) <- "logical"; x }

#' Methods to convert pixsets to various objects
#'
#' @param x pixset to convert
#' @aliases convert_pixset
#' @param ... ignored
#' @seealso where
#' @examples
#' 
#' px <- boats > 250
#' #Convert to array of logicals
#' as.logical(px) %>% dim
#' #Convert to data.frame: gives all pixel locations in the set
#' as.data.frame(px) %>% head
#' #Drop flat dimensions
#' as.data.frame(px,drop=TRUE) %>% head
#' @param drop drop flat dimensions
#' @export
as.data.frame.pixset <- function(x,...,drop=FALSE) {
    co <- coord.index(x,which(x))
    if (drop)
        {
            co[,dim(x)>1,drop=FALSE]
        }
    else
        {
            co
        }
}


##' @export 
as.matrix.pixset <- function(x,...) {
    d <- dim(x)
    if (sum(d==1) == 2)
        {
            x <- squeeze(x)
            class(x) <- "matrix"
            x
        }
    else if (sum(d > 1) == 1)
    {
        warning("Image is one-dimensional")
        as.vector(x) %>% as.matrix
    }
    else
        {
            stop("Too many non-empty dimensions")
        }
}


size <- function(x) sum(x)


check.pixset <- function(x)
    {
        if (!is(x,"pixset"))
            {
                stop("Argument must be a pixel set")
            }
    }

#' Return locations in pixel set
#'
#' @param x a pixset
#' @examples
#' #All pixel locations with value greater than .99
#' where(boats > .99) 
#' @export
where <- function(x)
    {
        
        if (!is.pixset(x))
            {
                if (is.logical(x))
                    {
                        x <- as.pixset(x)
                    }
                else
                    {
                        stop("Argument must be a pixel set")
                    }
            }
        as.data.frame(x,drop=TRUE)
    }

#' @export
plot.pixset <- function(x,...) as.cimg(x) %>% plot(...)

#' @export
print.pixset <- function(x,...)
    {
        d <- dim(x)
        msg <- sprintf("Pixel set of size %i. Width: %i pix Height: %i pix Depth: %i Colour channels: %i \n",size(x),d[1],d[2],d[3],d[4])
        cat(msg)
        invisible(x)
    }

#' @method Ops imager_array
#' @export
Ops.imager_array <- function(e1, e2)
{
    
    e1 <- unclass(e1)
    if (!missing(e2))
        {
            e2 <- unclass(e2)
        }
    out <- NextMethod(.Generic)
    if (is.logical(out))
        {
            as.pixset(out)
        }
    else
        {
            as.cimg(out)
        }
}


#' Grow/shrink a pixel set
#'
#' Grow/shrink a pixel set through morphological dilation/erosion. The default is to use square or rectangular structuring elements, but an arbitrary structuring element can be given as input. 
#' A structuring element is a pattern to be moved over the image: for example a 3x3 square. In "shrink" mode, a element of the pixset is retained only if and only the structuring element fits entirely within the pixset. In "grow" mode, the structuring element acts like a neighbourhood: all pixels that are in the original pixset *or* in the neighbourhood defined by the structuring element belong the new pixset. 
#' @param px a pixset
#' @param x either an integer value, or an image/pixel set. 
#' @param y width of the rectangular structuring element (if x is an integer value)
#' @param z depth of the rectangular structuring element (if x is an integer value)
#' @param boundary are pixels beyond the boundary considered to have value TRUE or FALSE (default TRUE)
#' @examples
#' #A pixel set:
#' a <- grayscale(boats) > .8
#' plot(a)
#' #Grow by a 8x8 square
#' grow(a,8) %>% plot
#' #Grow by a 8x2 rectangle
#' grow(a,8,2) %>% plot
#' #Custom structuring element
#' el <- matrix(1,2,2) %>% as.cimg
#' all.equal(grow(a,el),grow(a,2))
#' #Circular structuring element
#' px.circle(5) %>% grow(a,.) %>% plot
#' @export
grow <- function(px,x,y=x,z=x,boundary=TRUE)
{
    if (is.cimg(x) || is.pixset(x))
    {
        bdilate(px,as.pixset(x),boundary_conditions=boundary)
    }
    else if (x==y && y==z)
    {
        if (boundary)
        {
            bdilate_square(px,x)
        }
        else
        {
            msk <- array(TRUE,ifelse(dim(px) > 1,x,1)) %>% as.pixset
            bdilate(px,msk,boundary_conditions=FALSE)
        }
    }
    else
    {
        if (boundary)
        {
            bdilate_rect(px,x,y,z)
        }
        else
        {
            msk <- array(TRUE,ifelse(dim(px) > 1,c(x,y,z,1),1)) %>% as.pixset
            bdilate(px,msk,boundary_conditions=FALSE)
        }
    }
}

##' Clean up and fill in pixel sets (morphological opening and closing)
##' 
##' Cleaning up a pixel set here means removing small isolated elements (speckle). Filling in means removing holes.
##' Cleaning up can be achieved by shrinking the set (removing speckle), followed by growing it back up. Filling in can be achieved by growing the set (removing holes), and shrinking it again. 
##' @param px a pixset
##' @param ... parameters that define the structuring element to use, passed on to "grow" and "shrink"
##' @examples
##  #A pixset
##' im <- load.example("birds") %>% grayscale
##' sub <- imsub(-im,y> 380) %>% threshold("85%")
##' plot(sub)
##' #Turn into a pixel set
##' px <- sub==1
##' layout(t(1:2))
##' plot(px,main="Before clean-up")
##' clean(px,3) %>% plot(main="After clean-up")
##' #Now fill in the holes
##' px <- clean(px,3)
##' plot(px,main="Before filling-in")
##' fill(px,28) %>% plot(main="After filling-in")
##' @author Simon Barthelme
##' @export
clean <- function(px,...)
    {
        shrink(px,...) %>% grow(...)
    }

##' @describeIn clean Fill in holes using morphological closing
##' @export
fill <- function(px,...)
    {
        grow(px,...) %>% shrink(...)
    }


#' @describeIn grow shrink pixset using erosion
#' @examples
#' #Sometimes boundary conditions matter
#' im <- imfill(10,10)
#' px <- px.all(im)
#' shrink(px,3,bound=TRUE) %>% plot(main="Boundary conditions: TRUE")
#' shrink(px,3,bound=FALSE) %>% plot(main="Boundary conditions: FALSE")
#' @export
shrink <- function(px,x,y=x,z=x,boundary=TRUE)
{
    if (is.cimg(x) || is.pixset(x))
    {
        berode(px,as.pixset(x),boundary_conditions=boundary)
    }
    else if (x==y && y==z)
    {
        if (boundary)
        {
            berode_square(px,x)
        }
        else
        {
            msk <- array(TRUE,ifelse(dim(px) > 1,x,1)) %>% as.pixset
            berode(px, msk,boundary_conditions=FALSE)
        }
    }
    else
    {
        if (boundary)
        {
            berode_rect(px,x,y,z)
        }
        else
        {
            msk <- array(TRUE,ifelse(dim(px) > 1,c(x,y,z,1),1)) %>% as.pixset
            berode(px,msk,boundary_conditions=FALSE)
        }
    }
}


##' Various useful pixsets
##'
##' These functions define some commonly used pixsets.
##' px.left gives the left-most pixels of an image, px.right the right-most, etc.
##' px.circle returns an (approximately) circular pixset of radius r, embedded in an image of width x and height y
##' Mathematically speaking, the set of all pixels whose L2 distance to the center equals r or less.
##' px.diamond is similar but returns a diamond (L1 distance less than r)
##' px.square is also similar but returns a square (Linf distance less than r)
##' @name common_pixsets
##' @param r radius (in pixels)
##' @param x width (default 2*r+1)
##' @param y height (default 2*r+1)
##' @return a pixset
##' @examples
##' px.circle(20,350,350) %>% plot(interp=FALSE)
##' px.circle(3) %>% plot(interp=FALSE)
##' r <- 5
##' layout(t(1:3))
##' plot(px.circle(r,20,20))
##' plot(px.square(r,20,20))
##' plot(px.diamond(r,20,20))
##' #These pixsets are useful as structuring elements
##' px <- grayscale(boats) > .8
##' grow(px,px.circle(5)) %>% plot
##' #The following functions select pixels on the left, right, bottom, top of the image
##' im <- imfill(10,10)
##' px.left(im,3) %>% plot(int=FALSE)
##' px.right(im,1) %>% plot(int=FALSE)
##' px.top(im,4) %>% plot(int=FALSE)
##' px.bottom(im,2) %>% plot(int=FALSE)
##' #All of the above
##' px.borders(im,1) %>% plot(int=FALSE)
##' @author Simon Barthelme
NULL

#' @describeIn common_pixsets A circular-shaped pixset
##' @export
px.circle <- function(r,x=2*r+1,y=2*r+1)
    {
        im <- imfill(x,y)
        (Xc(im)-(x+1)/2)^2 + (Yc(im)-(y+1)/2)^2 <= r^2
    }

#' @describeIn common_pixsets A diamond-shaped pixset
#' @export
px.diamond <- function(r,x=2*r+1,y=2*r+1)
    {
        im <- imfill(x,y)
        abs(Xc(im)-(x+1)/2) + abs(Yc(im)-(y+1)/2) <= r
    }

#' @describeIn common_pixsets A square-shaped pixset
#' @export
px.square <- function(r,x=2*r+1,y=2*r+1)
    {
        im <- imfill(x,y)
        (abs(Xc(im)-(x+1)/2) <= r) & (abs(Yc(im)-(y+1)/2) <= r)
    }


#' @describeIn common_pixsets n left-most pixels (left-hand border)
#' @param im an image
#' @param n number of pixels to include
#' @export
px.left <- function(im,n=1)
    {
        Xc(im) <= n
    }

#' @describeIn common_pixsets n top-most pixels 
#' @export
px.top <- function(im,n=1)
    {
        Yc(im) <= n
    }

#' @describeIn common_pixsets n bottom-most pixels 
#' @export
px.bottom <- function(im,n=1)
    {
        Yc(im) > height(im)- n
    }


#' @describeIn common_pixsets n right-most pixels 
#' @export
px.right <- function(im,n=1)
    {
        Xc(im) > (width(im)-n)
    }

#' @describeIn common_pixsets image borders (to depth n)
#' @export
px.borders <- function(im,n=1)
{
    if (all(dim(im)[1:2] > 1))
        {
            (px.left(im,n) | px.right(im,n)) | (px.top(im,n) | px.bottom(im,n))
        }
    else
    {
        (px.left(im,n) | px.right(im,n))
    }
}

#' @describeIn common_pixsets all pixels in image
#' @export
px.all <- function(im)
{
    array(TRUE,dim=dim(im)) %>% pixset
}

#' @describeIn common_pixsets no pixel in image
#' @export
px.none <- function(im)
{
    array(FALSE,dim=dim(im)) %>% pixset
}


#' Find the boundary of a shape in a pixel set
#' 
#' @param px pixel set
#' @param depth boundary depth (default 1)
#' @param high_connexity if FALSE, use 4-point neighbourhood. If TRUE, use 8-point.  (default FALSE)
#' @examples
#' px.diamond(10,30,30) %>% boundary %>% plot
#' px.square(10,30,30) %>% boundary %>% plot
#' px.square(10,30,30) %>% boundary(depth=3) %>% plot
#' px <- (px.square(10,30,30) | px.circle(12,30,30))
#' boundary(px,high=TRUE) %>% plot(int=TRUE,main="8-point neighbourhood")
#' boundary(px,high=TRUE) %>% plot(int=FALSE,main="4-point neighbourhood")
#' @export
boundary <- function(px,depth=1,high_connexity=FALSE)
{
    a <- if (high_connexity) sqrt(2) else 1
    bd <- bdistance_transform(px,FALSE)
    (bd <= depth*a) & px
}


##' Highlight pixel set on image
##'
##' Overlay an image plot with the contours of a pixel set. Note that this function doesn't do the image plotting, just the highlighting. 
##' 
##' @param px a pixel set 
##' @param col color of the contours
##' @param ... passed to the "lines" function
##' @author Simon Barthelme
##' @seealso colorise, another way of highlighting stuff
##' @examples
##' #Select similar pixels around point (180,200)
##' px <- px.flood(boats,180,200,sigma=.08)
##' plot(boats)
##' #Highlight selected set
##' highlight(px)
##' px.flood(boats,18,50,sigma=.08) %>% highlight(col="white",lwd=3)
##' @export
highlight <- function(px,col="red",...)
{
    if (spectrum(px) == 1)
    {
        contours(px) %>% purrr::walk(function(v) lines(v$x,v$y,col=col,...))
    }
    else
    {
        contours(px) %>% { purrr::flatten(.) } %>% purrr::walk(function(v) lines(v$x,v$y,col=col,...))
    }
}

#' Select a region of homogeneous colour 
#'
#' Select pixels that are similar to a seed pixel. The underlying algorithm is the same as the bucket fill (AKA flood fill). Unlike with the bucket fill, the image isn't changed, the function simply returns a pixel set containing the selected pixels.
#'
#' Old name: selectSimilar (deprecated)
#' 
#' @param im an image
#' @param x X-coordinate of the starting point of the region to flood
#' @param y Y-coordinate of the starting point of the region to flood
#' @param z Z-coordinate of the starting point of the region to flood
#' @param sigma Tolerance concerning neighborhood values.
#' @param high_connexity Use 8-connexity (only for 2d images, default FALSE).
#' @export
#' @examples
#' #Select part of a sail 
#' px <- px.flood(boats,x=169,y=179,sigma=.2) 
#' plot(boats)
#' highlight(px)
#' @seealso bucketfill
#' @export
px.flood <- function(im,x,y,z=1,sigma=0,high_connexity=FALSE)
    {
        out <- bucket_select(im,x,y,z,sigma,high_connexity)
        if (spectrum(im) > 1)
        {
            imrep(out,spectrum(im)) %>% imappend("c")
        }
        else
        {
            out
        }
    }

##' Split pixset into connected components
##'
##' Compute connected components (using "label"), then split into as many sets as there are components.
##' Useful for segmentation
##' @param px a pixset
##' @param ... further arguments passed to label
##' @seealso label
##' @return a list of pixsets
##' @examples
##' px <- isoblur(grayscale(boats),5) > .75
##' plot(px)
##' spl <- split_connected(px)
##' plot(spl[[1]])
##' px <- isoblur(grayscale(boats),5) > .75
##' plot(px)
##' spl <- split_connected(px)
##' plot(spl[[1]])
##' @author Simon Barthelme
##' @export split_connected
split_connected <- function(px,...)
{
    if (sum(px)==0)
    {
        stop("Pixset is empty, can't split")
    }
    else
        {
            lab <- label(px,...)
            lab <- lab*as.cimg(px)
            unique(lab) %>% { .[. > 0] } %>% map_il(function(v) lab==v)
        }
}

#' @export
display.pixset <- function(x,...) display(as.cimg(x),...)



##' @export
`[.pixset` <- function(x,...) {
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
            pixset(out)
        }
    }
    else
    {
        out
    }
}

##' @export
`[<-.pixset` <- function(x,...,value) {
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
    pixset(out)
}


##' Compute the bounding box of a pixset 
##'
##' This function returns the bounding box of a pixset as another pixset. If the image has more than one frame, a bounding cube is returned.
##' If the image has several colour channels, the bounding box is computed separately in each channel.
##' crop.bbox crops an image using the bounding box of a pixset. 
##' @param px a pixset
##' @return a pixset object
##' @examples
##' im <- grayscale(boats)
##' px <- im > .85
##' plot(im)
##' highlight(bbox(px))
##' highlight(px,col="green")
##' crop.bbox(im,px) %>% plot
##' @author Simon Barthelme
##' @export
bbox <- function(px)
{
    w <- where(px)
    #Compute separately across colour channels 
    if (spectrum(px) > 1)
    {
        imsplit(px,"c") %>% map(bbox) %>% imappend("c")
    }
    
    if (sum(px) == 0)
    {
        px
    }
    else
        {
            if (depth(px)==1)
            {
                (Xc(px) %inr% range(w$x)) & (Yc(px) %inr% range(w$y))
            }
            else
            {
                (Xc(px) %inr% range(w$x)) & (Yc(px) %inr% range(w$y)) & (Zc(px) %inr% range(w$z))
            }
        }
}



#' @describeIn bbox crop image using the bounding box of pixset px
#' @param im an image
#' @export
crop.bbox <- function(im,px)
{
    w <- where(px)
    
    if (spectrum(px) > 1)
    {
        px <- imsplit(px,"c") %>% parany
    }
    
    if (sum(px) == 0)
    {
        im
    }
    else
        {
            if (depth(px)==1)
            {
                imsub(im,x %inr% range(w$x),y %inr% range(w$y))
            }
            else
            {
                imsub(im,x %inr% range(w$x),y %inr% range(w$y),z %inr% range(w$z))
            }
        }
}




##' A pixset for NA values
##'
##' A pixset containing all NA pixels
##' @param im an image
##' @return a pixset
##' @examples
##' im <- boats
##' im[1] <- NA
##' px.na(im)
##' @export
px.na <- function(im)
{
    is.na(im) %>% pixset
}


##' Fill in a colour in an area given by a pixset
##'
##' Paint all pixels in pixset px with the same colour 
##' @param im an image
##' @param px either a pixset or a formula, as in imeval. 
##' @param col colour to fill in. either a vector of numeric values or a string (e.g. "red")
##' @param alpha transparency (default 1, no transparency)
##' @return an image
##' @examples
##' im <- load.example("coins")
##' colorise(im,Xc(im) < 50,"blue") %>% plot
##' #Same thing with the formula interface
##' colorise(im,~ x < 50,"blue") %>% plot
##' #Add transparency
##' colorise(im,~ x < 50,"blue",alpha=.5) %>% plot
##' #Highlight pixels with low luminance values
##' colorise(im,~ . < 0.3,"blue",alpha=.2) %>% plot
##' @author Simon Barthelme
##' @export
colorise <- function(im,px,col,alpha=1)
{
    if (is.character(col))
    {
        col <- col2rgb(col)[,1]/255
    }
    if (is.formula(px)) {
        px <- imeval(im, px)
    }

    if (spectrum(px)!=1)
    {
        px <- imsplit(px,"c") %>% parany
    }
    mod <- function(im,c) { if (alpha==1) { im[px] <- c} else { im[px] <- alpha*c+(1-alpha)*im[px]}; im }
    imsplit(im,"c") %>% map2_il(col,mod) %>% imappend("c")
}

##' Remove all connected regions that touch image boundaries
##'
##' All pixels that belong to a connected region in contact with image boundaries are set to FALSE. 
##' @param px a pixset
##' @return a pixset 
##' @author Simon Barthelme
##' @examples
##' im <- draw_circle(imfill(100,100),c(0,50,100),c(50,50,50),radius=10,color=1)
##' plot(im)
##' as.pixset(im) %>% px.remove_outer %>% plot
##' @export
px.remove_outer <- function(px)
{
    pad(px,2,"xy",val=TRUE) %>% bucketfill(1,1,color=0) %>% crop.borders(1,1)  %>% as.pixset
}
