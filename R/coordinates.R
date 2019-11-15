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
                res[,d > 1,drop=FALSE]
            }
        else
            {
                res
            }
    }

##' Coordinates as images
##'
##' These functions return pixel coordinates for an image, as an image. All is made clear in the examples (hopefully)
##' @param im an image
##' @name imcoord
##' @return another image of the same size, containing pixel coordinates
##' @seealso as.cimg.function, pixel.grid
##' @examples
##' im <- imfill(5,5) #An image
##' Xc(im) #An image of the same size, containing the x coordinates of each pixel
##' Xc(im) %>% imrow(1)
##' Yc(im) %>% imrow(3) #y is constant along rows
##' Yc(im) %>% imcol(1)
##' #Mask bits of the boats image:
##' plot(boats*(Xc(boats) < 100))
##' plot(boats*(dnorm(Xc(boats),m=100,sd=30))) #Gaussian window
NULL

##' @describeIn imcoord X coordinates 
##' @export
Xc <- function(im) coordImage(im,"x")
##' @describeIn imcoord Y coordinates 
##' @export
Yc <- function(im) coordImage(im,"y")
##' @describeIn imcoord Z coordinates 
##' @export
Zc <- function(im) coordImage(im,"z")
##' @describeIn imcoord C coordinates 
##' @export
Cc <- function(im) coordImage(im,"c")

coordImage <- function(im,channel)
{
    d <- dim(im)
    fun <- list(x=getXc,y=getYc,z=getZc,c=getCc)
    fun[[channel]](d[1],d[2],d[3],d[4])+1
}




##' Linear index in internal vector from pixel coordinates
##'
##' Pixels are stored linearly in (x,y,z,c) order. This function computes the vector index of a pixel given its coordinates
##' @param im an image
##' @param coords a data.frame with values x,y,z (optional), c (optional)
##' @param outside what to do if some coordinates are outside the image: "stop" issues error, "NA" replaces invalid coordinates with NAs. Default: "stop". 
##' @return a vector of indices (NA if the indices are invalid)
##' @examples
##' im <- as.cimg(function(x,y) x+y,100,100)
##' px <- index.coord(im,data.frame(x=c(3,3),y=c(1,2)))
##' im[px] #Values should be 3+1=4, 3+2=5
##' @author Simon Barthelme
##' @seealso coord.index, the reverse operation
##' @export
index.coord <- function(im,coords,outside="stop")
{
    if ("c" %in% names(coords))
            {
                names(coords) <- stringr::str_replace_all(names(coords),c("c"="cc")) #Safer
            }
    nc <- setdiff(names.coords,"c")
    ms <- setdiff(nc,names(coords))
    for (m in ms) #Add missing coordinates
    {
        if (dimn(im,m) == 1)
        {
            #Check singleton oth. ambiguous
            coords[[m]] <- 1
        }
        else
        {
            stop(paste0("Ambiguous coordinates: missing ",m))
        }
    }
    if (prod(dim(im)) == 1) #Edge case
    {
        ind <- rowSums(as.matrix(coords[,c("x","y","z","cc")])-1)+1
    }
    else
    {
        ns <- nc[dim(im) > 1]
        ws <- (c(1,cumprod(dim(im))[1:3]))[dim(im) > 1]
        ind <- 1+(as.matrix(coords[,ns])-1)%*%ws
    }
    check <- with(coords,checkcoords(x,y,z,cc,dim(im)))
    if (!all(check))
    {
        if (outside=="stop")
        {
            stop("Some coordinates are outside the image")
        }
        else if (outside == "NA")
        {
            ind[!check] <- NA
        }
    }
    as.vector(ind)
}

##' Coordinates from pixel index
##'
##' Compute (x,y,z,cc) coordinates from linear pixel index. 
##' @param im an image
##' @param index a vector of indices
##' @return a data.frame of coordinate values
##' @seealso index.coord for the reverse operation
##' @examples
##' cind <- coord.index(boats,33)
##' #Returns (x,y,z,c) coordinates of the 33rd pixel in the array
##' cind
##' all.equal(boats[33],with(cind,at(boats,x,y,z,cc)))
##' all.equal(33,index.coord(boats,cind))
##' @author Simon Barthelme
##' @export
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


pixel.index <- coord.index

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


dimn <- function(im,v) dim(im)[index.coords[[v]]]
