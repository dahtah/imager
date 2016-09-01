# Stencils are data frames which define neighbourhoods around a pixel


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
        d <- center.stencil(stencil,...) %>% index.coord(im,.)
        im[d]
    }

check.dim <- function(im,coord,d)
{
    if (is.character(d)) d <- index.coords[[d]]
    coord %inr% c(1,dim(im)[d])
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

