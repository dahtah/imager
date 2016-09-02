#Some interactive functions 

##' Select image regions interactively
##'
##' These functions let you select a shape in an image (a point, a line, or a rectangle)
##' They either return the coordinates of the shape (default), or the contents.
##' In case of lines contents are interpolated. 
##' @name grab
##' @param im an image
##' @param coord if TRUE, return coordinates. if FALSE, content
##' @return either a vector of coordinates, or an image
##' @seealso display
##' @examples
##' ##Not run: interactive only 
##' ##grabRect(boats)
##' ##grabRect(boats,TRUE)
##' @author Simon Barthelme
NULL

##' @rdname grab
##' @export 
grabLine <- function(im,coord=TRUE)
{
    if (depth(im) > 1)
    {
        stop("This function does not work on images with depth > 1")
    }
    else
    {
        out <- select(im,1)
        cs <- c(out)[-c(3,6)]
        cs <- cs+1
        names(cs) <- c("x0","y0","x1","y1")
        if (coord)
        {
            cs
        }
        else
        {
            interp.line(im,cs[1],cs[2],cs[3],cs[4])
        }
    }
}

##' @rdname grab
##' @export 
grabRect <- function(im,coord=TRUE)
{
    if (depth(im) > 1)
    {
        stop("This function does not work on images with depth > 1")
    }
    else
    {
        out <- select(im)
        cs <- c(out)[-c(3,6)]
        cs[1:2] <- cs[1:2]+1
        names(cs) <- c("x","y","w","h")
        if (coord)
        {
            cs
        }
        else
        {
            subim(im, x %inr% c(cs[1],cs[1]+cs[3]), y %inr% c(cs[2],cs[2]+cs[4]))
        }
    }
}

##' @rdname grab
##' @export 
grabPoint <- function(im,coord=TRUE)
{
    if (depth(im) > 1)
    {
        stop("This function does not work on images with depth > 1")
    }
    else
    {
        out <- select(im)
        cs <- c(out)[1:2]
        cs <- cs+1
        names(cs) <- c("x","y")
        if (coord)
        {
            cs
        }
        else
        {
            if (spectrum(im) > 1)
            {
                color.at(im,cs[1],cs[2]) %>% setNames(c('r','g','b'))
            }
            else
            {
                at(im,cs[1],cs[2])
            }
        }
    }
}

interp.line <- function(im,x0,y0,x1,y1,n,...)
{
    dx <- x1-x0
    dy <- y1-y0
    l <- sqrt(dx^2+dy^2)
    if (missing(n)) n <- l
    t <- seq(0,1,l=n)
    x <- x0+t*dx
    y <- y0+t*dy
    interp(im,data.frame(x=x,y=y),...)
}

