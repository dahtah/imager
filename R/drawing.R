#' Bucket fill
#'
#' @param im an image
#' @param x X-coordinate of the starting point of the region to fill.
#' @param y Y-coordinate of the starting point of the region to fill.
#' @param z Z-coordinate of the starting point of the region to fill.
#' @param color a vector of values (of length spectrum(im)), or a colour name (e.g. "red"). If missing, use the colour at location (x,y,z).
#' @param sigma Tolerance for neighborhood values: spread to neighbours if difference is less than sigma (for grayscale). If there are several channels, the sum of squared differences is used: if it below sigma^2, the colour spreads. 
#' @param opacity opacity. If the opacity is below 1, paint with transparency. 
#' @param high_connexity Use 8-connexity (only for 2d images, default FALSE).
#' @export
#' @examples
#' #Change the colour of a sail 
#' boats.new <- bucketfill(boats,x=169,y=179,color="pink",sigma=.2) 
#' layout(t(1:2))
#' plot(boats,main="Original")
#' plot(boats.new,main="New sails")
#'
#' #More spreading, lower opacity, colour specified as vector
#' ugly <- bucketfill(boats,x=169,y=179,color=c(0,1,0),sigma=.6,opacity=.5)
#' plot(ugly)
#' @seealso px.flood
bucketfill <- function(im,x,y,z = 1,color,opacity=1,sigma=0,high_connexity=FALSE)
    {
        if (missing(color))
            {
                color <- color.at(im,x,y,z)
            }
        if (is.character(color))
        {
            color <- col2rgb(color)[,1]/255
        }
        if (length(color)!=spectrum(im))
        {
            if (spectrum(im)==1)
            {
                im <- add.colour(im)
                warning("Adding colour channels to image (was grayscale)")
            }
            else
            {
                stop('The color argument must be the same length as the number of color channels')
            }
        }
        bucket_fill(im,x,y,z,color,opacity,sigma,high_connexity)
    }

##' Draw circle on image
##'
##' Add circle or circles to an image. Like other native CImg drawing functions, this is meant to be basic but fast. Use implot for flexible drawing. 
##' @param im an image
##' @param x x coordinates
##' @param y y coordinates
##' @param radius radius (either a single value or a vector of length equal to length(x))
##' @param color either a string ("red"), a character vector of length equal to x, or a matrix of dimension length(x) times spectrum(im)
##' @param opacity scalar or vector of length equal to length(x). 0: transparent 1: opaque.
##' @param filled fill circle (default TRUE)
##' @return an image
##' @seealso implot
##' @examples
##' draw_circle(boats,c(50,100),c(150,200),30,"darkgreen") %>% plot
##' draw_circle(boats,125,60,radius=30,col=c(0,1,0),opacity=.2,filled=TRUE) %>% plot
##' @author Simon Barthelme
##' @export
draw_circle <- function(im,x,y,radius,color="white",opacity=1,filled=TRUE)
{
    if (is.vector(color) & (length(color) != length(x)))
    {
        color <- matrix(color,length(x),length(color),byrow=TRUE)
    }

    if (is.character(color))
        {
            color <- t(col2rgb(color)/255)
        }
    if (ncol(color)!=spectrum(im)) stop("colour argument has wrong length")
    if (length(x) != length(y)) stop("x and y must be the same length")
    if (length(radius)==1) radius <- rep(radius,length(x))
    if (length(opacity)==1) opacity <- rep(opacity,length(x)) 
    draw_circle_(im,x,y,radius,color,opacity,filled)
}

##' Draw rectangle on image
##'
##' Add a rectangle to an image. Like other native CImg drawing functions, this is meant to be basic but fast. Use implot for flexible drawing. 
##' @param im an image
##' @param x0 x coordinate of the bottom-left corner 
##' @param y0 y coordinate  of the bottom-left corner 
##' @param x1 x coordinate of the top-right corner 
##' @param y1 y coordinate  of the top-right corner 
##' @param color either a vector, or a string (e.g. "blue")
##' @param opacity 0: transparent 1: opaque.
##' @param filled fill rectangle (default TRUE)
##' @return an image
##' @seealso implot,draw_circle
##' @examples
##' draw_rect(boats,1,1,50,50,"darkgreen") %>% plot
##' @author Simon Barthelme
##' @export
draw_rect <- function(im,x0,y0,x1,y1,color="white",opacity=1,filled=TRUE)
{
    if (is.character(color))
    {
        color <- col2rgb(color)[,1]/255
    }
    if (length(color)!=spectrum(im)) stop("colour argument has wrong length")
    ls <- map_int(c(x0,y0,x1,y1),length)
    if (min(ls) != max(ls)) stop("x0,y0,x1,y1 must be the same length")
    draw_rect_(im,x0,y0,x1,y1,color,opacity,filled)
}


##' Draw text on an image
##'
##' Like other native CImg drawing functions, this is meant to be basic but fast. Use implot for flexible drawing. 
##' @param im an image
##' @param x x coord. 
##' @param y y coord.
##' @param text text to draw (a string)
##' @param color either a vector or a string (e.g. "red")
##' @param opacity 0: transparent 1: opaque.
##' @param fsize font size (in pix., default 20)
##' @return an image
##' @seealso implot,draw_circle,draw_rect
##' @examples
##' draw_text(boats,100,100,"Some text",col="black") %>% plot
##' @author Simon Barthelme
##' @export
draw_text <- function(im,x,y,text,color,opacity=1,fsize=20)
{
    if (is.character(color))
    {
        color <- col2rgb(color)[,1]/255
    }
    if (length(color)!=spectrum(im)) stop("colour argument has wrong length")
    draw_text_(im,x,y,text,color,opacity,fsize)
}
