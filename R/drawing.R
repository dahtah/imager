#' Bucket fill
#'
#' @param im an image
#' @param x X-coordinate of the starting point of the region to fill.
#' @param y Y-coordinate of the starting point of the region to fill.
#' @param z Z-coordinate of the starting point of the region to fill.
#' @param color Pointer to spectrum() consecutive values, defining the drawing color. If missing, use value at location (x,y,z)
#' @param sigma Tolerance concerning neighborhood values.
#' @param opacity Opacity of the drawing.
#' @param high_connexity Use 8-connexity (only for 2d images, default FALSE).
#' @export
#' @examples
#' #Change the colour of a sail 
#' boats.new <- bucketfill(boats,x=169,y=179,color=c(125,0,125),sigma=20) 
#' layout(t(1:2))
#' plot(boats,main="Original")
#' plot(boats.new,main="New sails")
#' @seealso px.flood
bucketfill <- function(im,x,y,z = 1,color,opacity=1,sigma=0,high_connexity=FALSE)
    {
        if (missing(color))
            {
                color <- color.at(im,x,y,z)
            }
        else
        {
            if (is.character(color))
                {
                    color <- col2rgb(color)[,1]/255
                }
        }
        if (length(color)!=spectrum(im)) stop("colour argument has wrong length")
        bucket_fill(im,x,y,z,color,opacity,sigma,high_connexity)
    }

##' Draw circle on image
##'
##' Add circle or circles to an image. Like other native CImg drawing functions, this is meant to be basic but fast. Use implot for flexible drawing. 
##' @param im an image
##' @param x x coordinates
##' @param y y coordinates
##' @param radius radius (either a single value or a vector)
##' @param color either a vector, or a string
##' @param opacity 0: transparent 1: opaque. 
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
    if (is.character(color))
    {
        color <- col2rgb(color)[,1]/255
    }
    if (length(color)!=spectrum(im)) stop("colour argument has wrong length")
    if (length(x) != length(y)) stop("x and y must be the same length")
    if (length(radius)==1) radius <- rep(radius,length(x)) 
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
    draw_text_(im,x,y,text,color,opacity=1,fsize=20)
}
