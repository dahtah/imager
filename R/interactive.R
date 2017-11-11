#Some interactive functions 

##' Select image regions interactively
##'
##' These functions let you select a shape in an image (a point, a line, or a rectangle)
##' They either return the coordinates of the shape (default), or the contents.
##' In case of lines contents are interpolated. Note that grabLine does not support the "pixset" return type. 
##' @name grab
##' @param im an image
##' @param output one of "im","pixset","coord","value". Default "coord"
##' @return Depending on the value of the output parameter. Either a vector of coordinates (output = "coord"), an image (output = "im"), a pixset (output = "pixset"), or a vector of values (output = "value"). grabLine and grabPoint support the "value" output mode and not the "im" output. 
##' @seealso display
##' @examples
##' ##Not run: interactive only 
##' ##grabRect(boats)
##' ##grabRect(boats,TRUE)
##' @author Simon Barthelme
NULL

##' @rdname grab
##' @export 
grabLine <- function(im,output="coord")
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
        if (output=="coord")
        {
            cs
        }
        else if (output=="value")
        {
            out <- interp.line(im,cs[1],cs[2],cs[3],cs[4])
            attr(out,"coords") <- cs
            out
        }
        else
        {
            stop("Unknown output mode")
        }
    }
}

##' @rdname grab
##' @export 
grabRect <- function(im,output="coord")
{
    if (depth(im) > 1)
    {
        stop("This function does not work on images with depth > 1")
    }
    else
    {
        out <- select(im)
        cs <- c(out)[-c(3,6)] + 1
        names(cs) <- c("x0","y0","x1","y1")
        if (output=="coord")
        {
            cs
        }
        else if (output == "im" | output == "image")
        {
            out <- subim(im, x %inr% c(cs[1],cs[3]), y %inr% c(cs[2],cs[4]))
            attr(out,"coords") <- cs
            out
        }
        else if (output == "px" | output == "pixset")
        {
            out <- (Xc(im) %inr% c(cs[1],cs[3])) & (Yc(im) %inr% c(cs[2],cs[4]))
            attr(out,"coords") <- cs
            out
        }
        else
        {
            stop("Unknown output mode")
        }
    }
}

##' @rdname grab
##' @export 
grabPoint <- function(im,output="coord")
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
        if (output=="coord")
        {
            cs
        }
        else if (output=="value")
        {
            if (spectrum(im) > 1)
            {
                out <- color.at(im,cs[1],cs[2]) %>% setNames(c('r','g','b'))
            }
            else
            {
                out <- at(im,cs[1],cs[2])
            }
            attr(out,"coords") <- cs
            out
        }
        else if (output=="px" | output == "pixset")
        {
            out <- px.none(im)
            out[cs[1],cs[2],1,] <- TRUE
            out
        }
        else
        {
            stop("Unknown output mode")
        }
    }
}


##' Build simple interactive interfaces using imager
##'
##' To explore the effect of certain image manipulations, filter settings, etc., it's useful to have a basic interaction mechanism. You can use shiny for that, but imager provides a lightweight alternative. The user writes a function that gets called every time a user event happens (a click, a keypress, etc.). The role of the function is to process the event and output an image, which will then be displayed.
##' You can exit the interface at any time by pressing Esc.
##' See examples for more.
##' This feature is experimental!!!
##' @param fun a function that takes a single argument (a list of user events) and returns an image to be plotted. The image won't be rescaled before plotting, so make sure RGB values are in [0,1]. 
##' @param title a title for the window (default "", none)
##' @param init initial image to display (optional)
##' @return an image, specifically the last image displayed
##' @author Simon Barthelme
##' @examples
##' #Implement a basic image gallery:
##' #press "right" and "left" to view each image in a list
##' gallery <- function(iml)
##' {
##'     ind <- 1
##'     f <- function(state)
##'    {
##'         if (state$key=="arrowleft")
##'         {
##'             ind <<- max(ind-1,1)
##'         }
##'         if (state$key=="arrowright")
##'         {
##'             ind <<- min(ind+1,length(iml))
##'         }
##'         iml[[ind]]
##'     }
##'     interact(f)
##' }
##' ##Not run (interactive only)
##' ##map_il(1:10,~ isoblur(boats,.)) %>% gallery
##' @export
interact <- function(fun,title="",init)
{
    if (missing(init))
    {
        init <- imfill(1,1,val=rep(0,3))
    }
    interact_(fun,init=init,title=title)
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

