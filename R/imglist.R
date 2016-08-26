
##' Image list
##'
##' An imlist object is simply a list of images (of class cimg). 
##' @param l a list
##' @export
##' @examples
##' #imsplit returns objects of class "imlist"
##' imsplit(boats,"c")
##' list(a=imfill(3,3),b=imfill(10,10)) %>% imlist
imlist <- function(l)
{
    if (!(map_lgl(l,is.cimg) %>% all))
    {
        stop("The list may only contain images (objects with class cimg)")
    }
    else
    {
        class(l) <- c("imlist","list")
        l
    }
}

##' @export
as.imlist.list <- imlist

##' @export
as.list.imlist <- function(x) { class(x) <- "list"; x }

##' @export
as.data.frame.imlist <- function(x)
{
    if (is.null(names(x))) names(x) <- 1:n
    map_df(x,as.data.frame,.id="im")
}


print.imlist <- function(x,...)
{
    msg <- sprintf("Image list of size %i \n",length(x))
    cat(msg)
    invisible(x)
}


##' Plot an image list
##'
##' Each image in the list will be plotted separately. The layout argument controls the overall layout of the plot window. 
##' @param x an image list (of type imlist)
##' @param layout either a matrix (in the format defined by the layout command) or one of "row","col" or "rect". Default: "rect"
##' @param ... other parameters, to be passed to the plot command
##' @examples
##' imsplit(boats,"c") #Returns an image list
##' 
##' @author Simon Barthelme
##' @export
plot.imlist <- function(x,layout="rect",...)
{
    n <- length(x)
    if (is.matrix(layout))
    {
        graphics::layout(layout)
    }
    else if (is.character(layout))
    {
        if (layout == "row")
        {
            graphics::layout(t(1:n))
        }
        else if (layout == "col")
        {
            graphics::layout(1:n)
        }
        else if (layout =="rect")
        {
            graphics::layout(rect.layout(n))
        }
        else
        {
            stop("Unknown layout")
        }
    }
    else 
    {
        stop("layout must be a matrix or string")
    }
        


    if (is.null(names(x))) names(x) <- 1:n
    map2(x,names(x),function(im,nm) plot(im,main=nm,...))
    graphics::layout(1)
}

rect.layout <- function(n)
{
    sn <- sqrt(n)
    if (sn == n)
    {
        layout(matrix(1:n,sn,sn))
    }
    else
    {
        dim <- rep(floor(sn),2)
        while (prod(dim) < n)
        {
            dim[which.min(dim)] <- dim[which.min(dim)]+1
        }
        matrix(1:prod(dim),dim[1],dim[2],byrow=TRUE)
    }
}

#' Display image list using CImg library
#'
#' @param imlist a list of cimg objects
#' @export
display.list <- function(im,rescale=TRUE)
{
    display_list(im,rescale)
}
