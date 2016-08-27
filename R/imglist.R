
##' Image list
##'
##' An imlist object is simply a list of images (of class cimg).
##' 
##' @param l a list
##' @param ... ignored 
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

##' @describeIn imlist
##' @export
as.imlist.list <- imlist


##' @describeIn imlist
##' @export
as.list.imlist <- function(x,...) { class(x) <- "list"; x }

##' Convert image list to data.frame
##'
##' An column named "im"
##' @param x
##' @param index. Name of the colum containing the index (or name) of the image in the list. Default: "im"
##' @param ... Passed on to as.data.frame.cimg
##' @examples
##' #Transform the image gradient into a data.frame
##' gr <- imgradient(boats,"xy") %>% setNames(c("dx","dy")) %>% as.data.frame
##' str(gr)
##' @export
as.data.frame.imlist <- function(x,index="im",...)
{
    if (is.null(names(x))) names(x) <- 1:length(x)
    map_df(x,~ as.data.frame(.,...),.id=index)
}

##' @export
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
        matrix(1:n,sn,sn)
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
#' Click on individual images to zoom in.
#' 
#' @param x a list of cimg objects
#' @export
display.list <- function(x)
{
    display_list(x)
}

##' Type-stable map for use with the purrr package
##'
##' Works like purrr::map, purrr::map_dbl and the like but ensures that the output is an image list. 
##' @param ... passed to map
##' @return an image list
##' @author Simon Barthelme
##' @examples
##' #Returns a list
##' imsplit(boats,"x",2) %>% map(~ isoblur(.,3))
##' #Returns an "imlist" object
##' imsplit(boats,"x",2) %>% map_il(~ isoblur(.,3))
##' #Fails if function returns an object that's not an image
##' try(imsplit(boats,"x",2) %>% map_il(~ . > 2))
map_il <- function(...)
{
    map(...) %>% imlist
}
