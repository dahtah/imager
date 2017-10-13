
##' Image list
##'
##' An imlist object is simply a list of images (of class cimg). For convenience, some generic functions are defined that wouldn't work on plain lists, like plot, display and as.data.frame
##' DEPRECATION NOTE: in v0.30 of imager, the original behaviour of the "imlist" function was to take a list and turn it into an image list. This behaviour has now been changed to make "imlist" be more like "list". If you wish to turn a list into an image list, use as.imlist.  
##' @param ... images to be included in the image list
##' @export
##' @seealso plot.imlist, display.imlist, as.data.frame.imlist
##' @examples
##' imlist(a=imfill(3,3),b=imfill(10,10)) 
##' imsplit(boats,"x",6) 
##' imsplit(boats,"x",6) %>% plot
imlist <- function(...)
{
    list(...) %>% as.imlist.list
}
##' Check that an object is an imlist object
##' @param x an object
##' @return logical
##' @export
is.imlist <- function(x) is(x,"imlist")

##' @describeIn as.imlist convert from list
##' @export
as.imlist.list <- function(obj,...)
    {
        if (all(map_lgl(obj,is.cimg)) || all(map_lgl(obj,is.pixset)))
        {
            class(obj) <- c("imlist","list")
            obj
        }
        else
        {
             stop("The list may only contain images (objects with class cimg) or pixsets")
        }
    }


ilcat <- function(a,b)
{
    if (is.imlist(a) & is.imlist(b))
    {
        c(a,b) %>% as.imlist.list
    }
    else if (is.null(a))
        {
            as.imlist(b)
        }
    else if (is.null(b))
        {
            as.imlist(a)
        }
    else if (is.imlist(a) & is.cimg(b))
    {
        a[[length(a)+1]] <- b
        a
    }
    else  if (is.imlist(b) & is.cimg(a))
    {
        c(list(a),b) %>% as.imlist.list
    }
    else
    {
        stop("arguments should be image lists or images")
    }
}

##' Concatenation for image lists
##'
##' Allows you to concatenate image lists together, or images with image lists.
##' Doesn't quite work like R's "c" primitive: image lists are always *flat*, not nested, meaning each element of an image list is an image. 
##' @param ... objects to concatenate
##' @return an image list
##' @author Simon Barthelme
##' @examples
##'
##' l1 <- imlist(boats,grayscale(boats))
##' l2 <- imgradient(boats,"xy")
##' ci(l1,l2) #List + list
##' ci(l1,imfill(3,3)) #List + image
##' ci(imfill(3,3),l1,l2) #Three elements, etc.
##' @export
ci <- function(...)
    {
        l <- list(...)
        Reduce(ilcat,l,init=NULL)
    }

##' Convert various objects to image lists
##' 
##' @param obj an image list  
##' @param ... ignored
##' @return a list
##' @export
##' @examples
##' list(a=boats,b=boats*2) %>% as.imlist
as.imlist <- function(obj,...) UseMethod("as.imlist")

##' @export
as.list.imlist <- function(x,...) { class(x) <- "list"; x }

##' @describeIn as.imlist Convert from imlist (identity)
##' @export
as.imlist.imlist <- function(obj,...) obj

##' @describeIn as.imlist Convert from image
##' @export
as.imlist.cimg <- function(obj,...) list(obj) %>% as.imlist.list


##' Convert image list to data.frame
##'
##' @param x an image list (an imlist object)
##' @param index Name of the column containing the index (or name) of the image in the list. Default: "im"
##' @param ... Passed on to as.data.frame.cimg
##' @examples
##' #Transform the image gradient into a data.frame
##' gr <- imgradient(boats,"xy") %>% setNames(c("dx","dy")) %>% as.data.frame
##' str(gr)
##' @export
as.data.frame.imlist <- function(x,...,index="im")
{
    if (is.null(names(x))) names(x) <- 1:length(x)
    map_df(x,function(v) as.data.frame(v,...),.id=index)
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
##' Each image in the list will be plotted separately. The layout argument controls the overall layout of the plot window. The default layout is "rect", which will fit all of your images into a rectangle that's as close to a square as possible. 
##' @param x an image list (of type imlist)
##' @param layout either a matrix (in the format defined by the layout command) or one of "row","col" or "rect". Default: "rect"
##' @param ... other parameters, to be passed to the plot command
##' @examples
##' imsplit(boats,"c") #Returns an image list
##' imsplit(boats,"c") %>% plot
##' imsplit(boats,"c") %>% plot(layout="row")
##' imsplit(boats,"c") %>% plot(layout="col")
##' imsplit(boats,"x",5) %>% plot(layout="rect")
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
#' @param ... ignored
#' @examples
#' ##Not run: interactive only 
#' ## imgradient(boats,"xy") %>% display

#' @export
display.list <- function(x,...)
{
    display_list(x)
}

##' Type-stable map for use with the purrr package
##'
##' Works like purrr::map, purrr::map_dbl and the like but ensures that the output is an image list. 
##' @param ... passed to map
##' @return an image list
##' @author Simon Barthelme
##' @export
##' @examples
##' #Returns a list
##' imsplit(boats,"x",2) %>% purrr::map(~ isoblur(.,3))
##' #Returns an "imlist" object
##' imsplit(boats,"x",2) %>% map_il(~ isoblur(.,3))
##' #Fails if function returns an object that's not an image
##' try(imsplit(boats,"x",2) %>% map_il(~ . > 2))
##' #Parallel maps
##' map2_il(1:3,101:103,~ imshift(boats,.x,.y))
##' pmap_il(list(x=1:3,y=4:6,z=7:9),function(x,y,z) imfill(x,y,z))
map_il <- function(...)
{
    map(...) %>% as.imlist
}


##' @describeIn map_il Parallel map (two values)
##' @export
map2_il <- function(...)
{
    map2(...) %>% as.imlist
}

##' @describeIn map_il Parallel map (multiple values)
##' @export
pmap_il <- function(...)
{
    pmap(...) %>% as.imlist
}

#' @export
`[.imlist` <- function(x,...)
{
    out <- NextMethod()
    as.imlist(out)
}

##' Replicate images
##'
##' Kinda like rep, for images. Copy image n times and (optionally), append.
##' 
##' @param x an image
##' @param n number of replications 
##' @param axis axis to append along (one of NULL, "x","y","z","c"). Default: NULL
##' @return either an image or an image list
##' @author Simon Barthelme
##' @examples
##' #Result is a list
##' imrep(boats,3) %>% plot
##' #Result is an image 
##' imrep(boats,3,"x") %>% plot
##' #Make an animation by repeating each frame 10x
##' #map_il(1:5,~ isoblur(boats,.) %>% imrep(10,"z")) %>%
##' #                       imappend("z") %>% play
##' @export
imrep <- function(x,n=1,axis=NULL)
{
    if (n < 1) stop("n should be larger than 1") 
    l <- map_il(seq_len(n),~ x)
    if (!is.null(axis)) imappend(l,axis) else l
}
