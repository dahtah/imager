##' Apply function to each element of a list, then combine the result as an image by appending along specified axis
##'
##' This is just a shortcut for llply followed by imappend
##' @param lst a list
##' @param fun function to apply
##' @param axis which axis to append along (e.g. "c" for colour)
##' @param ... further arguments to be passed to fun
##' @examples
##' build.im <- function(size) as.cimg(function(x,y) (x+y)/size,size,size)
##' liply(c(10,50,100),build.im,"y") %>% plot
##' @export
liply <- function(lst,fun,axis,...)
    {
        llply(lst,fun,...) %>% imappend(axis=axis)
    }


##' Split an image along axis, apply function, return a list
##'
##' Shorthand for imsplit followed by llply
##' @param im image
##' @param axis axis for the split (e.g "c")
##' @param fun function to apply
##' @param ... extra arguments for function fun
##' @examples
##' parrots <- load.example("parrots")
##' ilply(parrots,"c",mean) #mean luminance per colour channel
##' @export
ilply <- function(im,axis,fun,...)
    {
        imsplit(im,axis) %>% llply(fun,...) 
    }
##' Split an image along axis, apply function, return a data.frame
##'
##' Shorthand for imsplit followed by ldply
##' @param im image
##' @param axis axis for the split (e.g "c")
##' @param fun function to apply
##' @param ... extra arguments to function fun
##' @examples
##' idply(boats,"c",mean) #mean luminance per colour channel
##' @export
idply <- function(im,axis,fun,...)
    {
        imsplit(im,axis) %>% ldply(fun,...) 
    }

##' Split an image, apply function, recombine the results as an image
##'
##' This is just imsplit followed by llply followed by imappend
##' 
##' @param im image 
##' @param axis axis for the split (e.g "c")
##' @param fun function to apply
##' @param ... extra arguments to function fun
##' @examples
##' ##' #Normalise colour channels separately, recombine
##' iiply(boats,"c",function(v) (v-mean(v))/sd(v)) %>% plot 
##' 
##' @export
iiply <- function(im,axis,fun,...)
    {
        imsplit(im,axis) %>% llply(fun,...) %>% imappend(axis=axis)
    }

##' Split an image along a certain axis (producing a list)
##'
##' Use this if you need to process colour channels separately, or frames separately, or rows separately, etc. You can also use it to chop up an image into blocks.
##' Returns an "imlist" object, which is essentially a souped-up list. 
##' @param im an image 
##' @param axis the axis along which to split (for example 'c')
##' @param nb number of objects to split into. 
##' if nb=-1 (the default) the maximum number of splits is used, i.e. split(im,"c") produces a list containing all individual colour channels.
## if nb = -px, with px > 1 split into blocks of px pixels.
##' @seealso imappend (the reverse operation)
##' @examples
##' im <- as.cimg(function(x,y,z) x+y+z,10,10,5)
##' imsplit(im,"z") #Split along the z axis into a list with 5 elements
##' imsplit(im,"z",2) #Split along the z axis into two groups
##' imsplit(boats,"x",-200) %>% plot #Blocks of 200 pix. along x
##' imsplit(im,"z",2) %>% imappend("z") #Split and reshape into a single image
##' #You can also split pixsets
##' imsplit(boats > .5,"c") %>% plot
##' @export
imsplit <- function(im,axis,nb=-1)
{
    if (is.cimg(im))
    {
        l <- im_split(im,axis,nb)
    }
    else if (is.pixset(im))
    {
        l <- px_split(im,axis,nb)
    }
    else
    {
        stop("im must be either an image or pixset")
    }
    d.ind <- index.coords[[axis]]
    d <- dim(im)
    if (nb!=-1)
    {
        b.end <- laply(l,function(v) dim(v)[d.ind]) %>% cumsum
        b.start <- c(1,b.end[-length(l)]+1)
        b.str <- sprintf("= %i - %i",b.start,b.end)
        names(l) <- paste(axis,b.str)
    }
    else
    {
        names(l) <- paste(axis,1:length(l),sep=" = ")
    }
    l
}


imsplit.recur <- function(im,spl,nb=-1)
    {
        if (length(spl) > 1)
            {
                imsplit.recur(im,spl[[1]]) %>% llply(imsplit.recur,spl=spl[-1])
            }
        else
            {
                l <- im_split(im,axis,nb)
                d.ind <- index.coords[[axis]]
                d <- dim(im)
                if (nb!=-1)
                    {
                        b.end <- laply(l,function(v) dim(v)[d.ind]) %>% cumsum
                        b.start <- c(1,b.end[-length(l)]+1)
                        b.str <- sprintf("= %i - %i",b.start,b.end)
                        names(l) <- paste(axis,b.str)
                    }
                else
                    {
                        names(l) <- paste(axis,1:length(l),sep=" = ")
                    }
                l
            }
    }

##' Combining images
##'
##' These functions take a list of images and combine them by adding, multiplying, taking the parallel min or max, etc.
##' The max. in absolute value of (x1,x2) is defined as x1 if (|x1| > |x2|), x2 otherwise. It's useful for example in getting the most extreme value while keeping the sign. 
##' "parsort","parrank" and "parorder" aren't really reductions because they return a list of the same size. They perform a pixel-wise sort (resp. order and rank) across the list.
##' parvar returns an unbiased estimate of the variance (as in the base var function). parsd returns the square root of parvar. 
##' @name imager.combine
##' @param x a list of images
##' @param na.rm ignore NAs (default FALSE)
##' @examples
##' im1 <- as.cimg(function(x,y) x,100,100)
##' im2 <- as.cimg(function(x,y) y,100,100)
##' im3 <- as.cimg(function(x,y) cos(x/10),100,100)
##' l <- imlist(im1,im2,im3)
##' add(l) %>% plot #Add the images
##' average(l) %>% plot #Average the images
##' mult(l) %>% plot #Multiply
##' wsum(l,c(.1,8,.1)) %>% plot #Weighted sum
##' parmax(l) %>% plot #Parallel max
##' parmin(l) %>% plot #Parallel min
##' parmed(l) %>% plot #Parallel median
##' parsd(l) %>% plot #Parallel std. dev
##' #parsort can also be used to produce parallel max. and min
##' (parsort(l)[[1]]) %>% plot("Parallel min")
##' (parsort(l)[[length(l)]]) %>% plot("Parallel max")
##' #Edge detection (Euclidean norm of gradient)
##' imgradient(boats,"xy") %>% enorm %>% plot
##' #Pseudo-artistic effects
##' l <- map_il(seq(1,35,5),~ boxblur(boats,.))
##' parmin(l) %>% plot
##' average(l) %>% plot
##' mult(l) %>% plot
##' #At each pixel, which colour channel has the maximum value?
##' imsplit(boats,"c") %>% which.parmax %>% table
##' #Same thing using parorder (ties are broken differently)!!!
##' imsplit(boats,"c") %>% { parorder(.)[[length(.)]] } %>% table
##' @author Simon Barthelme
##' @seealso imsplit,Reduce
NULL

check.reduce <- function(l)
{
    l <- as.imlist(l)
    if (length(l) > 1) #Check dimensions
    {
        ok <- sapply(l,dim) %>% { apply(.,1,stats::sd) == 0 } %>% all
        if (!ok)
        {
            stop("Images must all be the same size")
        }
    }
    l
}


##' @describeIn imager.combine Add images
##' @export
add <- function(x,na.rm=FALSE) {
    x <- check.reduce(x)
    reduce_wsum(x,rep(1,length(x)),na_rm=na.rm)
}

##' @describeIn imager.combine Weighted sum of images
##' @param w weights (must be the same length as the list)
##' @export
wsum <- function(x,w,na.rm=FALSE)
{
    if (length(w)!=length(x)) stop("weights must the same length as input")
    check.reduce(x) %>% reduce_wsum(w,na_rm=na.rm)
}


##' @describeIn imager.combine Average images
##' @export
average <- function(x,na.rm=FALSE) check.reduce(x) %>% reduce_average(na_rm=na.rm)


##' @describeIn imager.combine Multiply images (pointwise)
##' @export
mult <- function(x,na.rm=FALSE) check.reduce(x) %>% reduce_prod(na_rm=na.rm)

##' @describeIn imager.combine Parallel max over images 
##' @export
parmax <- function(x,na.rm=FALSE) check.reduce(x) %>% reduce_minmax(na_rm=na.rm,max=TRUE)

##' @describeIn imager.combine Parallel max in absolute value over images, 
##' @export
parmax.abs <- function(x) maxmin.abs(x,TRUE)
    

##' @describeIn imager.combine Parallel min in absolute value over images, 
##' @export
parmin.abs <- function(x) maxmin.abs(x,FALSE)


##' @describeIn imager.combine Parallel min over images 
##' @export
parmin <- function(x,na.rm=FALSE) check.reduce(x) %>% reduce_minmax(na_rm=na.rm,max=FALSE)


##' @describeIn imager.combine Euclidean norm (i.e. sqrt(A^2 + B^2 + ...))
##' @export
enorm <- function(x) check.reduce(x) %>% reduce_list(5)

##' @describeIn imager.combine Median
##' @export
parmed <- function(x,na.rm=FALSE) check.reduce(x) %>% reduce_med(na_rm=na.rm)

##' @describeIn imager.combine Variance
##' @export
parvar <- function(x) check.reduce(x) %>% reduce_list(4)

##' @describeIn imager.combine Std. deviation 
##' @export
parsd <- function(x) parvar(x) %>% sqrt


##' @describeIn imager.combine Parallel all (for pixsets)
##' @export
parall <- function(x) check.reduce(x) %>% Reduce(function(a,b) a & b,.)

##' @describeIn imager.combine Parallel any (for pixsets)
##' @export
parany <- function(x) check.reduce(x) %>% Reduce(function(a,b) a | b,.)


##' @describeIn imager.combine Test equality
##' @export
equal <- function(x)
{
    if (length(x) == 1)
    {
        stop("x has only one element")
    }
    else
    {
        acc <- px.all(x[[1]])
        v <- x[[1]]
        x <- x[-1]
        for (xv in x)
        {
            acc[xv!=v] <- FALSE
        }
        acc
    }
}

##' @describeIn imager.combine index of parallel maxima
##' @export
which.parmax <- function(x) maxmin.ind(x,max=TRUE)

##' @describeIn imager.combine index of parallel minima
##' @export
which.parmin <- function(x) maxmin.ind(x,max=FALSE)


##' @describeIn imager.combine pixel-wise sort
##' @param increasing if TRUE, sort in increasing order (default TRUE)
##' @export
parsort <- function(x,increasing=TRUE) check.reduce(x) %>% psort(increasing)

##' @describeIn imager.combine pixel-wise order 
##' @export
parorder <- function(x,increasing=TRUE) check.reduce(x) %>% porder(increasing)

##' @describeIn imager.combine pixel-wise rank
##' @export
parrank <- function(x,increasing=TRUE) check.reduce(x) %>% prank(increasing)





maxmin.abs <- function(L,max=TRUE)
{
    n <- length(L)
    cmax <- abs(L[[1]])
    out <- L[[1]]
    for (ind in 2:n)
    {
        aL <- abs(L[[ind]])
        if (max)
        {
            v <- aL > cmax
        }
        else
        {
            v <- aL < cmax
        }
        out[v] <- L[[ind]][v]
        cmax[v] <- aL[v]
    }
    out
}


maxmin.ind <- function(L,max=TRUE)
{
    n <- length(L)
    pind <- L[[1]]*0 + 1
    cmax <- L[[1]]
    for (ind in 2:n)
    {
        if (max)
        {
            v <- L[[ind]] > cmax
        }
        else
        {
            v <- L[[ind]] < cmax
        }
        pind[v] <- ind
        cmax[v] <- L[[ind]][v]
    }
    pind
}

#' Combine a list of images into a single image 
#' 
#' All images will be concatenated along the x,y,z, or c axis.
#' 
#' @param imlist a list of images (all elements must be of class cimg) 
#' @param axis the axis along which to concatenate (for example 'c')
#' @seealso imsplit (the reverse operation)
#' @export
#' @examples
#' imappend(list(boats,boats),"x") %>% plot
#' imappend(list(boats,boats),"y") %>% plot
#' plyr::rlply(3,imnoise(100,100)) %>% imappend("c") %>% plot
#' boats.gs <- grayscale(boats)
#' plyr::llply(seq(1,5,l=3),function(v) isoblur(boats.gs,v)) %>% imappend("c") %>% plot
#' #imappend also works on pixsets
#' imsplit(boats > .5,"c") %>% imappend("x") %>% plot
##' @export 
imappend <- function(imlist,axis)
{
    if (all(map_lgl(imlist,is.cimg)))
    {
        im_append(imlist,axis)
    }
    else if (all(map_lgl(imlist,is.pixset)))
    {
        px_append(imlist,axis)
    }
    else
    {
        stop("List contains unknown image type (must be all images, or all pixsets)")
    }
}
