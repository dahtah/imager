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
##' 
##' @param im an image 
##' @param axis the axis along which to split (for example 'c')
##' @param nb number of objects to split into. 
##' if nb=-1 (the default) the maximum number of splits is used ie. split(im,"c") produces a list containing all individual colour channels
##' @seealso imappend (the reverse operation)
##' @examples
##' im <- as.cimg(function(x,y,z) x+y+z,10,10,5)
##' imsplit(im,"z") #Split along the z axis into a list with 5 elements
##' imsplit(im,"z",2) #Split along the z axis into two groups
##' imsplit(im,"z",2) %>% imappend("z") #Split and reshape into a single image
##' @export
imsplit <- function(im,axis,nb=-1)
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
##' 
##' @name imager.combine
##' @param x a list of images
##' @examples
##' im1 <- as.cimg(function(x,y) x,100,100)
##' im2 <- as.cimg(function(x,y) y,100,100)
##' im3 <- as.cimg(function(x,y) cos(x/10),100,100)
##' l <- list(im1,im2,im3)
##' add(l) %>% plot #Add the images
##' average(l) %>% plot #Average the images
##' mult(l) %>% plot #Multiply
##' parmax(l) %>% plot #Parallel max
##' parmin(l) %>% plot #Parallel min
##' #Edge detection
##' imgradient(boats,"xy") %>% enorm %>% plot
##' #Pseudo-artistic effects
##' llply(seq(1,35,5),function(v) boxblur(boats,v)) %>% parmin %>% plot
##' llply(seq(1,35,5),function(v) boxblur(boats,v)) %>% average %>% plot
##'
##' #At each pixel, which colour channel has the maximum value?
##' imsplit(boats,"c") %>% which.parmax %>% table
##' 
##' @author Simon Barthelme
##' @seealso imsplit,Reduce
NULL

##' @describeIn imager.combine Add images
##' @export
add <- function(x) Reduce("+", x)

##' @describeIn imager.combine Average images
##' @export
average <- function(x) Reduce("+", x)/length(x)

##' @describeIn imager.combine Multiply images (pointwise)
##' @export
mult <- function(x) Reduce("*", x)

##' @describeIn imager.combine Parallel max over images 
##' @export
parmax <- function(x) Reduce(pmax, x)

##' @describeIn imager.combine Parallel max in absolute value over images, 
##' @export
parmax.abs <- function(x) maxmin.abs(x,TRUE)
    

##' @describeIn imager.combine Parallel max in absolute value over images, 
##' @export
parmin.abs <- function(x) maxmin.abs(x,FALSE)


##' @describeIn imager.combine Parallel min over images 
##' @export
parmin <- function(x) Reduce(pmin, x)

##' @describeIn imager.combine Euclidean norm (i.e. sqrt(A^2 + B^2 + ...))
##' @export
enorm <- function(x) Map(function(v) v^2,x) %>% add %>% sqrt

##' @describeIn imager.combine index of parallel maxima
##' @export
which.parmax <- function(x) maxmin.ind(x,max=TRUE)

##' @describeIn imager.combine index of parallel minima
##' @export
which.parmin <- function(x) maxmin.ind(x,max=FALSE)



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
