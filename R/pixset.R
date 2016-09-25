#' @export
pixset <- function(X)
    {
        if (is.logical(X) && is.array(X) && (length(dim(X)) == 4 ))
            {
                class(X) <-c("pixset","logical")
                X
            }
        else
            {
                stop("A pixset must be a logical array with four dimensions")
            }
    }

#' @export
is.pixset <- function(x) is(x,"pixset")


#' @export
as.pixset <- function(x,...) UseMethod("as.pixset")
#' @export
as.pixset.logical <- function(x,...) pixset(x)
#' @export
as.pixset.cimg <- function(x,...) pixset(x!=0)
#' @export
as.cimg.pixset <- function(x,...) x+0
#' @export
as.logical.pixset <- function(x,...) { class(x) <- "logical"; x }
#' @export
as.data.frame.pixset <- function(x,...,drop=FALSE) {
    co <- coord.index(x,which(x))
    if (drop)
        {
            co[,dim(x)>1]
        }
    else
        {
            co
        }
}


##' @export
as.matrix.pixset <- function(x,...) {
    d <- dim(x)
    if (sum(d==1) == 2)
        {
            x <- squeeze(x)
            class(x) <- "matrix"
            x
        }
    else if (sum(d > 1) == 1)
    {
        warning("Image is one-dimensional")
        as.vector(x) %>% as.matrix
    }
    else
        {
            stop("Too many non-empty dimensions")
        }
}


size <- function(x) sum(x)


check.pixset <- function(x)
    {
        if (!is(x,"pixset"))
            {
                stop("Argument must be a pixel set")
            }
    }

#' @export
where <- function(x)
    {
        
        if (!is.pixset(x))
            {
                if (is.logical(x))
                    {
                        x <- as.pixset(x)
                    }
                else
                    {
                        stop("Argument must be a pixel set")
                    }
            }
        as.data.frame(x,drop=TRUE)
    }

#' @export
plot.pixset <- function(x,...) as.cimg(x) %>% plot(...)

#' @export
print.pixset <- function(x,...)
    {
        d <- dim(x)
        msg <- sprintf("Pixel set of size %i. Width: %i pix Height: %i pix Depth: %i Colour channels: %i \n",size(x),d[1],d[2],d[3],d[4])
        cat(msg)
        invisible(x)
    }

#' @export
Ops.cimg <- function(e1, e2)
{
    out <- NextMethod(.Generic)
    if (is.logical(out) && length(dim(out))==4)
        {
            as.pixset(out)
        }
    else
        {
            out
        }
}

#' @export
Ops.pixset <- function(e1, e2)
{
    
    e1 <- unclass(e1)
    if (!missing(e2))
        {
            e2 <- unclass(e2)
        }
    out <- NextMethod(.Generic)
    if (is.logical(out))
        {
            as.pixset(out)
        }
    else
        {
            as.cimg(out)
        }
}


#' Grow/shrink a pixel set
#'
#' Grow/shrink a pixel set through morphological dilation/erosion. The default is to use square or rectangular structuring elements, but an arbitrary structuring element can be given as input. 
#' A structuring element is a pattern to be moved over the image: for example a 3x3 square. In "shrink" mode, a element of the pixset is retained only if and only the structuring element fits entirely within the pixset. In "grow" mode, the structuring element acts like a neighbourhood: all pixels that are in the original pixset *or* in the neighbourhood defined by the structuring element belong the new pixset. 
#' @param px 
#' @param x either an integer value, or an image/pixel set. 
#' @param y width of the rectangular structuring element (if x is an integer value)
#' @param z depth of the rectangular structuring element (if x is an integer value)
#' @examples
#' #A pixel set:
#' a <- grayscale(boats) > .8
#' plot(a)
#' #Grow by a 8x8 square
#' grow(a,8) %>% plot
#' #Grow by a 8x2 rectangle
#' grow(a,8,2) %>% plot
#' #Custom structuring element
#' el <- matrix(1,2,2) %>% as.cimg
#' all.equal(grow(a,el),grow(a,2))
#' #Circular structuring element
#' 
#' @export
grow <- function(px,x,y=x,z=x)
    {
        if (is.cimg(x) || is.pixset(x))
            {
                bdilate(px,as.pixset(x))
            }
        else if (x==y && y==z)
            {
                bdilate_square(px,x)
            }
        else
            {
                bdilate_rect(px,x,y,z)
            }
    }


px.circle <- function(x,y,rad)
    {
        im <- imfill(x,y)
        (Xc(im)-(x+1)/2)^2 + (Yc(im)-(y+1)/2)^2 < rad^2
    }
