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
    if (is.logical(out))
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

grow <- function(px,x)
    {
        if (length(x) == 1 && is.numeric(x))
            {
                bdilate_square(px,x)
            }
#        else if (length(x) == 2 
    }
