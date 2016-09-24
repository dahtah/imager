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

as.pixset <- function(x,...) UseMethod("as.imlist")
as.pixset.logical <- function(x,...) pixset(x)
as.pixset.cimg <- function(x,...) pixset(x!=0)
as.cimg.pixset <- function(x,...) as.cimg.logical(x)

size <- function(x) sum(x)

