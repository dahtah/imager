##' Interpolate image values
##'
##' This function provides 2D and 3D (linear or cubic) interpolation for pixel values.
##' Locations need to be provided as a data.frame with variables x,y,z, and c (the last two are optional). 
##' @param im the image (class cimg)
##' @param locations a data.frame 
##' @param cubic if TRUE, use cubic interpolation. If FALSE, use linear (default FALSE)
##' @examples
##' 
##' loc <- data.frame(x=runif(10,1,width(boats)),y=runif(10,1,height(boats))) #Ten random locations
##' interp(boats,loc)
##' @export
interp <- function(im,locations,cubic=FALSE)
{
    if (!check.inside(im,locations))
        {
            stop("Some locations are outside the image")
        }
    if (!is.data.frame(locations))
        {
            stop('Second argument must be a data.frame')
        }
    if (any(names(locations) == "cc"))
    {
        locations <- plyr::rename(locations,c("cc"="c")) 
    }

    nms <- intersect(names(locations),c("x","y","z","c"))

    if (setequal(nms,c("x","y")))
        {
            if (depth(im) == 1 & spectrum(im) ==1)
                {
                    interp_xy(im,locations$x-1,locations$y-1,cubic=cubic)
                }
            else if (depth(im) != 1)
                {
                    frames(im) %>% llply(interp,locations=locations,cubic=cubic)
                }
            else if (spectrum(im) != 1)
                {
                    channels(im) %>% llply(interp,locations=locations,cubic=cubic)
                }
        }
    else if (setequal(nms,c("x","y","z")))
        {
            if (spectrum(im) ==1)
                {
                    interp_xyz(im,locations$x-1,locations$y-1,locations$z - 1,cubic=cubic)
                }
            else
                {
                      channels(im) %>% llply(interp,locations=locations,cubic=cubic)
                }
        }
        else if (setequal(nms,c("x","y","c")))
        {
            if (depth(im) ==1)
                {
                    interp_xyc(im,ix=locations$x-1,iy=locations$y-1,ic=locations$c + 1,cubic=cubic)
                }
            else
                {
                      frames(im) %>% llply(interp,locations=locations,cubic=cubic)
                }
        }
    else if (all(c("x","y","z","c") %in% nms))
        {
            interp_xyzc(im,ix=locations$x-1,iy=locations$y-1,iz=locations$z-1,ic=locations$c-1,cubic=cubic)
        }
    else
        {
            stop('Unrecognised coordinates (use x,y,z,c to index the locations)')
        }
}


check.inside <- function(im,locations)
{
    if (any(names(locations) == "cc"))
    {
        locations <- plyr::rename(locations,c("cc"="c")) 
    }
    check <- list(x = function(x) { (x >= 1) & (x <= width(im)) },
                  y = function(y) { (y >= 1) & (y <= height(im)) },
                  z = function(z) { (z >= 1) & (z <= depth(im)) },
                  c = function(c) { (c >= 1) & (c <= spectrum(im)) })
    nms <- intersect(names(locations),c("x","y","z","c"))
    ## if (!all(c("x","y","z","c") %in% nms))
    ## {
    ##     stop('Unrecognised coordinates (use x,y,z,c to index the locations)')
    ## }
    ##     else
    ##     {
    A <- laply(nms,function(nm) check[[nm]](locations[[nm]]))
    all(A)
##        }                               
}
