#' Sharpen image.
#'
#' The default sharpening filter is inverse diffusion. The "shock filter" is a non-linear diffusion that has better edge-preserving properties.
#'
#' @param im an image
#' @param amplitude Sharpening amplitude (positive scalar, 0: no filtering). 
#' @param type Filtering type. "diffusion" (default) or "shock"
#' @param edge Edge threshold (shock filters only, positive scalar, default 1).
#' @param alpha Window size for initial blur (shock filters only, positive scalar, default 0).
#' @param sigma Window size for diffusion tensor blur (shock filters only, positive scalar, default 0).
#'
#' @export
#' @examples
#' layout(t(1:2))
#' plot(boats,main="Original")
#' imsharpen(boats,150)  %>% plot(main="Sharpened")
imsharpen <- function(im,amplitude,type="diffusion",edge=1,alpha=0,sigma=0)
    {
        sharpen(im,amplitude,type=="shock",edge,alpha,sigma)
    }


#' Blur image isotropically.
#' @param im an image
#' @param sigma Standard deviation of the blur (positive)
#' @param neumann If true, use Neumann boundary conditions, Dirichlet otherwise  (default true, Neumann)
#' @param gaussian Use a Gaussian filter (actually van Vliet-Young). Default: 0th-order Deriche filter.
#' @param na.rm if TRUE, ignore NA values. Default FALSE, in which case the whole image is NA if one of the values is NA (following the definition of the Gaussian filter)
#' @seealso deriche,vanvliet,inpaint,medianblur
#' @export
#' @examples
#' isoblur(boats,3) %>% plot(main="Isotropic blur, sigma=3")
#' isoblur(boats,10) %>% plot(main="Isotropic blur, sigma=10")
isoblur <- function(im,sigma,neumann=TRUE,gaussian=TRUE,na.rm=FALSE)
{
    if (na.rm)
    {
        nas <- px.na(im)
        im[nas] <- 0
        isoblur_(im,sigma,neumann,gaussian)/isoblur_(1-nas,sigma,neumann,gaussian)
    }
    else
    {
        isoblur_(im,sigma,neumann,gaussian)
    }
}

##' Fill-in NA values in an image
##'
##' Fill in NA values (inpainting) using a Gaussian filter, i.e. replace missing pixel values with a weighted average of the neighbours. 
##' @param im input image
##' @param sigma std. deviation of the Gaussian (size of neighbourhood)
##' @return an image with missing values filled-in.
##' @examples
##' im <- boats
##' im[sample(nPix(im),1e4)] <- NA
##' inpaint(im,1) %>% imlist(im,.) %>%
##'    setNames(c("before","after")) %>% plot(layout="row")
##' @author Simon Barthelme
##' @export
inpaint <- function(im,sigma)
{
    px <- px.na(im)
    im[px] <- isoblur(im,sigma,na.rm=TRUE)[px]
    im
}
