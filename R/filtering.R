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
