#' Rotate an image along the XY plane.  
#'
#' If cx and cy aren't given, the default is to centre the rotation in the middle of the image. When cx and cy are given, the algorithm used is different, and does not change the size of the image.
#' @param im an image
#' @param angle Rotation angle, in degrees.
#' @param cx Center of rotation along x (default, image centre)
#' @param cy Center of rotation along y (default, image centre)
#' @param interpolation Type of interpolation. One of 0=nearest,1=linear,2=cubic. 
#' @param boundary Boundary conditions. One of 0=dirichlet, 1=neumann, 2=periodic
#' @seealso imwarp, for flexible image warping, which includes rotations as a special case 
#' @examples
#' imrotate(boats,30) %>% plot
#' #Shift centre to (20,20)
#' imrotate(boats,30,cx=20,cy=20) %>% plot 
#' @export
imrotate <- function (im, angle, cx,cy,interpolation = 1L, boundary = 0L) 
{
    if (missing(cx) && missing(cy))
    {
        rotate(im,angle,interpolation,boundary)
    }
    else if (!missing(cx) && !missing(cy))
    {
        rotate_xy(im,angle,cx,cy,interpolation,boundary)
    }
    else
    {
        stop("You must specify both cx and cy")
    }
}

