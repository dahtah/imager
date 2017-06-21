#' Bucket fill
#'
#' @param im an image
#' @param x X-coordinate of the starting point of the region to fill.
#' @param y Y-coordinate of the starting point of the region to fill.
#' @param z Z-coordinate of the starting point of the region to fill.
#' @param color a vector of values (of length spectrum(im)), or a colour name (e.g. "red"). If missing, use the colour at location (x,y,z).
#' @param sigma Tolerance for neighborhood values: spread to neighbours if difference is less than sigma (for grayscale). If there are several channels, the sum of squared differences is used: if it below sigma^2, the colour spreads. 
#' @param opacity opacity. If the opacity is below 1, paint with transparency. 
#' @param high_connexity Use 8-connexity (only for 2d images, default FALSE).
#' @export
#' @examples
#' #Change the colour of a sail 
#' boats.new <- bucketfill(boats,x=169,y=179,color="pink",sigma=.2) 
#' layout(t(1:2))
#' plot(boats,main="Original")
#' plot(boats.new,main="New sails")
#'
#' #More spreading, lower opacity, colour specified as vector
#' ugly <- bucketfill(boats,x=169,y=179,color=c(0,1,0),sigma=.6,opacity=.5)
#' plot(ugly)
#' @seealso px.flood
bucketfill <- function(im,x,y,z = 1,color,opacity=1,sigma=0,high_connexity=FALSE)
    {
        if (missing(color))
            {
                color <- color.at(im,x,y,z)
            }
        if (is.character(color))
        {
            color <- col2rgb(color)[,1]/255
        }
        if (length(color)!=spectrum(im))
        {
            if (spectrum(im)==1)
            {
                im <- add.colour(im)
                warning("Adding colour channels to image (was grayscale)")
            }
            else
            {
                stop('The color argument must be the same length as the number of color channels')
            }
        }
        bucket_fill(im,x,y,z,color,opacity,sigma,high_connexity)
    }


