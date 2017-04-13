#' Bucket fill
#'
#' @param im an image
#' @param x X-coordinate of the starting point of the region to fill.
#' @param y Y-coordinate of the starting point of the region to fill.
#' @param z Z-coordinate of the starting point of the region to fill.
#' @param color Pointer to spectrum() consecutive values, defining the drawing color. If missing, use value at location (x,y,z)
#' @param sigma Tolerance concerning neighborhood values.
#' @param opacity Opacity of the drawing.
#' @param high_connexity Use 8-connexity (only for 2d images, default FALSE).
#' @export
#' @examples
#' #Change the colour of a sail 
#' boats.new <- bucketfill(boats,x=169,y=179,color=c(125,0,125),sigma=20) 
#' layout(t(1:2))
#' plot(boats,main="Original")
#' plot(boats.new,main="New sails")
#' @seealso px.flood
bucketfill <- function(im,x,y,z = 1,color,opacity=1,sigma=0,high_connexity=FALSE)
    {
        if (missing(color))
            {
                color <- color.at(im,x,y,z)
            }
        bucket_fill(im,x,y,z,color,opacity,sigma,high_connexity)
    }


