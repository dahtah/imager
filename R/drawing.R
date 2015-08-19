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
#' @seealso bucket_select
bucketfill <- function(im,x,y,z = 1,color,opacity=1,sigma=0,high_connexity=FALSE)
    {
        if (missing(color))
            {
                color <- color.at(im,x,y,z)
            }
        bucket_fill(im,x,y,z,color,opacity,sigma,high_connexity)
    }


#' Select a region of homogeneous colour 
#'
#' The underlying algorithm is the same as the bucket fill (AKA flood fill). Unlike with the bucket fill, the image isn't changed, the function simply returns a binary mask of the selected region
#'
#' @param im an image
#' @param x X-coordinate of the starting point of the region to fill.
#' @param y Y-coordinate of the starting point of the region to fill.
#' @param z Z-coordinate of the starting point of the region to fill.
#' @param sigma Tolerance concerning neighborhood values.
#' @param high_connexity Use 8-connexity (only for 2d images, default FALSE).
#' @export
#' @examples
#' #Select part of a sail 
#' impart <- selectSimilar(boats,x=169,y=179,sigma=20) 
#' layout(t(1:2))
#' plot(boats,main="Original")
#' plot(impart,main="Selected region")
#' @seealso bucketfill
#' @export
selectSimilar <- function(im,x,y,z=1,sigma=0,high_connexity=FALSE)
    {
        bucket_select(im,x,y,z,sigma,high_connexity)

    }
