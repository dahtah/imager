#' Form an adjacency graph from a pixset
#'
#' Return a graph where nodes are pixels, and two nodes are connected if and only if both nodes are in the pixset, and the pixels are adjacent. Optionnally, add weights corresponding to distance (either 1 or sqrt(2), depending on the orientation of the edge).
#' The graph is represented as an igraph "graph" object
#' @param x a pixset
#' @param weighted add weight for distance (default TRUE)
#' @param ... ignored
#' @return an igraph "graph" object
#' @seealso as.igraph.cimg
#' @examples
#' library(igraph)
#' #Simple 3x3 lattice
#' px <- px.all(imfill(3,3))
#' as.igraph(px) %>% plot
#' #Disconnect central pixel
#' px[5] <- FALSE
#' as.igraph(px) %>% plot
#' #Form graph from thresholded image
#' im <- load.example("coins")
#' px <- threshold(im) %>% fill(5)
#' G <- as.igraph(px)
#' #Label connected components
#' v <- (igraph::clusters(G)$membership)
#' as.cimg(v,dim=dim(px)) %>% plot
#' #Find a path across the image that avoids all
#' #the coins
#' G <- as.igraph(!px)
#' start <- index.coord(im,data.frame(x=1,y=100))
#' end <- index.coord(im,data.frame(x=384,y=300))
#' sp <- igraph::shortest_paths(G,start,end,output="vpath")
#' path <- sp$vpath[[1]] %>% as.integer %>% coord.index(im,.)
#' @export
as.igraph.pixset <- function(x,weighted=TRUE,...)
{
    px <- x
    if (any(dim(px)[3:4] > 1)) stop('Only defined for 2D pixsets')
    bg <- bgraph(px)
    G <- cbind(bg$ii,bg$jj) %>% { igraph::graph_from_edgelist(.,directed=FALSE) }
    if (weighted)
    {
        igraph::E(G)$weight <- bg$dst
    }
    #Complete graph if necessary
    mxind <- igraph::V(G) %>% as.integer %>% max
    if (mxind < nPix(px))
        {
            G <- igraph::add_vertices(G,nPix(px)-mxind)
        }
    G
}

##' Form a graph from an image
##'
##' In this graph representation, every pixel is a vertex connected to its neighbours.
##' The image values along edges are stored as graph attributes (see examples).
##' @param x an image (must be 2D, 3D not implemented yet)
##' @param mask optional: a pixset. if provided, pixels are only connected if they are both in the pixset.
##' @param ... ignored
##' @return a graph (igraph format) with attributes value.from, value.to and dist
##' @author Simon Barthelme
##' @seealso as.igraph.pixset
##' @examples
##' library(igraph)
##' im <- imfill(5,5)
##' G <- as.igraph(im)
##' plot(G)
##' #Shortest-path distance from pixel 1 to all other pixels
##' d <- igraph::distances(G,1) %>% as.vector
##' as.cimg(d,dim=gsdim(im)) %>% plot(interpolate=FALSE)
##' #Notice that moving along the diagonal has the same cost
##' #as moving along the cardinal directions, whereas the Euclidean distance
##' #is actually sqrt(2) and not 1. 
##' #Modify weight attribute, to change the way distance is computed
##' igraph::E(G)$weight <- G$dist
##' d2 <- igraph::distances(G,1) %>% as.vector
##' as.cimg(d2,dim=gsdim(im)) %>% plot(interpolate=FALSE)
##' #More interesting example
##' im <- grayscale(boats)
##' G <- as.igraph(im)
##' #value.from holds the value of the source pixel, value.to the sink's
##' #set w_ij = (|v_i - v_j|)/d_ij
##' igraph::E(G)$weight <- (abs(G$value.from - G$value.to))/G$dist
##' igraph::distances(G,5000) %>% as.vector %>%
##'     as.cimg(dim=gsdim(im)) %>% plot
##' @export
as.igraph.cimg <- function(x,mask=px.all(channel(im,1)),...)
{
    im <- x
    if (any(dim(mask)[1:2] !=dim(im)[1:2])) {
        stop('image and mask must have the same x,y dimensions')
    }
    if (depth(im) > 1) stop('Only implemented for 2D images (so far)')
    if (spectrum(im) > 1)
    {
        if (spectrum(mask) > 1)
        {
            warning("mask has colour channels, using OR reduction")
            mask <- imsplit(mask,"c") %>% parany
        }
    }
    bg <- bgraph(mask)
    G <- cbind(bg$ii,bg$jj) %>% { igraph::graph_from_edgelist(.,directed=FALSE) }
    G$value.to <- imsplit(im,"c") %>% map(~ .[bg$jj]) %>% do.call(cbind,.)
    G$value.from <- imsplit(im,"c") %>% map(~ .[bg$ii]) %>% do.call(cbind,.)
    G$dist <- bg$dst
    mxind <- igraph::V(G) %>% as.integer %>% max
    n <- nPix(im)/spectrum(im)
    if (mxind < n)
        {
            G <- igraph::add_vertices(G,n-mxind)
        }
    G
}

graph.sim <- function(G,reg=1)
{
    igraph::E(G)$weight <- G$dist/(reg+sqrt(rowSums((G$value.from - G$value.to)^2)))
    G
}
