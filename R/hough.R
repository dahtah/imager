
##' Hough transform for lines
##'
##' Two algorithms are used, depending on the input: if the input is a pixset then the classical Hough transform is used. If the input is an image, then a faster gradient-based heuristic is used. 
##' The method returns either an image (the votes), or a data.frame. In both cases the parameterisation used is the Hesse normal form (theta,rho), where a line is represented as the set of values such that cos(theta)*x + sin(theta)*y = rho. Here theta is an angle and rho is a distance.
##' The image form returns a histogram of scores in (rho,theta) space, where good candidates for lines have high scores. The data.frame form may be more convenient for further processing in R: each line represents a pair (rho,theta) along with its score. 
##' If the 'shift' argument is true, then the image is assumed to start at x=1,y=1 (more convenient for plotting in R). If false, the image begins at x=0,y=0 and in both cases the origin is at the top left. 
##' @param im an image or pixset 
##' @param ntheta number of bins along theta (default 100)
##' @param data.frame return a data.frame? (default FALSE)
##' @param shift if TRUE, image is considered to begin at (x=1,y=1). 
##' @return either an image or a data.frame
##' @author Simon Barthelme
##' @examples
##'
##' #Find the lines along the boundary of a square
##' px <- px.square(30,80,80) %>% boundary
##' plot(px)
##' #Hough transform
##' hough_line(px,ntheta=200) %>% plot
##'
##' df <- hough_line(px,ntheta=800,data.frame=TRUE)
##' #Plot lines with the highest score
##' plot(px)
##' with(subset(df,score > quantile(score,.9995)),nfline(theta,rho,col="red"))
##'
##' plot(boats)
##' df <- hough_line(boats,ntheta=800,data=TRUE)
##' @export
hough_line <- function(im,ntheta=100,data.frame=FALSE,shift=TRUE)
{
    if (any(dim(im)[3:4]) > 1) stop('The Hough transform is only defined for 2D, grayscale images')
    #Bins for angle  
    theta.bins <- seq(0,2*pi,length=ntheta)
    if (is.pixset(im))
    {
        hl <- hough_line_px(im,theta.bins)
    }
    else if (is.cimg(im))
    {
        hl <- hough_line_grad(im,ntheta)
    }
    maxd <- sum(dim(im)[1:2]^2) %>% sqrt %>% ceiling %>% { .*2 }
    rho.bins <- seq(-maxd/2,maxd/2,l=maxd)
    if (data.frame)
    {
        theta <- theta.bins[Yc(hl)]
        rho <- rho.bins[Xc(hl)]
        if (shift) rho <- rho+cos(theta)+sin(theta)
        data.frame(theta=theta,rho=rho,score=as.vector(hl))
    }
    else
    {
        attr(hl,"rho.bins") <- rho.bins
        attr(hl,"theta.bins") <- theta.bins
        hl
    }
}

##' Plot a line, Hesse normal form parameterisation
##'
##' This is a simple interface over abline meant to be used along with the Hough transform.  In the Hesse normal form (theta,rho), a line is represented as the set of values (x,y) such that cos(theta)*x + sin(theta)*y = rho. Here theta is an angle and rho is a distance.
##' See the documentation for hough_lines. 
##' @param theta angle (radians)
##' @param rho distance
##' @param col colour
##' @param ... other graphical parameters, passed along to abline
##' @return nothing
##' @author Simon Barthelme
##' @examples
##' #Boring example, see ?hough_lines
##' plot(boats)
##' nfline(theta=0,rho=10,col="red")
##' @export
nfline <- function(theta,rho,col,...)
{
    if (length(theta)!=length(rho)) stop("rho and theta must be the same length")
    n <- length(theta)
    if (length(col)==1) col <- rep(col,n)
    for (ind in seq_along(theta))
        {
            if (theta[ind]==0)
            {
                abline(v=rho[ind],col=col[ind],...)
            }
            else
            {
                abline(rho[ind]/sin(theta[ind]),-cos(theta[ind])/sin(theta[ind]),col=col[ind],...)
            }
        }
    invisible()
}


##' Circle detection using Hough transform
##'
##' Detects circles of known radius in a pixset. The output is an image where the pixel value at (x,y) represents the amount of evidence for the presence of a circle of radius r at position (x,y). 
##' NB: in the current implementation, does not detect circles centred outside the limits of the pixset. 
##' @param px a pixset (e.g., the output of a Canny detector)
##' @param radius radius of circle 
##' @return a histogram of Hough scores, with the same dimension as the original image. 
##' @author Simon Barthelme
##' @examples
##' im <- load.example('coins')
##' px <- cannyEdges(im)
##' #Find circles of radius 20
##' hc <- hough_circle(px,20)
##' plot(hc)
##' #Clean up, run non-maxima suppression
##' nms <- function(im,sigma) { im[dilate_square(im,sigma) != im] <- 0; im}
##' hc.clean <- isoblur(hc,3) %>% nms(50)
##' #Top ten matches
##' df <- as.data.frame(hc.clean) %>%
##' dplyr::arrange(desc(value)) %>% head(10)
##' with(df,circles(x,y,20,fg="red",lwd=3))
##' @export
hough_circle <- function(px,radius)
{
    if (!is.pixset(px)) stop('Input must be a pixset')
    if (any(dim(px)[3:4]) > 1) stop('The Hough transform is only defined for 2D pixsets')
    hough_circle_(px,radius)
}

##' Add circles to plot
##'
##' Base R has a function for plotting circles called "symbols". Unfortunately, the size of the circles is inconsistent across devices. This function plots circles whose radius is specified in used coordinates. 
##' @param x centers (x coordinate)
##' @param y centers (y coordinate)
##' @param radius radius (in user coordinates)
##' @param bg background colour
##' @param fg foreground colour
##' @param ... passed to polygon, e.g. lwd
##' @return none, used for side effect
##' @seealso hough_circle
##' @author Simon Barthelme
##' @export
circles <- function(x,y,radius,bg=NULL,fg="white",...)
{
    xy <- xy.coords(x, y, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
    x <- xy$x
    y <- xy$y
    n <- length(x)
    if (length(bg)==1) bg <- rep(bg,n)
    if (length(fg)==1) fg <- rep(fg,n)
    if (length(radius)==1) radius <- rep(radius,n)

    for (ind in seq_along(x))
    {
        t <- seq(0,2*pi,l=max(150,5*radius))
        polygon(radius[ind]*cos(t)+x[ind],radius[ind]*sin(t)+y[ind],border=fg[ind],col=bg[ind],...)
    }
}
