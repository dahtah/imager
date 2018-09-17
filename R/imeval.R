
##' Evaluation in an image context
##'
##' imeval does for images what "with" does for data.frames, namely contextual evaluation. It provides various shortcuts for pixel-wise operations.
##' imdo runs imeval, and reshapes the output as an image of the same dimensions as the input (useful for functions that return vectors).
##' imeval takes inspiration from purrr::map in using formulas for defining anonymous functions using the "." argument.
##' Usage is made clear (hopefully) in the examples.
##' The old version of imeval used CImg's internal math parser, but has been retired.
##' @param obj an image, pixset or imlist
##' @param ...  one or more formula objects, defining anonymous functions that will be evaluated with the image as first argument (with extra contextual variables added to the evaluation context)
##' @param env additional variables (defaults to the calling environment)
##' @seealso imchange, which modifies specific parts of an image
##' @examples
##' ## Computing mean absolute deviation
##' imeval(boats, ~ mean(abs(.-median(.))))
##' ##Equivalent to:
##' mean(abs(boats-median(boats)))
##' ##Two statistics
##' imeval(boats,mad=  ~ mean(abs(.-median(.))),sd=  ~ sd(.))
##' ##imeval can precompute certain quantities, like the x or y coord. of each pixel
##' imeval(boats,~ x) %>% plot
##' ##same as Xc(boats) %>% plot
##' ## Other predefined quantities:
##' ##w is width, h is height
##' imeval(boats,~ x/w) %>% range
##' ##It defines certain transformed coordinate systems:
##' ##Scaled x,y,z
##' ## xs=x/w
##' ## ys=y/h
##' ##Select upper-left quadrant (returns a pixset)
##' imeval(boats,~ xs<.5 & ys < .5) %>% plot
##' ##Fade effect
##' imeval(boats,~ xs*.) %>% plot
##' ## xc and yc are another set of transformed coordinates
##' ## where xc=0,yc=0 is the image center
##' imeval(boats,~ (abs(xc)/w)*.) %>% plot
##'
##'##rho, theta: circular coordinates. rho is distance to center (in pix.), theta angle
##' ##Gaussian mask with sd 10 pix.
##' blank <- imfill(30,30)
##' imeval(blank,~ dnorm(rho,sd=w/3)) %>% plot(int=FALSE)
##' imeval(blank,~ theta) %>% plot
##' ##imeval is made for interactive use, meaning it
##' ##accesses the environment it got called from, e.g. this works: 
##' f <- function()
##' {
##'   im1 <- imfill(3,3,val=1)
##'    im2 <- imfill(3,3,val=3)
##' 
##'   imeval(im1,~ .+im2)
##' }
##' f()
##' ##imeval accepts lists as well 
##' map_il(1:3, ~ isoblur(boats,.)) %>%
##'    imeval(~ xs*.) %>%
##'    plot
##'
##' ##imeval is useful for defining pixsets:
##' ##here, all central pixels that have value under the median
##' grayscale(boats) %>%
##'     imeval(~ (. > median(.)) & rho < 150) %>%
##'     plot
##' ##other abbreviations are defined:
##' ##s for imshift, b for isoblur, rot for imrotate.
##' ##e.g.
##' imeval(boats, ~ .*s(.,3)) %>% plot
##' 
##' @author Simon Barthelme
##' @export
imeval <- function(obj,...,env=parent.frame())
{
    nms <- as.list(substitute(list(...))[-1]) %>% names
    args <- list(...)

    if (is.imlist(obj) || is.list(obj))
    {
        out <- map(obj,function(v) imeval(v,...,env=env))
        if (all(map_lgl(out,is.cimg)) || all(map_lgl(out,is.pixset))) as.imlist(out) else out
    }
    else
    {

        all.form <- map_lgl(args,is.formula) %>% all
        if (!all.form) stop("All arguments (except the first) need to be formulas")
        vars <- map(args,all.vars) %>% reduce(union)
        im <- obj
        newenv <- new.env(parent=env)
        newenv$. <- im

        add.variables(im,vars,newenv)

        eval.form <- function(fo) parse(text=as.character(fo)[2]) %>% eval(envir=newenv)
        if (length(args) > 1)
        {
            out <- map(args,eval.form) %>% setNames(nms)
            if (all(map_lgl(out,is.cimg)) || all(map_lgl(out,is.pixset))) as.imlist(out) else out
        }
        else
        {
            eval.form(args[[1]])
        }
    }
}


add.variables <- function(im,vars,env)
{
    x <- dim(im)[1]
    y <- dim(im)[2]
    z <- dim(im)[3]
    env$w <- as.integer(x)
    env$h <- as.integer(y)
    env$b <- isoblur
    env$s <- imshift
    env$rot <- imrotate
    if (missing(env))  env <- list()
    if (any(c("x","xc","xs","rho","theta") %in% vars)) env$x <- Xc(im)
    if (any(c("c") %in% vars)) env$c <- Cc(im)

    if (any(c("y","yc","ys","rho","theta") %in% vars)) env$y <- Yc(im)
    if (any(c("z","zc","zs") %in% vars)) env$z <- Zc(im)
    
    mx <- (x+1)/2;my <- (y+1)/2;mz <- (z+1)/2;
    if ("xc" %in% vars) env$xc <- env$x-mx
    if ("yc" %in% vars) env$yc <- env$y-my
    if ("zc" %in% vars) env$zc <- env$z-mz
    if ("xs" %in% vars) env$xs <- env$x/x
    if ("ys" %in% vars) env$ys <- env$y/y
    if ("zs" %in% vars) env$zs <- env$z/z
    if ("rho" %in% vars) env$rho <-sqrt( (env$x-mx)^2+(env$y-my)^2 )
    if ("theta" %in% vars) env$theta <-atan2( (env$y-my), (env$x-mx))
    
    invisible(env)
}


##' Modify parts of an image
##'
##' A shortcut for modifying parts of an image, using imeval syntax. See doc for imeval first. As part of a pipe, avoids the creating of intermediate variables. 
##' @param obj an image or imlist
##' @param where where to modify. a pixset, or a formula (in imeval syntax) that evaluates to a pixset. 
##' @param fo a formula (in imeval syntax) used to modify the image part
##' @param env evulation environment (see imeval)
##' @seealso imeval
##' @return a modified image
##' @examples
##' #Set border to 0:
##' imchange(boats,px.borders(boats,10),~ 0) %>% plot
##' #Eq. to
##' im <- boats
##' im[px.borders(im,10)] <- 0
##' #Using formula syntax
##' imchange(boats,~ px.borders(.,10),~ 0)
##' #Replace with grayscale ramp
##' imchange(boats,~ px.borders(.,10),~ xs) %>% plot
##' #Kill red channel in image
##' imchange(boats,~ c==1,~ 0) %>% plot
##' #Shit hue by an amount depending on eccentricity
##' load.example("parrots") %>%
##'   RGBtoHSL %>%
##'   imchange(~ c==1,~ .+80*exp(-(rho/550)^2) ) %>%
##'   HSLtoRGB %>%
##'   plot
##' 
##' @author Simon Barthelme
##' @export
imchange <- function(obj,where,fo,env=parent.frame())
{
    if (is.imlist(obj) || is.list(obj))
    {
        out <- map_il(obj,function(v) imchange(v,where,fo,env=env))
#        if (all(map_lgl(out,is.cimg)) || all(map_lgl(out,is.pixset))) as.imlist(out) else out
    }
    else if  (is.cimg(obj) || is.pixset(obj))
    {
    if (is.formula(where))
    {
        where <- imeval(obj,where)
    }
    
    if (is.pixset(where))
    {
        im <- obj
        newenv <- new.env()
        newenv$. <- im
        add.variables(im,all.vars(fo),newenv)
        for (var in names(newenv))
        {
            if (is.cimg(newenv[[var]]) || is.pixset(newenv[[var]]))
                {
                    newenv[[var]] <- newenv[[var]][where]
                }
        }
        fo <- parse(text=as.character(fo)[2])
        im[where] <- eval(fo,envir=newenv,enclos=env)
        im
    }
    else
    {
        stop('where argument must either be a pixset, or a formula that evaluates into a pixset')
    }
    }
    else
    {
        stop('object must be an image or pixset')
    }
    
}

is.formula <- function (x) inherits(x, "formula")

##'
##' @param form a single formula
##' @examples
##' #The rank function outputs a vector
##' grayscale(boats) %>% rank %>% class
##' #Auto-reshape into an image
##' grayscale(boats)  %>% imdo(~ rank(.)) %>% plot
##' #Note that the above performs histogram normalisation
##' 
##' #Also works on lists
##' imsplit(boats,"c") %>% imdo( ~ rank(.)) %>% imappend("c") %>% plot
##' @describeIn imeval run imeval and reshape
##' @export
imdo <- function(obj,form)
{
    out <- imeval(obj,form)
    if (is.list(out))
    {
        map2_il(out,obj,function(o,i) as.cimg(o,dim=dim(i)))
    }
    else 
    {
        as.cimg(out,dim=dim(obj))
    }
}
