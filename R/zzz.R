##detect X11 on OS X, so that loading the library doesn't just fail with a mysterious message
##original code adapted from rgl package by Duncan Murdoch
.onLoad <- function(lib, pkg)
{
  dynlib <- "imager"
  unixos <- "none"
  
  if ( .Platform$OS.type == "unix" ) {
    unixos <- system("uname",intern=TRUE)
  }
  dll <- try(library.dynam(dynlib, pkg, lib))
  if (inherits(dll, "try-error"))
    stop(paste("\tLoading imager failed.", 
    	       if (unixos == "Darwin") 
    	         "\n\tOn MacOS, imager depends on XQuartz, which you can download from xquartz.org."),
         call. = FALSE)
}
