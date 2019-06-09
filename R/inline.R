#Sets up a Rcpp inline plugin 
#Much of this is adapted from RcppGSL by Romain Francois and Dirk Eddelbuettel
 
.pkgglobalenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    if (.Platform$OS.type=="windows") {
        imager_cflags <- "-Dcimg_r_mode -fpermissive"
        imager_libs   <- "-lgdi32"
    } else {
        imager_cflags <- "-Dcimg_r_mode -fpermissive -I/usr/X11R6/include -I/opt/X11/include"
        imager_libs   <- "-lX11 -L/usr/X11R6/lib -L/opt/X11/include"
    }
    assign("imager_cflags", imager_cflags, envir=.pkgglobalenv)
    assign("imager_libs", imager_libs, envir=.pkgglobalenv)
}

LdFlags <- function(print = TRUE) {
    if (print) cat(.pkgglobalenv$imager_libs) else .pkgglobalenv$imager_libs
}

CFlags <- function(print = TRUE) {
    if (print) cat(.pkgglobalenv$imager_cflags) else .pkgglobalenv$imager_cflags
}

inlineCxxPlugin <- function(...) {
    plugin <- Rcpp::Rcpp.plugin.maker(
        include.before = "#include <imager.h>",
        include.after = "using namespace cimg_library;",
	package        = "imager",
        Makevars = NULL, Makevars.win = NULL
    )
    settings <- plugin()
    settings$env$PKG_LIBS <- LdFlags(FALSE)
    settings$env$PKG_CPPFLAGS <- CFlags(FALSE)
    ## settings$configure <- readLines( system.file( "skeleton", "configure", package = "imager" ) )
    ## settings$configure.win <- readLines( system.file( "skeleton", "configure.win", package = "imager" ) )
    ## settings$Makevars.in <- readLines( system.file( "skeleton", "Makevars.in", package = "imager" ) )
    settings                            
}

