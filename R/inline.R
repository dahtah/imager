
inlineCxxPlugin <- Rcpp:::Rcpp.plugin.maker(
	include.before = "#include <imager.h>",
        include.after = "using namespace cimg_library;",
	libs           = "-lX11", 
	package        = "imager"
)
