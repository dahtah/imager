
inlineCxxPlugin <- Rcpp:::Rcpp.plugin.maker(
	include.before = "",
        include.after = "#include <CImg.h>\n#include <wrappers.h>\nusing namespace cimg_library;",
	libs           = "-lX11", 
	package        = "imager"
)
