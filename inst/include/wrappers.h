//[[Rcpp::interfaces(r, cpp)]]

#ifndef CIMG_WRAP
#define CIMG_WRAP
namespace Rcpp {
  template <> cimg_library::CImg<double> as(SEXP inp);
  template <> cimg_library::CImgList<double> as(SEXP inp);
  template <> SEXP wrap(const cimg_library::CImg<double> &img); 
  template <> SEXP wrap(const cimg_library::CImgList<double> &ilist); 
}

cimg_library::CImg<double> sharedCImg(SEXP inp);
cimg_library::CImgList<double> sharedCImgList(SEXP inp);
#endif
