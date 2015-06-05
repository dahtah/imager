#ifndef CIMG_WRAP
#define CIMG_WRAP
namespace Rcpp {
  template <> cimg_library::CImg<double> as(SEXP inp)
    {
      Rcpp::NumericVector Rvec(inp);
      IntegerVector d = Rvec.attr("dim");
      if (d.length() < 4)
	{
	  Rcpp::stop("Expecting a four-dimensional array");
	}
      cimg_library::CImg<double> img(Rvec.begin(),d[0],d[1],d[2],d[3],false);
      return img;		
    }
  //  template <> cimg_library::CImgList<double> as(SEXP inp);
  template <> SEXP wrap(const cimg_library::CImg<double> &img)
    {
      Rcpp::IntegerVector dims(4);
      dims[0] = img.width();
      dims[1] = img.height();
      dims[2] = img.depth();
      dims[3] = img.spectrum();
      Rcpp::NumericVector out(img.begin(),img.end());
      out.attr("class") = "cimg";
      out.attr("dim") = dims;
      return Rcpp::wrap(out);
    } 
  template <> SEXP wrap(const cimg_library::CImgList<double> &ilist); 
}

cimg_library::CImg<double> sharedCImg(SEXP inp);
cimg_library::CImgList<double> sharedCImgList(SEXP inp);
#endif
