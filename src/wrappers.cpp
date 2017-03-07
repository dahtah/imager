#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;

// namespace Rcpp{
// //Convert a R cimg object to a C++ CImg object
// template <> cimg_library::CImg<double> as(SEXP inp) {
//     Rcpp::NumericVector Rvec(inp);
//     IntegerVector d = Rvec.attr("dim");
//     if (d.length() < 4)
//       {
// 	Rcpp::stop("Expecting a four-dimensional array");
//       }
//     cimg_library::CImg<double> img(Rvec.begin(),d[0],d[1],d[2],d[3],false);
//     return img;		
//   }
  

// //Convert a CImg object to an R cimg object
// template <> SEXP wrap(const cimg_library::CImg<double> &img) 
//   {
//     IntegerVector dims(4);
//     dims[0] = img.width();
//     dims[1] = img.height();
//     dims[2] = img.depth();
//     dims[3] = img.spectrum();
//     Rcpp::NumericVector out(img.begin(),img.end());
//     out.attr("class") = "cimg";
//     out.attr("dim") = dims;
//     return Rcpp::wrap(out);
//   }


//   //Convert a CImgList to an R list
//   template <> SEXP wrap(const cimg_library::CImgList<double> &ilist) 
//   {
//     Rcpp::List out(ilist.size());
//     for (int i = 0;i < ilist.size(); i++)
//       {
// 	out[i] = wrap(ilist(i));
//       }
//     return Rcpp::wrap(out);
//   }
// }

//Return a CImg object for in-place modification
CId sharedCImg(SEXP inp) {
    Rcpp::NumericVector Rvec(inp);
    IntegerVector d = Rvec.attr("dim");
    if (d.length() < 4)
      {
	Rcpp::stop("Expecting a four-dimensional array");
      }
    CId img(Rvec.begin(),d[0],d[1],d[2],d[3],true);
    return img;		
  }

CImg<int> sharedCImg_bool(SEXP inp) {
    Rcpp::LogicalVector Rvec(inp);
    IntegerVector d = Rvec.attr("dim");
    if (d.length() < 4)
      {
	Rcpp::stop("Expecting a four-dimensional array");
      }
    CImg<int> img(Rvec.begin(),d[0],d[1],d[2],d[3],true);
    return img;		
  }




//Return a CImg object for in-place modification
CImgList<double > sharedCImgList(SEXP inp) {
  Rcpp::List L(inp);
  int n = L.length();
  CImgList<double > CL;
  CImg<double > tmp;
  for (int i=0;i < n;i++)
    {
      CL.insert(sharedCImg(L[i]),i,true);
    }
  return CL;
}

//NB:: For some obscure reason R stores booleans as integers 
CImgList<int > sharedCImgList_bool(SEXP inp) {
  Rcpp::List L(inp);
  int n = L.length();
  CImgList<int > CL;
  CImg<int > tmp;
  for (int i=0;i < n;i++)
    {
      CL.insert(sharedCImg_bool(L[i]),i,true);
    }
  return CL;
}












