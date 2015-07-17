#include <imager.h>

using namespace Rcpp;
using namespace cimg_library;

//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSL(NumericVector im) {
  CId img = as<CId >(im);
  img.RGBtoHSL();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector HSLtoRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.HSLtoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSV(NumericVector im) {
  CId img = as<CId >(im);
  img.RGBtoHSV();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector HSVtoRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.HSVtoRGB();
  return wrap(img);
}




//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSI(NumericVector im) {
  CId img = as<CId >(im);
  img.RGBtoHSI();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector HSItoRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.HSItoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector RGBtosRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.RGBtosRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector sRGBtoRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.sRGBtoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector RGBtoYCbCr(NumericVector im) {
  CId img = as<CId >(im);
  img.RGBtoYCbCr();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector YCbCrtoRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.YCbCrtoRGB();
  return wrap(img);
}


//' @export
// [[Rcpp::export]]
NumericVector RGBtoYUV(NumericVector im) {
  CId img = as<CId >(im);
  img.RGBtoYUV();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector YUVtoRGB(NumericVector im) {
  CId img = as<CId >(im);
  img.YUVtoRGB();
  return wrap(img);
}

//' Convert an RGB image to grayscale 
//' 
//' This function converts from RGB to grayscale by first converting to HSL and keeping only the L channel
//' @return a grayscale image (spectrum == 1)
//' @export
// [[Rcpp::export]]
NumericVector grayscale(NumericVector im) {
  CId img = as<CId >(im);
  return wrap(img.RGBtoHSL().get_channel(2));
}


// NumericVector HSLtoRGB(NumericVector inp) {
//   //There's a bug in CImg with HSLtoRGB with CId  
//   CImg<float> img(as<CId >(inp));
//   img.HSLtoRGB();
//   CId out(img);
//   return wrap(out);
// }

