#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"

using namespace Rcpp;
using namespace cimg_library;

//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSL(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.RGBtoHSL();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector HSLtoRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.HSLtoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSV(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.RGBtoHSV();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector HSVtoRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.HSVtoRGB();
  return wrap(img);
}




//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSI(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.RGBtoHSI();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector HSItoRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.HSItoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector RGBtosRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.RGBtosRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector sRGBtoRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.sRGBtoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector RGBtoYCbCr(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.RGBtoYCbCr();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector YCbCrtoRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.YCbCrtoRGB();
  return wrap(img);
}


//' @export
// [[Rcpp::export]]
NumericVector RGBtoYUV(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.RGBtoYUV();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector YUVtoRGB(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  img.YUVtoRGB();
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector grayscale(NumericVector inp) {
  CImg<double> img = as<CImg<double> >(inp);
  return wrap(img.RGBtoHSL().get_channel(2));
}


// NumericVector HSLtoRGB(NumericVector inp) {
//   //There's a bug in CImg with HSLtoRGB with CImg<double>  
//   CImg<float> img(as<CImg<double> >(inp));
//   img.HSLtoRGB();
//   CImg<double> out(img);
//   return wrap(out);
// }

