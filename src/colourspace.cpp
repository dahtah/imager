#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"

using namespace Rcpp;
using namespace cimg_library;

//' @describeIn imager.colourspaces RGB to HSL conversion 
//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSL(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.RGBtoHSL();
  return wrap(img);
}

//' @describeIn imager.colourspaces HSL to RGB conversion 
//' @export
// [[Rcpp::export]]
NumericVector HSLtoRGB(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.HSLtoRGB();
  return wrap(img);
}

//' @describeIn imager.colourspaces RGB to HSV conversion 
//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSV(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.RGBtoHSV();
  return wrap(img);
}

//' @describeIn imager.colourspaces HSV to RGB conversion 
//' @export
// [[Rcpp::export]]
NumericVector HSVtoRGB(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.HSVtoRGB();
  return wrap(img);
}



//' @describeIn imager.colourspaces RGB to HSI conversion 
//' @export
// [[Rcpp::export]]
NumericVector RGBtoHSI(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.RGBtoHSI();
  return wrap(img);
}

//' @describeIn imager.colourspaces HSI to RGB conversion 
//' @export
// [[Rcpp::export]]
NumericVector HSItoRGB(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.HSItoRGB();
  return wrap(img);
}

//' @describeIn imager.colourspaces sRGB to RGB conversion 
//' @export
// [[Rcpp::export]]
NumericVector sRGBtoRGB(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.sRGBtoRGB();
  return wrap(img);
}

//' @describeIn imager.colourspaces RGB to YCbCr conversion 
//' @export
// [[Rcpp::export]]
NumericVector RGBtoYCbCr(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.RGBtoYCbCr();
  return wrap(img);
}

//' @describeIn imager.colourspaces YCbCr to RGB conversion 
//' @export
// [[Rcpp::export]]
NumericVector YCbCrtoRGB(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.YCbCrtoRGB();
  return wrap(img);
}

//' @describeIn imager.colourspaces RGB to YUV conversion 
//' @export
// [[Rcpp::export]]
NumericVector RGBtoYUV(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.RGBtoYUV();
  return wrap(img);
}

//' @describeIn imager.colourspaces YUV to RGB conversion 
//' @export
// [[Rcpp::export]]
NumericVector YUVtoRGB(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  img.YUVtoRGB();
  return wrap(img);
}

//' Convert an RGB image to grayscale 
//' 
//' This function converts from RGB to grayscale by first converting to HSL and keeping only the L channel
//' @param im an RGB image 
//' @return a grayscale image (spectrum == 1)
//' @export
// [[Rcpp::export]]
NumericVector grayscale(NumericVector im) {
  CImg<double> img = as<CImg<double> >(im);
  return wrap(img.RGBtoHSL().get_channel(2));
}


// NumericVector HSLtoRGB(NumericVector inp) {
//   //There's a bug in CImg with HSLtoRGB with CImg<double>  
//   CImg<float> img(as<CImg<double> >(inp));
//   img.HSLtoRGB();
//   CImg<double> out(img);
//   return wrap(out);
// }

