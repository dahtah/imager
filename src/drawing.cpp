#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

// [[Rcpp::export]]
NumericVector bucket_fill(NumericVector im,int x,int y,int z,NumericVector color,  float opacity=1,float sigma=0,bool high_connexity=false)
{
  CId img = as<CId >(im);
  img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,sigma,high_connexity);
  return wrap(img);
}

// [[Rcpp::export]]
NumericVector bucket_select(NumericVector im,int x,int y,int z,float sigma=0,bool high_connexity=false)
{
  CId img = as<CId >(im);
  CId out(img);
  NumericVector color(img.spectrum());
  float opacity=1;
  img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,out,sigma,high_connexity);
  return wrap(out);
}
