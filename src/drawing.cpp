#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

// [[Rcpp::export]]
NumericVector bucket_fill(NumericVector im,int x,int y,int z,NumericVector color,  float opacity=1,float sigma=0,bool high_connexity=false)
{
  CId img = as<CId >(im);
  try{
    img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,sigma,high_connexity);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
LogicalVector bucket_select(NumericVector im,int x,int y,int z,float sigma=0,bool high_connexity=false)
{
  CId img = as<CId >(im);
  CIb out(img,"xyzc");
  NumericVector color(img.spectrum());
  float opacity=1;

  try{
    img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,out,sigma,high_connexity);
  

  }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(out);
}
