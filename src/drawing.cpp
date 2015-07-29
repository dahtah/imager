#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

//' Bucket fill
//'       @param im an image
//' @param x X-coordinate of the starting point of the region to fill.
//' @param y Y-coordinate of the starting point of the region to fill.
//' @param z Z-coordinate of the starting point of the region to fill.
//' @param color Pointer to spectrum() consecutive values, defining the drawing color.
//' @param sigma Tolerance concerning neighborhood values.
//' @param opacity Opacity of the drawing.
//' @param is_high_connexity Use 8-connexity (only for 2d images).
//' @export
// [[Rcpp::export]]
NumericVector bucket_fill(NumericVector im,int x,int y,int z,NumericVector color,  float opacity=1,float sigma=0,bool is_high_connexity=false)
{
  CId img = as<CId >(im);
  img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,sigma,is_high_connexity);
  return wrap(img);
}


//' Select a region of homogeneous colour 
//'
//' The underlying algorithm is the same as the bucket fill (AKA flood fill). Unlike with the bucket fill, the image isn't changed, the function simply returns a binary mask of the selected region
//'
//'       @param im an image
//' @param x X-coordinate of the starting point of the region to fill.
//' @param y Y-coordinate of the starting point of the region to fill.
//' @param z Z-coordinate of the starting point of the region to fill.
//' @param sigma Tolerance concerning neighborhood values.
//' @param is_high_connexity Use 8-connexity (only for 2d images).
//' @export
// [[Rcpp::export]]
NumericVector bucket_select(NumericVector im,int x,int y,int z,float sigma=0,bool is_high_connexity=false)
{
  CId img = as<CId >(im);
  CId out(img);
  NumericVector color(img.spectrum());
  float opacity=1;
  img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,out,sigma,is_high_connexity);
  return wrap(out);
}
