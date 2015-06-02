#define cimg_use_png
#define cimg_use_jpeg
#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"

using namespace Rcpp;
using namespace cimg_library;



//' @export
// [[Rcpp::export]]
NumericVector load_image(std::string fname) {
  CImg<double> image(fname.c_str());
  return wrap(image);
}

//' @export
// [[Rcpp::export]]
void save_image(NumericVector im, std::string fname) {
  CImg<double> image = as<CImg<double> >(im);
  image.save(fname.c_str());
  return;
}

//' Split an image along a certain axis (producing a list)
//' 
//' @param im an image 
//' @param axis the axis along which to split (for example 'c')
//' @param nb number of objects to split into. 
//' if nb=-1 (the default) the maximum number of splits is used ie. split(im,"c") produces a list containing all individual colour channels
//' @seealso imappend (the reverse operation)
//' @export
// [[Rcpp::export]]
List imsplit(NumericVector im,char axis,int nb=-1)
{
   CImg<double> img = as<CImg<double> >(im);
   CImgList<double> out;
   out = img.get_split(axis,nb);
   return wrap(out);
}

//' Combine a list of images into a single image 
//' 
//' All images will be concatenated along the x,y,z, or c axis.
//' 
//' @param im an image 
//' @param axis the axis along which to split (for example 'c')
//' @seealso imsplit (the reverse operation)
//' @export
// [[Rcpp::export]]
NumericVector imappend(List imlist,char axis)
{
   CImgList<double> ilist = sharedCImgList(imlist);
   CImg<double> out(ilist.get_append(axis));
   //   out.display();
   return wrap(out);
}


//' @export
// [[Rcpp::export]]
List select_patches(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector wx,IntegerVector wy)
{
  CImg<double> img = as<CImg<double> >(im);
  int n = cx.length();
  CImgList<double> L;

  for (int i = 0; i < n; i++)
    {
      L.insert(img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2),i,true);
    }
  return wrap(L);
}

//' @export
// [[Rcpp::export]]
List select_patches3D(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector cz,IntegerVector wx,IntegerVector wy,IntegerVector wz)
{
  CImg<double> img = as<CImg<double> >(im);
  int n = cx.length();
  CImgList<double> L;

  for (int i = 0; i < n; i++)
    {
      L.insert(img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cz(i)-wz(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2,cz(i)+wz(i)/2),i,true);
    }
  return wrap(L);
}
