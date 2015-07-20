#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"

using namespace Rcpp;
using namespace cimg_library;




// [[Rcpp::export]]
NumericVector load_image(std::string fname) {
  CImg<double> image(fname.c_str());
  return wrap(image);
}


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
// [[Rcpp::export]]
List im_split(NumericVector im,char axis,int nb=-1)
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
//' @param imlist a list of images (all elements must be of class cimg) 
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

//' Return image patches 
//'
//' Patches are rectangular (cubic) image regions centered at cx,cy (cz) with width wx and height wy (opt. depth wz)
//'
//' @param im an image
//' @param cx: vector of x coordinates for patch centers 
//' @param cy: vector of y coordinates for patch centers 
//' @param wx: vector of coordinates for patch width 
//' @param wy: vector of coordinates for patch height 
//' @return a list of image patches (cimg objects)
//' @export
// [[Rcpp::export]]
List extract_patches(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector wx,IntegerVector wy)
{
  CImg<double> img = as<CImg<double> >(im);
  int n = cx.length();
  List out(n);

  for (int i = 0; i < n; i++)
    {
      out[i] = wrap(img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2));
    }
  return out;
}

//' @param cz: vector of z coordinates for patch centers 
//' @param wz: vector of coordinates for patch depth
//' @describeIn extract_patches Extract 3D patches
//' @export
// [[Rcpp::export]]
List extract_patches3D(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector cz,IntegerVector wx,IntegerVector wy,IntegerVector wz)
{
  CImg<double> img = as<CImg<double> >(im);
  int n = cx.length();
  List out(n);


  for (int i = 0; i < n; i++)
    {
      out[i] = img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cz(i)-wz(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2,cz(i)+wz(i)/2);
    }
  return out;
}
