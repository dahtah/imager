#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;




// [[Rcpp::export]]
NumericVector load_image(std::string fname) {
  CId image(fname.c_str());
  return wrap(image);
}


// [[Rcpp::export]]
void save_image(NumericVector im, std::string fname) {
  CId image = as<CId >(im);
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
   CId img = as<CId >(im);
   CImgList<double> out;
   out = img.get_split(axis,nb);
   return wrap(out);
}

//' Combine a list of images into a single image 
//' 
//' All images will be concatenated along the x,y,z, or c axis.
//' 
//' @param imlist a list of images (all elements must be of class cimg) 
//' @param axis the axis along which to concatenate (for example 'c')
//' @seealso imsplit (the reverse operation)
//' @export
//' @examples
//' imappend(list(boats,boats),"x") %>% plot
//' imappend(list(boats,boats),"y") %>% plot
//' plyr::rlply(3,imnoise(100,100)) %>% imappend("c") %>% plot
//' boats.gs <- grayscale(boats)
//' plyr::llply(seq(1,5,l=3),function(v) isoblur(boats.gs,v)) %>% imappend("c") %>% plot
// [[Rcpp::export]]
NumericVector imappend(List imlist,char axis)
{
   CImgList<double> ilist = sharedCImgList(imlist);
   CId out(ilist.get_append(axis));
   //   out.display();
   return wrap(out);
}

//' Return image patches 
//'
//' Patches are rectangular (cubic) image regions centered at cx,cy (cz) with width wx and height wy (opt. depth wz)
//'
//' @param im an image
//' @param cx vector of x coordinates for patch centers 
//' @param cy vector of y coordinates for patch centers 
//' @param wx vector of coordinates for patch width 
//' @param wy vector of coordinates for patch height 
//' @return a list of image patches (cimg objects)
//' @export
//' @examples
//' #2 patches of size 5x5 located at (10,10) and (10,20)
//' extract_patches(boats,c(10,10),c(10,20),rep(5,2),rep(5,2)) 
// [[Rcpp::export]]
List extract_patches(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector wx,IntegerVector wy)
{
  CId img = as<CId >(im);
  int n = cx.length();
  List out(n);

  for (int i = 0; i < n; i++)
    {
      out[i] = wrap(img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2));
    }
  return out;
}

//' @param cz vector of z coordinates for patch centers 
//' @param wz vector of coordinates for patch depth
//' @describeIn extract_patches Extract 3D patches
//' @export
// [[Rcpp::export]]
List extract_patches3D(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector cz,IntegerVector wx,IntegerVector wy,IntegerVector wz)
{
  CId img = as<CId >(im);
  int n = cx.length();
  List out(n);


  for (int i = 0; i < n; i++)
    {
      out[i] = img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cz(i)-wz(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2,cz(i)+wz(i)/2);
    }
  return out;
}

// [[Rcpp::export]]
NumericVector draw_image(NumericVector im,NumericVector sprite,int x=0,int y = 0, int z = 0,float opacity = 1)
{
  CId img = as<CId >(im);
  CId spr = as<CId >(sprite);
  img.draw_image(x,y,z,spr,opacity);
  return wrap(img);
}
