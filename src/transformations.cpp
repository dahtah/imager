#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"
using namespace Rcpp;
using namespace cimg_library;
//' Autocrop image region 
//'
//' @param color Color used for the crop. If  0, color is guessed.
//' @param axes Axes used for the crop.
//' @export
// [[Rcpp::export]]
NumericVector autocrop(NumericVector im,NumericVector color,std::string axes = "zyx")
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.autocrop(color.begin(),axes.c_str());
   return wrap(out);
}

//' Rotate image by an arbitrary angle.
//'
//' Most of the time, the size of the image is modified.
//'
//' @param angle Rotation angle, in degrees.
//' @param interpolation Type of interpolation. Can be <tt>{ 0=nearest | 1=linear | 2=cubic }</tt>.
//' @param boundary Boundary conditions. Can be <tt>{  0=dirichlet | 1=neumann | 2=periodic }</tt>.
//' @export
// [[Rcpp::export]]
NumericVector imrotate(NumericVector im,float  	angle,
		     unsigned int interpolation = 1,
		     unsigned int boundary = 0)
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.rotate(angle,interpolation,boundary);
   return wrap(out);
}

//' Rotate image by an arbitrary angle, around a center point.
//'
//'       @param angle Rotation angle, in degrees.
//'       @param cx X-coordinate of the rotation center.
//'       @param cy Y-coordinate of the rotation center.
//'       @param zoom Zoom factor.
//'       @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann | 2=periodic }</tt>.
//'       @param interpolation_type Type of interpolation. Can be <tt>{ 0=nearest | 1=linear | 2=cubic }</tt>.
//'
//' @export
// [[Rcpp::export]]
NumericVector rotate_xy(NumericVector im,
			float  	angle,
			float cx,float cy, float zoom=1,
			unsigned int interpolation = 1,
			unsigned int boundary = 0)
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.rotate(angle,cx,cy,zoom,interpolation,boundary);
   return wrap(out);
}

//' Mirror image content along specified axis 
//'       @param axis Mirror axis ("x","y","z","c")
//' @export
// [[Rcpp::export]]
NumericVector mirror(NumericVector im,char axis)
{
   CImg<double> img = as<CImg<double> >(im);
   img.mirror(axis);
   return wrap(img);
}

//' @export
// [[Rcpp::export]]
NumericVector permute_axes(NumericVector im,std::string perm)
{
   CImg<double> img = as<CImg<double> >(im);
   img.permute_axes(perm.c_str());
   return wrap(img);
}

//' Resize image to double-size, using the Scale2X algorithm.
//'
//'Use anisotropic upscaling algorithm
//'       <a href="http://scale2x.sourceforge.net/algorithm.html">described here</a>.
//' @export
// [[Rcpp::export]]
NumericVector resize_doubleXY(NumericVector im)
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.resize_doubleXY();
   return wrap(out);
}

//' Resize image to half-size, using an optimized filter
//'
//'Use anisotropic upscaling algorithm
//'       <a href="http://scale2x.sourceforge.net/algorithm.html">described here</a>.
//' @export
// [[Rcpp::export]]
NumericVector resize_halfXY(NumericVector im)
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.resize_halfXY();
   return wrap(out);
}



//' Resize image to triple-size, using the Scale2X algorithm.
//'
//'Use anisotropic upscaling algorithm
//'       <a href="http://scale2x.sourceforge.net/algorithm.html">described here</a>.
//' @export
// [[Rcpp::export]]
NumericVector resize_tripleXY(NumericVector im)
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.resize_tripleXY();
   return wrap(out);
}




//' Shift image content.
//'
//'       @param delta_x Amount of displacement along the X-axis.
//'       @param delta_y Amount of displacement along the Y-axis.
//'       @param delta_z Amount of displacement along the Z-axis.
//'       @param delta_c Amount of displacement along the C-axis.
//'       @param boundary_conditions can be:
//'          - 0: Zero border condition (Dirichlet).
//'          - 1: Nearest neighbors (Neumann).
//'          - 2: Repeat Pattern (Fourier style).
//' @export
// [[Rcpp::export]]
NumericVector imshift(NumericVector im, int delta_x=0,  int delta_y=0,  int delta_z=0,  int delta_c=0,
                    int boundary_conditions=0)
{
   CImg<double> img = as<CImg<double> >(im);
   img.shift(delta_x,delta_y,delta_z,delta_c,boundary_conditions);
   return wrap(img);
}


//' Resize image to new dimensions.
//' If pd[x,y,z,v]<0, it corresponds to a percentage of the original size (the default value is -100).
//' @param size_x Number of columns (new size along the X-axis).
//' @param size_y Number of rows (new size along the Y-axis).
//' @param size_z Number of slices (new size along the Z-axis).
//' @param size_c Number of vector-channels (new size along the C-axis).
//' @param interpolation_type Method of interpolation:
//' -1 = no interpolation: raw memory resizing.
//' 0 = no interpolation: additional space is filled according to  boundary_conditions.
//' 1 = nearest-neighbor interpolation.
//' 2 = moving average interpolation.
//' 3 = linear interpolation.
//' 4 = grid interpolation.
//' 5 = cubic interpolation.
//' 6 = lanczos interpolation.
//' @param boundary_conditions Border condition type.
//' @param centering_x Set centering type (only if  interpolation_type=0).
//' @param centering_y Set centering type (only if  interpolation_type=0).
//' @param centering_z Set centering type (only if  interpolation_type=0).
//' @param centering_c Set centering type (only if  interpolation_type=0).
//' @export
// [[Rcpp::export]]
NumericVector resize(NumericVector im, int size_x=-100,  int size_y=-100,
                     int size_z=-100,  int size_c=-100,
                     int interpolation_type=1,  unsigned int boundary_conditions=0,
                     float centering_x = 0,  float centering_y = 0,
                     float centering_z = 0,  float centering_c = 0)
{
   CImg<double> img = as<CImg<double> >(im);
   CImg<double> out(img,false);
   out.resize(size_x,size_y,size_z,size_c,interpolation_type,boundary_conditions,
	      centering_x,centering_y,centering_z,centering_c);
   return wrap(out);
}

//' Warp image
//'
//' @param warp Warping field. The (x,y,z) fields should be stacked along the colour coordinate. 
//' @param mode Can be { 0=backward-absolute | 1=backward-relative | 2=forward-absolute | 3=forward-relative }
//' @param is_relative does warping field give absolute or relative warping coordinates?
//' @param interpolation Can be <tt>{ 0=nearest | 1=linear | 2=cubic }</tt>.
//' @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann | 2=periodic }</tt>.
//' @seealso imwarp for a user-friendly interface
//' @export
// [[Rcpp::export]]
NumericVector warp(NumericVector im,NumericVector warpfield, 
		   unsigned int mode=0,
		   unsigned int interpolation=1, 
		   unsigned int boundary_conditions=0)
{
  CImg<double> img = as<CImg<double> >(im);
  CImg<double> wrp = as<CImg<double> >(warpfield);
  //CImg<double> out(img,false);

  CImg<double> out=img.get_warp(wrp,mode,interpolation,boundary_conditions);
  //out.warp(wrp,mode,interpolation,boundary_conditions);
  return wrap(out);
}




// NumericVector mirror(NumericVector im,std::string axes)
// {
//    CImg<double> img = as<CImg<double> >(im);
//    img.mirror(axes.c_str());
//    return wrap(img);
// }
