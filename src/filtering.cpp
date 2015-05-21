#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"
using namespace Rcpp;
using namespace cimg_library;

//' Apply recursive Deriche filter.
//' @param sigma Standard deviation of the filter.
//' @param order Order of the filter. Can be <tt>{ 0=smooth-filter | 1=1st-derivative | 2=2nd-derivative }</tt>.
//' @param axis Axis along which the filter is computed. Can be <tt>{ 'x' | 'y' | 'z' | 'c' }</tt>.
//' @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann }</tt>.
//' @export
// [[Rcpp::export]]

NumericVector deriche(NumericVector inp,float sigma,int order=0,char axis = 'x',bool boundary_conditions=0)
{
  CImg<double> img = as<CImg<double> >(inp);
  img.deriche(sigma,order,axis,boundary_conditions);
  return wrap(img);
}


//' Van Vliet recursive Gaussian filter.
//'
//'       From: I.T. Young, L.J. van Vliet, M. van Ginkel, Recursive Gabor filtering.
//'       IEEE Trans. Sig. Proc., vol. 50, pp. 2799-2805, 2002.
//'       (this is an improvement over Young-Van Vliet, Sig. Proc. 44, 1995)
//'
//'       Boundary conditions (only for order 0) using Triggs matrix, from
//'       B. Triggs and M. Sdika. Boundary conditions for Young-van Vliet
//'       recursive filtering. IEEE Trans. Signal Processing,
//'       vol. 54, pp. 2365-2367, 2006.
//'
//'       @param sigma standard deviation of the Gaussian filter
//'       @param order the order of the filter 0,1,2,3
//'       @param axis  Axis along which the filter is computed. Can be <tt>{ 'x' | 'y' | 'z' | 'c' }</tt>.
//'       @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann }</tt>.
//'       (Dirichlet boundary condition has a strange behavior)
//' @export
// [[Rcpp::export]]
NumericVector vanvliet(NumericVector inp,float sigma,int order=0,char axis = 'x',bool boundary_conditions=0)
{
  CImg<double> img = as<CImg<double> >(inp);
  img.vanvliet(sigma,order,axis,boundary_conditions);
  return wrap(img);
}


//' Blur image isotropically.
//' @param sigma Standard deviation of the blur.
//' @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann }
//' @seealso
//'  deriche(), vanvliet().
//' @export
// [[Rcpp::export]]
NumericVector isoblur(NumericVector inp,float sigma,bool boundary_conditions=true,bool is_gaussian=false) {
  CImg<double> img = as<CImg<double> >(inp);
  img.blur(sigma,boundary_conditions,is_gaussian);
  return wrap(img);
}


//' Blur image with the median filter.
//'    
//'  @param n Size of the median filter.
//'  @param threshold Threshold used to discard pixels too far from the current pixel value in the median computation.
//' @export
// [[Rcpp::export]]
NumericVector medianblur(NumericVector inp,int n, float threshold) {
  CImg<double> img = as<CImg<double> >(inp);
  img.blur_box(n,threshold);
  return wrap(img);
}

//' Blur image with a box filter.
//' @param sigma Size of the box window.
//' @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann }</tt>.a
//' @seealso deriche(), vanvliet().
//' @export
// [[Rcpp::export]]
NumericVector boxblur(NumericVector inp,float sigma,bool boundary_conditions=true) {
  CImg<double> img = as<CImg<double> >(inp);
  img.blur_box(sigma,boundary_conditions);
  return wrap(img);
}


//' Blur image with a box filter.
//'
//' This is a recursive algorithm, not depending on the values of the box kernel size.
//'
//'       @param sigma_x Size of the box window, along the X-axis.
//'       @param sigma_y Size of the box window, along the Y-axis.
//'       @param sigma_z Size of the box window, along the Z-axis.
//'       @param boundary_conditions Boundary conditions. Can be <tt>{ false=dirichlet | true=neumann }</tt>.
//'       @seealso blur().
//'
//' @export
// [[Rcpp::export]]
NumericVector boxblur_xy(NumericVector inp,float sx,float sy,bool boundary_conditions=true) {
  CImg<double> img = as<CImg<double> >(inp);
  img.blur_box(sx,sy,0,boundary_conditions);
  return wrap(img);
}

//' Correlate image by a mask.
//'
//'  The correlation of the image instance this by the mask mask is defined to be:
//'  res(x,y,z) = sum_{i,j,k} (*this)(x + i,y + j,z + k)*mask(i,j,k).
//'
//'       @param mask = the correlation kernel.
//'       @param boundary_conditions = the border condition type (0=zero, 1=dirichlet)
//'       @param is_normalized = enable local normalization.
//'      
//'
//' @export
// [[Rcpp::export]]
NumericVector correlate(NumericVector im,NumericVector filter, bool boundary_conditions=true,bool is_normalised = false) {
  CImg<double> img = as<CImg<double> >(im);
  CImg<double> flt = as<CImg<double> >(filter);
  img.correlate(flt,boundary_conditions,is_normalised);
  return wrap(img);
}


//' Convolve image by a mask.
//'
//'      The result  res of the convolution of an image img by a mask mask is defined to be:
//'       res(x,y,z) = sum_{i,j,k} img(x-i,y-j,z-k)*mask(i,j,k)
//'
//'       @param mask = the correlation kernel.
//'       @param boundary_conditions = the border condition type (0=zero, 1=dirichlet)
//'       @param is_normalized = enable local normalization.
//'
//'
//' @export
// [[Rcpp::export]]
NumericVector convolve(NumericVector im,NumericVector filter, bool boundary_conditions=true,bool is_normalised = false) {
  CImg<double> img = as<CImg<double> >(im);
  CImg<double> flt = as<CImg<double> >(filter);
  img.convolve(flt,boundary_conditions,is_normalised);
  return wrap(img);
}


//' Sharpen image.
//'
//'       @param amplitude Sharpening amplitude
//'       @param sharpen_type Select sharpening method. Can be <tt>{ false=inverse diffusion | true=shock filters }</tt>.
//'       @param edge Edge threshold (shock filters only).
//'       @param alpha Gradient smoothness (shock filters only).
//'       @param sigma Tensor smoothness (shock filters only).
//'
//' @export
// [[Rcpp::export]]
NumericVector sharpen(NumericVector im,float amplitude,
		bool sharpen_type = false,float edge = 1,
		float alpha = 0,float sigma = 0)
 {
   CImg<double> img = as<CImg<double> >(im);
   img.sharpen(amplitude,sharpen_type,edge,alpha,sigma);
   return wrap(img);
}

//' Compute image gradient.
//'
//' @param axes Axes considered for the gradient computation, as a C-string (e.g "xy").
//' @param scheme = Numerical scheme used for the gradient computation:
//'       1 = Backward finite differences
//'       0 = Centered finite differences
//'       1 = Forward finite differences
//'       2 = Using Sobel masks
//'       3 = Using rotation invariant masks
//'       4 = Using Deriche recursive filter.
//'       5 = Using Van Vliet recursive filter.
//' @return a list of images (corresponding to the different directions)
//' @export
// [[Rcpp::export]]
List get_gradient(NumericVector im,std::string axes = "",int scheme=3)
{
   CImg<double> img = as<CImg<double> >(im);
   CImgList<double> grad = img.get_gradient(axes.c_str(),scheme);
   return wrap(grad);
}

//' Return image hessian.
//'       @param axes Axes considered for the hessian computation, as a character string (e.g "xy").
//' @export
// [[Rcpp::export]]
List get_hessian(NumericVector im,std::string axes = "")
{
   CImg<double> img = as<CImg<double> >(im);
   CImgList<double> hess = img.get_hessian(axes.c_str());
   return wrap(hess);
}

//' Compute field of diffusion tensors for edge-preserving smoothing.
//'
//'       @param sharpness Sharpness
//'       @param anisotropy Anisotropy
//'       @param alpha Standard deviation of the gradient blur.
//'       @param sigma Standard deviation of the structure tensor blur.
//'       @param is_sqrt Tells if the square root of the tensor field is computed instead.
//' @export
// [[Rcpp::export]]
NumericVector diffusion_tensors(NumericVector im,
				float sharpness = 0.7,	float anisotropy = 0.6,
				float alpha = 0.6,float sigma = 1.1,
				bool is_sqrt = false) 	
{
  CImg<double> img = as<CImg<double> >(im);
  img.diffusion_tensors(sharpness,anisotropy,alpha,sigma,is_sqrt);
  return wrap(img);
}

//' Compute Haar multiscale wavelet transform.
//'
//'       @param axis Axis considered for the transform.
//'       @param invert Set inverse of direct transform.
//'       @param nb_scales Number of scales used for the transform.
//'
//' @export
// [[Rcpp::export]]
NumericVector haar(NumericVector im,bool inverse=false,int nb_scales=1) {
  CImg<double> img = as<CImg<double> >(im);
  img.haar(inverse,nb_scales);
  return wrap(img);
}

//' @export
// [[Rcpp::export]]
List FFT_complex(NumericVector real,NumericVector imag,bool inverse=false,int nb_threads=0) {
  CImg<double> rl = as<CImg<double> >(real);
  CImg<double> img = as<CImg<double> >(imag);
  rl.FFT(rl,img,inverse,nb_threads);
  return List::create(_["real"] = wrap(rl),_["imag"] = wrap(img));
}


//' Estimate displacement field between two images.
//'
//' @param source Reference image.
//' @param smoothness Smoothness of estimated displacement field.
//' @param precision Precision required for algorithm convergence.
//' @param nb_scales Number of scales used to estimate the displacement field.
//' @param iteration_max Maximum number of iterations allowed for one scale.
//' @param is_backward If false, match I2(X + U(X)) = I1(X), else match I2(X) = I1(X - U(X)).
//' @export
// [[Rcpp::export]]
NumericVector displacement(NumericVector sourceIm,NumericVector destIm,float smoothness=0.1, float precision=5.0,unsigned int nb_scales=0, unsigned int iteration_max=10000,bool is_backward=false)
{
   CImg<double> src = as<CImg<double> >(sourceIm);
   CImg<double> dst = as<CImg<double> >(destIm);
   CImg<double> out(src,false);
   out.displacement(dst,smoothness,precision,nb_scales,iteration_max,is_backward);
   return wrap(out);
}
