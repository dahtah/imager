#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;

//' Apply recursive Deriche filter.
//'
//' The Deriche filter is a fast approximation to a Gaussian filter (order = 0), or Gaussian derivatives (order = 1 or 2).   
//' 
//' @param im an image
//' @param sigma Standard deviation of the filter.
//' @param order Order of the filter. 0 for a smoothing filter, 1 for first-derivative, 2 for second.
//' @param axis Axis along which the filter is computed ( 'x' , 'y', 'z' or 'c').
//' @param neumann If true, use Neumann boundary conditions (default false, Dirichlet)
//' @export
//' @examples
//' deriche(boats,sigma=2,order=0) %>% plot("Zeroth-order Deriche along x")
//' deriche(boats,sigma=2,order=1) %>% plot("First-order Deriche along x")
//' deriche(boats,sigma=2,order=1) %>% plot("Second-order Deriche along x")
//' deriche(boats,sigma=2,order=1,axis="y") %>% plot("Second-order Deriche along y")
// [[Rcpp::export]]
NumericVector deriche(NumericVector im,float sigma,int order=0,char axis = 'x',bool neumann=false)
{
  CId img = as<CId >(im);
  try{
      img.deriche(sigma,order,axis,neumann);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }

  return wrap(img);
}


//' Young-Van Vliet recursive Gaussian filter.
//'
//' The Young-van Vliet filter is a fast approximation to a Gaussian filter (order = 0), or Gaussian derivatives (order = 1 or 2).   
//'
//' @param im an image
//' @param sigma standard deviation of the Gaussian filter
//' @param order the order of the filter 0,1,2,3
//' @param axis  Axis along which the filter is computed. One of 'x', 'y', 'z', 'c'
//' @param neumann If true, use Neumann boundary conditions (default false, Dirichlet)
//' @references
//'       From: I.T. Young, L.J. van Vliet, M. van Ginkel, Recursive Gabor filtering.
//'       IEEE Trans. Sig. Proc., vol. 50, pp. 2799-2805, 2002.
//'       (this is an improvement over Young-Van Vliet, Sig. Proc. 44, 1995)
//'
//'       Boundary conditions (only for order 0) using Triggs matrix, from
//'       B. Triggs and M. Sdika. Boundary conditions for Young-van Vliet
//'       recursive filtering. IEEE Trans. Signal Processing,
//'       vol. 54, pp. 2365-2367, 2006.
//' @examples
//' vanvliet(boats,sigma=2,order=0) %>% plot("Zeroth-order Young-van Vliet along x")
//' vanvliet(boats,sigma=2,order=1) %>% plot("First-order Young-van Vliet along x")
//' vanvliet(boats,sigma=2,order=1) %>% plot("Second-order Young-van Vliet along x")
//' vanvliet(boats,sigma=2,order=1,axis="y") %>% plot("Second-order Young-van Vliet along y")
//' @export
// [[Rcpp::export]]
NumericVector vanvliet(NumericVector im,float sigma,int order=0,char axis = 'x',bool neumann=false)
{
  CId img = as<CId >(im);
  try{
    img.vanvliet(sigma,order,axis,neumann);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


// [[Rcpp::export]]
NumericVector isoblur_(NumericVector im,float sigma,bool neumann=true,bool gaussian=false) {
  CId img = as< CId >(im);
  try{
    img.blur(sigma,neumann,gaussian);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}




//' Blur image with the median filter.
//'    
//' In a window of size n x n centered at pixel (x,y), compute median pixel value over the window. Optionally, ignore values that are too far from the value at current pixel.  
//'
//' @param im an image
//' @param n Size of the median filter.
//' @param threshold Threshold used to discard pixels too far from the current pixel value in the median computation. Can be used for edge-preserving smoothing. Default 0 (include all pixels in window).
//' @export
//' @examples
//' medianblur(boats,5) %>% plot(main="Median blur, 5 pixels")
//' medianblur(boats,10) %>% plot(main="Median blur, 10 pixels")
//' medianblur(boats,10,8) %>% plot(main="Median blur, 10 pixels, threshold = 8")
//' @seealso isoblur, boxblur
// [[Rcpp::export]]
NumericVector medianblur(NumericVector im,int n, float threshold=0) {
  CId img = as<CId >(im);
  try{
    img.blur_median(n,threshold);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

//' Blur image with a box filter (square window)
//' @param im an image
//' @param boxsize Size of the box window (can be subpixel).
//' @param neumann If true, use Neumann boundary conditions, Dirichlet otherwise  (default true, Neumann)
//' @seealso deriche(), vanvliet().
//' @examples
//' boxblur(boats,5) %>% plot(main="Dirichlet boundary")
//' boxblur(boats,5,TRUE) %>% plot(main="Neumann boundary")
//' @export
// [[Rcpp::export]]
NumericVector boxblur(NumericVector im,float boxsize,bool neumann=true) {
  CId img = as<CId >(im);
  try{
    img.blur_box(boxsize,neumann);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' Compute image Laplacian
//'
//' The Laplacian is the sum of second derivatives, approximated here using finite differences.
//' @param im an image
//' @examples
//' imlap(boats) %>% plot
//' @export
// [[Rcpp::export]]
NumericVector imlap(NumericVector im) {
  CId img = as<CId >(im);
  try{
    img.laplacian();
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(img);
}


//' Blur image with a box filter.
//'
//' This is a recursive algorithm, not depending on the values of the box kernel size.
//'
//' @param im an image
//' @param sx Size of the box window, along the X-axis.
//' @param sy Size of the box window, along the Y-axis.
//' @param neumann If true, use Neumann boundary conditions, Dirichlet otherwise  (default true, Neumann)
//' @seealso blur().
//'
//' @export
//' @examples
//' boxblur_xy(boats,20,5) %>% plot(main="Anisotropic blur")
// [[Rcpp::export]]
NumericVector boxblur_xy(NumericVector im,float sx,float sy,bool neumann=true) {
  CId img = as<CId >(im);
  try{
    img.blur_box(sx,sy,0,neumann);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

//' Correlation/convolution of image by filter
//'
//'  The correlation of image im by filter flt is defined as:
//'  \eqn{res(x,y,z) = sum_{i,j,k} im(x + i,y + j,z + k)*flt(i,j,k).}
//'  The convolution of an image img by filter flt is defined to be:
//'       \eqn{res(x,y,z) = sum_{i,j,k} img(x-i,y-j,z-k)*flt(i,j,k)}
//'
//' @param im an image
//' @param filter the correlation kernel.
//' @param dirichlet boundary condition. Dirichlet if true, Neumann if false (default TRUE, Dirichlet)
//' @param normalise compute a normalised correlation (ie. local cosine similarity)
//'      
//'
//' @export
//' @examples
//' #Edge filter
//' filter <- as.cimg(function(x,y) sign(x-5),10,10) 
//' layout(t(1:2))
//' #Convolution vs. correlation 
//' correlate(boats,filter) %>% plot(main="Correlation")
//' convolve(boats,filter) %>% plot(main="Convolution")
// [[Rcpp::export]]
NumericVector correlate(NumericVector im,NumericVector filter, bool dirichlet=true,bool normalise = false) {
  CId img = as<CId >(im);
  CId flt = as<CId >(filter);
  try{
    img.correlate(flt,!dirichlet,normalise);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' @describeIn correlate convolve image with filter
//' @export
// [[Rcpp::export]]
NumericVector convolve(NumericVector im,NumericVector filter, bool dirichlet=true,bool normalise = false) {
  CId img = as<CId >(im);
  CId flt = as<CId >(filter);
  try{
    img.convolve(flt,!dirichlet,normalise);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


// [[Rcpp::export]]
NumericVector sharpen(NumericVector im,float amplitude,
		bool sharpen_type = false,float edge = 1,
		float alpha = 0,float sigma = 0)
 {
   CId img = as<CId >(im);
   try{
     img.sharpen(amplitude,sharpen_type,edge,alpha,sigma);
     }
   catch(CImgException &e){
     forward_exception_to_r(e);
     
   }
   return wrap(img);
}

//' Compute image gradient.
//'
//' @param im an image
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
//' @seealso imgradient
// [[Rcpp::export]]
List get_gradient(NumericVector im,std::string axes = "",int scheme=3)
{
   CId img = as<CId >(im);
   try{
     CImgList<double> grad = img.get_gradient(axes.c_str(),scheme);
     return wrap(grad);
     }
   catch(CImgException &e){
     forward_exception_to_r(e);
     CImgList<double> empty;
     return wrap(empty);
   }
   

}

//' Return image hessian.
//' @param im an image
//' @param axes Axes considered for the hessian computation, as a character string (e.g "xy").
// [[Rcpp::export]]
List get_hessian(NumericVector im,std::string axes = "")
{
   CId img = as<CId >(im);

   try{
     CImgList<double> hess = img.get_hessian(axes.c_str());
     return wrap(hess);
     }
   catch(CImgException &e){
     forward_exception_to_r(e);
     CImgList<double> empty;
     return wrap(empty); //will never reach here
   }
   
}

//' Compute field of diffusion tensors for edge-preserving smoothing.
//'
//' @param im an image
//' @param sharpness Sharpness
//' @param anisotropy Anisotropy
//' @param alpha Standard deviation of the gradient blur.
//' @param sigma Standard deviation of the structure tensor blur.
//' @param is_sqrt Tells if the square root of the tensor field is computed instead.
//' @export
// [[Rcpp::export]]
NumericVector diffusion_tensors(NumericVector im,
				float sharpness = 0.7,	float anisotropy = 0.6,
				float alpha = 0.6,float sigma = 1.1,
				bool is_sqrt = false) 	
{
  CId img = as<CId >(im);
  try{
    img.diffusion_tensors(sharpness,anisotropy,alpha,sigma,is_sqrt);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

//' Compute Haar multiscale wavelet transform.
//'
//' @param im an image
//' @param inverse Compute inverse transform (default FALSE)
//' @param nb_scales Number of scales used for the transform.
//' @export
//' @examples
//' #Image compression: set small Haar coefficients to 0
//' hr <- haar(boats,nb=3) 
//' mask.low <- threshold(abs(hr),"75%")
//' mask.high <- threshold(abs(hr),"95%")
//' haar(hr*mask.low,inverse=TRUE,nb=3) %>% plot(main="75% compression")
//' haar(hr*mask.high,inverse=TRUE,nb=3) %>% plot(main="95% compression")
// [[Rcpp::export]]
NumericVector haar(NumericVector im,bool inverse=false,int nb_scales=1) {
  CId img = as<CId >(im);
  try{
    img.haar(inverse,nb_scales);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
List FFT_complex(NumericVector real,NumericVector imag,bool inverse=false,int nb_threads=0) {
  CId rl = as<CId >(real);
  CId img = as<CId >(imag);
  try{
    rl.FFT(rl,img,inverse,nb_threads);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return List::create(_["real"] = wrap(rl),_["imag"] = wrap(img));
}

// [[Rcpp::export]]
List FFT_realim(NumericVector real,bool inverse=false,int nb_threads=0) {
  CId rl = as<CId >(real);
  CId im(rl,"xyzc",0);
  try{
    rl.FFT(rl,im,inverse,nb_threads);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return List::create(_["real"] = wrap(rl),_["imag"] = wrap(im));
}

// [[Rcpp::export]]
NumericVector FFT_realout(NumericVector real,NumericVector imag,bool inverse=false,int nb_threads=0) {
  CId rl = as<CId >(real);
  CId img = as<CId >(imag);
  try{
    rl.FFT(rl,img,inverse,nb_threads);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(rl);
}


//' Estimate displacement field between two images.
//'
//' @param sourceIm Reference image.
//' @param destIm Reference image.
//' @param smoothness Smoothness of estimated displacement field.
//' @param precision Precision required for algorithm convergence.
//' @param nb_scales Number of scales used to estimate the displacement field.
//' @param iteration_max Maximum number of iterations allowed for one scale.
//' @param is_backward If false, match I2(X + U(X)) = I1(X), else match I2(X) = I1(X - U(X)).
//' @export
// [[Rcpp::export]]
NumericVector displacement(NumericVector sourceIm,NumericVector destIm,float smoothness=0.1, float precision=5.0,unsigned int nb_scales=0, unsigned int iteration_max=10000,bool is_backward=false)
{
   CId src = as<CId >(sourceIm);
   CId dst = as<CId >(destIm);
   CId out(src,false);
   try{
     out.displacement(dst,smoothness,precision,nb_scales,iteration_max,is_backward);
     }
   catch(CImgException &e){
     forward_exception_to_r(e);
     
   }
   return wrap(out);
}


//' Blur image anisotropically, in an edge-preserving way.
//' 
//' Standard blurring removes noise from images, but tends to smooth away edges in the process. This anisotropic filter preserves edges better. 
//' 
//' @param im an image
//' @param amplitude Amplitude of the smoothing.
//' @param sharpness Sharpness.
//' @param anisotropy Anisotropy.
//' @param alpha Standard deviation of the gradient blur.
//' @param sigma Standard deviation of the structure tensor blur.
//' @param dl Spatial discretization.
//' @param da Angular discretization.
//' @param gauss_prec Precision of the diffusion process.
//' @param interpolation_type Interpolation scheme.
//'  Can be 0=nearest-neighbor | 1=linear | 2=Runge-Kutta 
//' @param fast_approx If true, use fast approximation (default TRUE)
//' @export
//' @examples
//' im <- load.image(system.file('extdata/Leonardo_Birds.jpg',package='imager'))
//' im.noisy <- (im + 80*rnorm(prod(dim(im)))) 
//' blur_anisotropic(im.noisy,ampl=1e4,sharp=1) %>% plot
// [[Rcpp::export]]
NumericVector blur_anisotropic(NumericVector im, float amplitude,  float sharpness=0.7,  float anisotropy=0.6,float alpha=0.6,  float sigma=1.1,  float dl=0.8,  float da=30,
                               float gauss_prec=2,  unsigned int interpolation_type=0,
                               bool fast_approx=true) {
  CId img = as<CId >(im);
  try{
    img.blur_anisotropic(amplitude,sharpness,anisotropy,alpha,sigma,dl,da,gauss_prec,interpolation_type,fast_approx);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
NumericVector periodic_part(NumericVector im)
{
  CId img = as<CId >(im);
  CId D(img,"xyzc",0);
  if (img.spectrum() > 1 or img.depth() > 1)
    {
      stop("This function works only on 2D grayscale images");
    }
  int w = img.width(),h = img.height();
  int x = 0,y=0;
  double f = 0;
  //Compute D matrix (differences across boundaries)
  for (x = 0; x < w; x++)
    {
      f = img(x,0) - img(x,h-1);
      D(x,0) = -f;
      D(x,h-1) = f;
    }
  x = 0;
  for (y = 0; y < h; y++)
    {
      f = img(0,y) - img(w-1,y);
      D(0,y) -= f;
      D(w-1,y) += f;
    }
  //Compute FFT of D 
  CId impart(D,"xyzc",0);
  CId realpart(D);
  double weight=0;
  D.FFT(realpart,impart);
  cimg_forXY(D,x,y)
    {
      weight = 2*cos(2*cimg::PI*x/w)+2*cos(2*cimg::PI*y/h) - 4;
      impart(x,y) /= weight;
      realpart(x,y) /= weight;
    }
  realpart(0,0) = 0;
  impart(0,0) = 0;
  //Inverse FFT
  D.FFT(realpart,impart,true);
  //Take out non-periodic part from the original image
  img -= realpart;
  return wrap(img);
}


