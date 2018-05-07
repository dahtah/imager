#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

// [[Rcpp::export]]
NumericVector autocrop_(NumericVector im,NumericVector color,std::string axes = "zyx")
{
  CId img = as<CId >(im);
  CId out;

  try{
    out = img.get_autocrop(color.begin(),axes.c_str());
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}

// [[Rcpp::export]]
NumericVector rotate(NumericVector im,float  	angle,
		     unsigned int interpolation = 1,
		     unsigned int boundary = 0)
{
  CId img = as<CId >(im);
  CId out(img,false);

  try{
    out.rotate(angle,interpolation,boundary);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}

//' Rotate image by an arbitrary angle, around a center point.
//'
//' @param im an image
//' @param angle Rotation angle, in degrees.
//' @param cx X-coordinate of the rotation center.
//' @param cy Y-coordinate of the rotation center.
//' @param interpolation Interpolation type. 0=nearest | 1=linear | 2=cubic 
//' @param boundary_conditions Boundary conditions. 0=dirichlet | 1=neumann | 2=periodic 
//' @examples
//' rotate_xy(boats,30,200,400) %>% plot
//' rotate_xy(boats,30,200,400,boundary=2) %>% plot
//' @export
// [[Rcpp::export]]
NumericVector rotate_xy(NumericVector im,
			float  	angle,
			float cx,float cy,
			unsigned int interpolation = 1,
			unsigned int boundary_conditions = 0)
{
  CId img = as<CId >(im);
  CId out(img,false);


  try{
    out.rotate(angle,cx,cy,interpolation,boundary_conditions);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}

//' Mirror image content along specified axis 
//' @param im an image
//' @param axis Mirror axis ("x","y","z","c")
//' @export
//' @examples
//' mirror(boats,"x") %>% plot
//' mirror(boats,"y") %>% plot
// [[Rcpp::export]]
NumericVector mirror(NumericVector im,char axis)
{
  CId img = as<CId >(im);
    
  try{
    img.mirror(axis);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(img);
}

//' Permute image axes
//' 
//' By default images are stored in xyzc order. Use permute_axes to change that order. 
//' @param im an image
//' @param perm a character string, e.g., "zxyc" to have the z-axis come first
//' @export
//' @examples
//' im <- array(0,c(10,30,40,3)) %>% as.cimg
//' permute_axes(im,"zxyc")
// [[Rcpp::export]]
NumericVector permute_axes(NumericVector im,std::string perm)
{
  CId img = as<CId >(im);
  try{

    img.permute_axes(perm.c_str());
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(img);
}

//' @describeIn resize_uniform Double size
//' @export
// [[Rcpp::export]]
NumericVector resize_doubleXY(NumericVector im)
{
  CId img = as<CId >(im);
  CId out(img,false);

  try{
    out.resize_doubleXY();
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}

//' @describeIn resize_uniform Half size
//' @export
// [[Rcpp::export]]
NumericVector resize_halfXY(NumericVector im)
{
  CId img = as<CId >(im);
  CId out(img,false);

  try{
    out.resize_halfXY();
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}


//' @describeIn resize_uniform Triple size
//' @export
// [[Rcpp::export]]
NumericVector resize_tripleXY(NumericVector im)
{
  CId img = as<CId >(im);
  CId out(img,false);

  try{
    out.resize_tripleXY();
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}




//' Shift image content.
//'
//' @param im an image
//' @param delta_x Amount of displacement along the X-axis.
//' @param delta_y Amount of displacement along the Y-axis.
//' @param delta_z Amount of displacement along the Z-axis.
//' @param delta_c Amount of displacement along the C-axis.
//' @param boundary_conditions can be:
//'          - 0: Zero border condition (Dirichlet).
//'          - 1: Nearest neighbors (Neumann).
//'          - 2: Repeat Pattern (Fourier style).
//' @export
//' @examples
//' imshift(boats,10,50) %>% plot
// [[Rcpp::export]]
NumericVector imshift(NumericVector im, int delta_x=0,  int delta_y=0,  int delta_z=0,  int delta_c=0,
                    int boundary_conditions=0)
{
  CId img = as<CId >(im);

  try{
    img.shift(delta_x,delta_y,delta_z,delta_c,boundary_conditions);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(img);
}


//' Resize image
//'
//' If the dimension arguments are negative, they are interpreted as a proportion of the original image. 
//' @param im an image
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
//' @seealso See imresize for an easier interface. 
//' @export
// [[Rcpp::export]]
NumericVector resize(NumericVector im, int size_x=-100,  int size_y=-100,
                     int size_z=-100,  int size_c=-100,
                     int interpolation_type=1,  unsigned int boundary_conditions=0,
                     float centering_x = 0,  float centering_y = 0,
                     float centering_z = 0,  float centering_c = 0)
{
  CId img = as<CId >(im);
  CId out(img,false);
  try{
    out.resize(size_x,size_y,size_z,size_c,interpolation_type,boundary_conditions,
	       centering_x,centering_y,centering_z,centering_c);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
   return wrap(out);
}

//' Warp image
//'
//' @param im an image
//' @param warpfield Warping field. The (x,y,z) fields should be stacked along the colour coordinate. 
//' @param mode Can be { 0=backward-absolute | 1=backward-relative | 2=forward-absolute | 3=forward-relative }
//' @param interpolation Can be <tt>{ 0=nearest | 1=linear | 2=cubic }</tt>.
//' @param boundary_conditions Boundary conditions. Can be <tt>{ 0=dirichlet | 1=neumann | 2=periodic }</tt>.
//' @seealso imwarp for a user-friendly interface 
//' @export
//' @examples
//' #Shift image via warp
//' warp.x <- imfill(width(boats),height(boats),val=5)
//' warp.y <- imfill(width(boats),height(boats),val=20)
//' warpfield <- list(warp.x,warp.y) %>% imappend("c")
//' warp(boats,warpfield,mode=1) %>% plot
// [[Rcpp::export]]
NumericVector warp(NumericVector im,NumericVector warpfield, 
		   unsigned int mode=0,
		   unsigned int interpolation=1, 
		   unsigned int boundary_conditions=0)
{
    CId img = as<CId >(im);
    CId wrp = as<CId >(warpfield);
    try{
      if ((mode==0) || (mode == 2)) //In R coordinates start at 1
	{
	  wrp--;
	}
      img.warp(wrp,mode,interpolation,boundary_conditions);
    }
    catch(CImgException &e){
      forward_exception_to_r(e);
    }
    return wrap(img);
}




