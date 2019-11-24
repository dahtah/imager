#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;



// [[Rcpp::export]]
NumericVector load_image(std::string fname) {
  try{
    CId image(fname.c_str());
    return wrap(image);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    NumericVector empty;
    return empty; //won't happen
  }
}


// [[Rcpp::export]]
void save_image(NumericVector im, std::string fname) {
  try{
    CId image = as<CId >(im);
    image.save(fname.c_str());
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
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
  try{
    CId img = as<CId >(im);
    CImgList<double> out;
    out = img.get_split(axis,nb);
    return wrap(out);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    List empty;
    return empty; //won't happen
  }
}


// [[Rcpp::export]]
NumericVector im_append(List imlist,char axis)
{
  try{
      CImgList<double> ilist = sharedCImgList(imlist);
      CId out(ilist.get_append(axis));
      return wrap(out);
  }
  catch(CImgException &e){
    forward_exception_to_r(e);
    NumericVector empty;
    return empty;
  }
}

// [[Rcpp::export]]
LogicalVector px_append(List imlist,char axis)
{
  try{
      CImgList<int> ilist = sharedCImgList_bool(imlist);
      CIb out(ilist.get_append(axis));
      return wrap(out);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    LogicalVector empty;
    return empty;
  }
}

//' Extract a numerical summary from image patches, using CImg's mini-language
//' Experimental feature. 
//' @param im an image
//' @param expr a CImg expression (as a string)
//' @param cx vector of x coordinates for patch centers 
//' @param cy vector of y coordinates for patch centers 
//' @param wx vector of coordinates for patch width 
//' @param wy vector of coordinates for patch height 
//' @examples
//' #Example: median filtering using patch_summary_cimg
//' #Center a patch at each pixel
//' im <- grayscale(boats)
//' patches <- pixel.grid(im)  %>% dplyr::mutate(w=3,h=3)
//' #Extract patch summary
//' out <- dplyr::mutate(patches,med=patch_summary_cimg(im,"ic",x,y,w,h))
//' as.cimg(out,v.name="med") %>% plot
//' @export
// [[Rcpp::export]]
NumericVector patch_summary_cimg(NumericVector im,std::string expr,IntegerVector cx,IntegerVector cy,IntegerVector wx,IntegerVector wy)
{
  CId img = as<CId >(im);
  int n = cx.length();
  NumericVector out(n);

  for (int i = 0; i < n; i++)
    {
      out[i] = img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2).eval(expr.c_str());
    }
  return out;
}

// Extract a patch summary, fast version
// Modified from original contribution by Martin Roth
// [[Rcpp::export]]
NumericVector extract_fast(NumericVector im,int fun,IntegerVector cx,IntegerVector cy,IntegerVector wx,IntegerVector wy)
{
  CId img = as<CId >(im);
  int n = cx.length();
  NumericVector out(n);
  CId patch;
  
  for (int i = 0; i < n; i++)
  {
    patch = img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2);
    switch (fun)
      {
      case 0:
	out[i] = patch.sum();
	break;
      case 1:
	out[i] = patch.mean();
	break;
      case 2:
	out[i] = patch.min();
	break;
      case 3:
	out[i] = patch.max();
	break;
      case 4:
	out[i] = patch.median();
      	break;
      case 5:
	out[i] = patch.variance();
	break;
      case 6:
	out[i] = sqrt(patch.variance());
      }
  }
  return out;
}

//' Extract image patches and return a list
//'
//' Patches are rectangular (cubic) image regions centered at cx,cy (cz) with width wx and height wy (opt. depth wz)
//' WARNINGS: 
//' - values outside of the image region are subject to boundary conditions. The default is to set them to 0 (Dirichlet), other boundary conditions are listed below. 
//' - widths and heights should be odd integers (they're rounded up otherwise). 
//' @param im an image
//' @param cx vector of x coordinates for patch centers 
//' @param cy vector of y coordinates for patch centers 
//' @param wx vector of patch widths (or single value)
//' @param wy vector of patch heights (or single value)
//' @param boundary_conditions integer. Can be 0 (Dirichlet, default), 1 (Neumann) 2 (Periodic) 3 (mirror). 
//' @return a list of image patches (cimg objects)
//' @export
//' @examples
//' #2 patches of size 5x5 located at (10,10) and (10,20)
//' extract_patches(boats,c(10,10),c(10,20),5,5)
// [[Rcpp::export]]
List extract_patches(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector wx,IntegerVector wy,int boundary_conditions=0)
{
  CId img = as<CId >(im);
  int n = cx.length();
  List out(n);
  bool rep = false;
  if (cx.length() != cy.length())
    {
      stop("cx and cy must have equal length");
    }
  if (wx.length() != wy.length())
    {
      stop("wx and wy must have equal length");
    }
  if (wx.length() == 1)
    {
      rep = true;
    }
  cx = cx - 1;
  cy = cy - 1;
  for (int i = 0; i < n; i++)
    {
      if (rep)
	{
	  out[i] = wrap(img.get_crop(cx(i)-wx(0)/2,cy(i)-wy(0)/2,cx(i)+wx(0)/2,cy(i)+wy(0)/2,boundary_conditions)); 
	}
      else
	{
	  out[i] = wrap(img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2,boundary_conditions)); 
	}
    }
  out.attr("class") = CharacterVector::create("imlist","list");
  return wrap(out);
}

//' @param cz vector of z coordinates for patch centers 
//' @param wz vector of coordinates for patch depth
//' @describeIn extract_patches Extract 3D patches
//' @export
// [[Rcpp::export]]
List extract_patches3D(NumericVector im,IntegerVector cx,IntegerVector cy,IntegerVector cz,IntegerVector wx,IntegerVector wy,IntegerVector wz,int boundary_conditions=0)
{
  CId img = as<CId >(im);
  int n = cx.length();
  List out(n);
  bool rep = false;
  if ((cx.length() != cy.length()) or (cx.length() != cz.length()) or (cy.length() != cz.length()))
    {
      stop("cx, cy and cz must have equal length");
    }
  if ((wx.length() != wy.length()) or (wx.length() != wz.length()) or (wy.length() != wz.length()))
    {
      stop("wx, wy and wz must have equal length");
    }
  if (wx.length() == 1)
    {
      rep = true;
    }
  for (int i = 0; i < n; i++)
    {
      if (rep)
	{
	  out[i] = img.get_crop(cx(i)-wx(0)/2,cy(i)-wy(0)/2,cz(i)-wz(0)/2,cx(i)+wx(0)/2,cy(i)+wy(0)/2,cz(i)+wz(0)/2,boundary_conditions);
	}
      else
	{
	  out[i] = img.get_crop(cx(i)-wx(i)/2,cy(i)-wy(i)/2,cz(i)-wz(i)/2,cx(i)+wx(i)/2,cy(i)+wy(i)/2,cz(i)+wz(i)/2,boundary_conditions);
	}
    }
  out.attr("class") = CharacterVector::create("imlist","list");
  return out;
}

// [[Rcpp::export]]
NumericVector draw_image(NumericVector im,NumericVector sprite,int x=0,int y = 0, int z = 0,float opacity = 1)
{
  CId img = as<CId >(im);

  try{
    CId spr = as<CId >(sprite);
    img.draw_image(x,y,z,spr,opacity);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


// [[Rcpp::export]]
List do_patchmatch(NumericVector im1,NumericVector im2,
			  unsigned int patch_width,
			  unsigned int patch_height,
			  unsigned int patch_depth,
			  unsigned int nb_iterations,
			  unsigned int nb_randoms,
			  float occ_penalization,
                          NumericVector guide)
{
  try{
    CId img1 = as<CId >(im1);
    CId img2 = as<CId >(im2);
    CId g = as<CId >(guide);
    CId mscore(img1,"xyzc");
    CImg<int> out = img1.matchpatch(img2,patch_width,patch_height,patch_depth,
				    nb_iterations,nb_randoms,occ_penalization,g,mscore);
    CId outfl(out);
    return List::create(_["warp"] = wrap(outfl),_["score"] = wrap(mscore));
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    List empty;
    return empty; //won't happen
  }

}



// Check that coordinates are all in image (indexing from 1)
// [[Rcpp::export]]
LogicalVector checkcoords(IntegerVector x,IntegerVector y,IntegerVector z,IntegerVector c,IntegerVector d)
{
  int n = x.length();
  LogicalVector out(n);
  for (int i = 0; i < n; i++)
    {
      if ((x[i] < 1) or (x[i] > d[0]) or (y[i] < 1) or (y[i] > d[1]) or (z[i] < 1) or (z[i] > d[2]) or (c[i] < 1) or (c[i] > d[3]))
	{
	  out[i] = false;
	}
      else
	{
	  out[i] = true;
	}
    }
  return out;
}


// [[Rcpp::export]]
int cimg_omp()
{
  return cimg::openmp_mode();
}

// [[Rcpp::export]]
int set_cimg_omp(int mode)
{
  return cimg::openmp_mode(mode);
}

// [[Rcpp::export]]
bool has_omp()
{
#ifdef cimg_use_openmp
  return true;
#else
  return false;
#endif
}

// [[Rcpp::export]]
List px_split(LogicalVector im,char axis,int nb=-1)
{
  try{
    CIb img = as<CIb >(im);
    CImgList<bool> out;
    out = img.get_split(axis,nb);
    return wrap(out);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    List empty;
    return empty; //won't happen
  }
}


