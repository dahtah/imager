#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;


//' Compute the pixel-wise median across an image list
//' 
//' Similarly to other reductions like "average", "parmax", and "parmin", this function computes the pixel-wise median in an image list (i.e., the median value for each pixel when comparing pixels across the list).
//' Images of different sizes are supported, in which case all images will be aligned to the first image, and padded with zeros.
//' @param x an image list
//' @examples
//' #Compute median values across colour channels
//' imsplit(boats,"c") %>% pmedian %>% plot
//' @export
// [[Rcpp::export]]
NumericVector pmedian(List x)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0));
  int n = x.size();
  cimg_pragma_openmp(parallel for cimg_openmp_if(out.size()>=65536))
  cimg_forXYZC(out,x,y,z,c)
    {
      CId vec(n,1,1,1);
      for (int i = 0; i <n; i++)
	{
	  vec(i) = L.atNXYZC(i,x,y,z,c);
	}
      out(x,y,z,c) = vec.median();
    }
  return wrap(out);
}

// [[Rcpp::export]]
NumericVector reduce_list(List x,int summary = 0)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),false);
  int n = x.size();
  cimg_pragma_openmp(parallel for cimg_openmp_if(out.size()>=65536))
  cimg_forXYZC(out,x,y,z,c)
    {
      CId vec(n,1,1,1);
      for (int i = 0; i <n; i++)
	{
	  vec[i] = L.atNXYZC(i,x,y,z,c);
	}
      switch (summary)
	{
	case 0:
	  out(x,y,z,c) = vec.sum(); break;
	case 1:
	  out(x,y,z,c) = vec.min(); break;
	case 2:
	  out(x,y,z,c) = vec.max(); break;
	case 3:
	  out(x,y,z,c) = vec.median(); break;
	case 4:
	  out(x,y,z,c) = vec.variance(); break;
	case 5:
	  out(x,y,z,c) = vec.mean(); break;
	case 6:
	  out(x,y,z,c) = sqrt(vec.pow(2).sum()); break;

	}
    }
  return wrap(out);
}
