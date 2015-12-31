#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

// [[Rcpp::export]]
NumericVector interp_xy(NumericVector inp,NumericVector ix,NumericVector iy, int z=0,int c=0,bool cubic=false)
{
    CId img = as<CId >(inp);
    int n = ix.length();
    double val;
    NumericVector out(n);

    for (int ind = 0; ind < n; ind++)
      {
	if (cubic)
	  {
	    val = img.cubic_atXY(ix[ind],iy[ind],z,c);
	  }
	else
	  {
	    val = img.linear_atXY(ix[ind],iy[ind],z,c);
	  }
	out[ind] = val;
      }
    
    return wrap(out);
}

// [[Rcpp::export]]
NumericVector interp_xyz(NumericVector inp,NumericVector ix,NumericVector iy,NumericVector iz,int c=0,bool cubic=false)
{
    CId img = as<CId >(inp);
    int n = ix.length();
    double val;
      NumericVector out(n);

    for (int ind = 0; ind < n; ind++)
      {
	if (cubic)
	  {
	    val = img.cubic_atXYZ(ix[ind],iy[ind],iz[ind],c);
	  }
	else
	  {
	    val = img.linear_atXYZ(ix[ind],iy[ind],iz[ind],c);
	  }
	out[ind] = val;
      }
    
    return wrap(out);
}


// [[Rcpp::export]]
NumericVector interp_xyzc(NumericVector inp,NumericVector ix,NumericVector iy,NumericVector iz,IntegerVector ic,bool cubic=false)
{
    CId img = as<CId >(inp);
    int n = ix.length();
    double val;
    NumericVector out(n);
    
    for (int ind = 0; ind < n; ind++)
      {
	if (cubic)
	  {
	    val = img.cubic_atXYZ(ix[ind],iy[ind],iz[ind],ic[ind]);
	  }
	else
	  {
	    val = img.linear_atXYZ(ix[ind],iy[ind],iz[ind],ic[ind]);
	  }
	out[ind] = val;
      }
    
    return wrap(out);
}

// [[Rcpp::export]]
NumericVector interp_xyc(NumericVector inp,NumericVector ix,NumericVector iy,int z,IntegerVector ic,bool cubic=false)
{
    CId img = as<CId >(inp);
    int n = ix.length();
    double val;
    NumericVector out(n);

    for (int ind = 0; ind < n; ind++)
      {
	if (cubic)
	  {
	    val = img.cubic_atXY(ix[ind],iy[ind],z,ic[ind]);
	  }
	else
	  {
	    val = img.linear_atXY(ix[ind],iy[ind],z,ic[ind]);
	  }
	out[ind] = val;
      }
    
    return wrap(out);
}
