#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;


// [[Rcpp::export]]
NumericVector getXc(int x,int y, int z, int c)
{
 CId im(x,y,z,c);
 cimg_forXYZC(im,xi,yi,zi,ci) { im(xi,yi,zi,ci) = xi; }
 return wrap(im);
}

// [[Rcpp::export]]
NumericVector getYc(int x,int y, int z, int c)
{
 CId im(x,y,z,c);
 cimg_forXYZC(im,xi,yi,zi,ci) { im(xi,yi,zi,ci) = yi; }
 return wrap(im);
}

// [[Rcpp::export]]
NumericVector getZc(int x,int y, int z, int c)
{
 CId im(x,y,z,c);
 cimg_forXYZC(im,xi,yi,zi,ci) { im(xi,yi,zi,ci) = zi; }
 return wrap(im);
}

// [[Rcpp::export]]
NumericVector getCc(int x,int y, int z, int c)
{
 CId im(x,y,z,c);
 cimg_forXYZC(im,xi,yi,zi,ci) { im(xi,yi,zi,ci) = ci; }
 return wrap(im);
}
