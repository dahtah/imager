//Hough transforms for lines and circles
#include <imager.h>

using namespace cimg_library;
using namespace Rcpp;

// Classical Hough transform
// adapted from _hough_line in scikit-image
// [[Rcpp::export]]
NumericVector hough_line_px(LogicalVector px,NumericVector theta)
{
  CIb ppx = as<CIb >(px);
  NumericVector ctheta = cos(theta),stheta = sin(theta);
  int maxd = 2*ceil(sqrt(pow( (double) ppx.width(),2)+pow( (double) ppx.height(),2)));
  int offset = maxd/2;
  CId out(maxd,theta.length());
  out.fill(0);
  cimg_forXY(ppx,x,y)
    {
      if (ppx(x,y))
	{
	  for (int j = 0; j < theta.length(); j++)
	    {
	      int accum_idx = ((int) round((ctheta[j] * x + stheta[j] * y))) + offset;
	      out(accum_idx,j)++;
	    }
	}
    }
  return wrap(out);
}

// Fast Hough transform through gradient voting
// Adapted from code by D. Tschumperlé in CImg examples
// [[Rcpp::export]]
NumericVector hough_line_grad(NumericVector im,int ntheta,double alpha=1.5)
{
  CId img = as<CId >(im);
  CImgList<> grad = img.get_gradient();
  cimglist_for(grad,l) grad[l].blur((float)alpha);

  double thetamax = 2*cimg::PI,
    rhomax = 2*ceil(sqrt(pow(img.width(),2.0)+pow(img.height(),2.0)));
  int offset = rhomax/2;
  CId out(rhomax,ntheta);
  out.fill(0);
  cimg_forXY(img,x,y)
    {
      const double
	gx = grad[0](x,y),
	gy = grad[1](x,y);
      double theta = std::atan2(gy,gx),
	      rho   = std::sqrt(x*x + y*y)*std::cos(std::atan2(y,x) - theta);
      theta = cimg::mod(theta,thetamax);
      int indt = (int)(theta*out.height()/thetamax),
	indr = (int)(round(rho) + offset);
      out(indr,indt)+= std::sqrt(gx*gx + gy*gy);
    }
  return wrap(out);
}


// [[Rcpp::export]]
NumericVector hough_circle_(LogicalVector px, int radius)
{
  CIb ppx = as<CIb >(px);
  CId out(ppx,"xy",0);
  int col[]={1};
  cimg_forXY(out,x,y)
    {
      if (ppx(x,y))
	{
	  out.draw_circle(x,y,radius,col,.1,1);
	}
    }
  return wrap(out);
}
