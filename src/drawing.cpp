#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

// [[Rcpp::export]]
NumericVector bucket_fill(NumericVector im,int x,int y,int z,NumericVector color,  float opacity=1,float sigma=0,bool high_connexity=false)
{
  CId img = as<CId >(im);
  try{
    img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,sigma,high_connexity);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
LogicalVector bucket_select(NumericVector im,int x,int y,int z,float sigma=0,bool high_connexity=false)
{
  CId img = as<CId >(im);
  CIb out(img,"xyzc");
  NumericVector color(img.spectrum());
  float opacity=1;

  try{
    img.draw_fill(x-1,y-1,z-1,color.begin(),opacity,out,sigma,high_connexity);
  

  }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(out);
}


// [[Rcpp::export]]
NumericVector draw_circle(NumericVector im,IntegerVector x, IntegerVector y,int radius,NumericVector color,double opacity=1,bool filled=true)
{
  CId img = as<CId >(im);
  for (int i = 0; i < x.length(); i++)
    {
      if (filled)
	{
	  img.draw_circle(x[i]-1,y[i]-1,radius,color.begin(),opacity);
	}
      else
	{
	  img.draw_circle(x[i]-1,y[i]-1,radius,color.begin(),opacity,0);
	}
    }
  return wrap(img);
}


// [[Rcpp::export]]
NumericVector draw_circle_(NumericVector im,IntegerVector x, IntegerVector y,IntegerVector radius,NumericMatrix color,NumericVector opacity=1,bool filled=true)
{
  CId img = as<CId >(im);
  for (int i = 0; i < x.length(); i++)
    {
      NumericVector col = color(i,_);
      if (filled)
	{
	  img.draw_circle(x[i]-1,y[i]-1,radius[i],col.begin(),opacity[i]);
	}
      else
	{
	  img.draw_circle(x[i]-1,y[i]-1,radius[i],col.begin(),opacity[i],0);
	}
    }
  return wrap(img);
}
  
// [[Rcpp::export]]
NumericVector draw_rect_(NumericVector im,IntegerVector x0, IntegerVector y0,IntegerVector x1, IntegerVector y1,NumericVector color,double opacity=1,bool filled=true)
{
  CId img = as<CId >(im);
  for (int i = 0; i < x0.length(); i++)
    {
      if (filled)
	{
	  img.draw_rectangle(x0[i]-1,y0[i]-1,x1[i]-1,y1[i]-1,color.begin(),opacity);
	}
      else
	{
	  img.draw_rectangle(x0[i]-1,y0[i]-1,x1[i]-1,y1[i]-1,color.begin(),opacity,0);
	}
    }
  return wrap(img);
}

// [[Rcpp::export]]
NumericVector draw_poly_(NumericVector im,NumericVector points, NumericVector color,float opacity=1)
{
  CId img = as<CId >(im);
  CId pts = as<CId >(points);
  img.draw_polygon(pts,color.begin(),opacity);
  return wrap(img);
}


// [[Rcpp::export]]
NumericVector draw_text_(NumericVector im,int x, int y,std::string text, NumericVector color,double opacity=1,int fsize=20)
{
  CId img = as<CId >(im);
  img.draw_text(x-1,y-1,text.c_str(),color.begin(),0,opacity,fsize);
  return wrap(img);
}
