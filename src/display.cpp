#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"
using namespace Rcpp;
using namespace cimg_library;


//' @export
// [[Rcpp::export]]
void display(NumericVector im)
{
   CImg<double> img = as<CImg<double> >(im);
   img.display();
   return;
}

//' @export
// [[Rcpp::export]]
void display_list(List imlist)
{
   CImgList<double> L = sharedCImgList(imlist);
   L.display();
   return;
}

//' @export
// [[Rcpp::export]]
void play(NumericVector inp,bool loop=false,int delay=30)
{
    CImg<double> img = as<CImg<double> >(inp);
    CImgDisplay disp(img.get_slice(0),"My images");
    int i = 0,n=img.depth();
    while (true)
      {
	img.get_slice(i).display(disp);
	if (i == n)
	  {
	    if (loop)
	      {
		i = 0;
	      }
	    else {
	      break;
	    }
	  }
	
	if (disp.is_closed())
	  {
	    break;
	  }
	else
	  {
	    i++;
	  }
	disp.wait(delay);
	Rcpp::checkUserInterrupt();
      }

    return;
}
