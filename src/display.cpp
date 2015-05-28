#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"
using namespace Rcpp;
using namespace cimg_library;

//' Display image using CImg library
//'
//' @param im an image (cimg object)
//' @export
// [[Rcpp::export]]
void display(NumericVector im)
{
   CImg<double> img = as<CImg<double> >(im);
   img.display();
   return;
}

//' Display image list using CImg library
//'
//' @param imlist a list of cimg objects
//' @export
// [[Rcpp::export]]
void display_list(List imlist)
{
   CImgList<double> L = sharedCImgList(imlist);
   L.display();
   return;
}

//' Play a video 
//'
//' A very basic video player. Press the space bar to pause and ESC to close. 
//' @param vid A cimg object, to be played as video
//' @param loop loop the video (default false)
//' @param delay delay between frames, in ms. Default 30. 
//' @export
// [[Rcpp::export]]
void play(NumericVector vid,bool loop=false,unsigned long delay=30)
{
  unsigned long t0 = cimg::time();
  unsigned long dt;
  CImg<double> img = as<CImg<double> >(vid);
  CImgDisplay disp(img.get_slice(0),"My images");
  int i = 0,n=img.depth();
  bool pause=false;
  while (true)
      {
	dt = cimg::time() - t0;
	// Rcout << "dt " << dt << std::endl;
	// Rcout << "time " << cimg::time() << std::endl;
	// Rcout << "t0 " << t0 << std::endl;
	//Time to update the display
	if ((dt >= delay) and (!pause))
	  {
	    //	    Rcout << "updating" << std::endl;
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
	    t0 = cimg::time();
	    i++;
	  }
	
	if (disp.is_closed() or disp.is_key(cimg::keyESC))
	  {
	    break;
	  }
	if (disp.is_key(cimg::keySPACE))
	  {
	    pause = !pause;
	    if (!pause)
	      {
	        t0 =cimg::time();
	      }
	    disp.flush();
	  }
	Rcpp::checkUserInterrupt();
      }

    return;
}
