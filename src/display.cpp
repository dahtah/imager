#include <imager.h>
#include "wrappers_cimglist.h"

using namespace Rcpp;
using namespace cimg_library;

//' Display image using CImg library
//'
//' Press escape or close the window to exit.
//'
//' @param im an image (cimg object)
//' @param normalise if true pixel values are rescaled to 0...255 (default TRUE)
//' @export
//' @examples
//' ##Not run: interactive only 
//' ##display(boats,TRUE) #Normalisation on 
//' ##display(boats/2,TRUE) #Normalisation on, so same as above
//' ##display(boats,FALSE) #Normalisation off
//' ##display(boats/2,FALSE) #Normalisation off, so different from above
// [[Rcpp::export]]
void display(NumericVector im,bool normalise=true)
{
   CId img = as<CId >(im);
   int norm;
   if (normalise)
     {
       norm = 3;
     }
   else
     {
       norm = 0;
     }
   CImgDisplay disp(img,"",norm,false,false);
   while (true)
      {
	if (disp.is_closed() or disp.is_key(cimg::keyESC))
	  {
	    break;
	  }
	Rcpp::checkUserInterrupt();
      }

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
//' @param normalise if true pixel values are rescaled to 0...255 (default TRUE). The normalisation is based on the *first frame*. If you don't want the default behaviour you can normalise by hand. Default TRUE.
//' @export
// [[Rcpp::export]]
void play(NumericVector vid,bool loop=false,unsigned long delay=30,bool normalise=true)
{
  unsigned long t0 = cimg::time();
  unsigned long dt;
  CId img = as<CId >(vid);
  int norm;
  if (normalise)
    {
      norm = 2;
    }
  else
    {
      norm = 0;
     }
  
  CImgDisplay disp(img.get_slice(0),"Video player",norm,false,false);
  int i = 0,n=img.depth();
  bool pause=false;
  while (true)
      {
	dt = cimg::time() - t0;
	//Time to update the display
	if ((dt >= delay) and (!pause))
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
