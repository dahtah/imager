#ifndef IMAGER_H
#define IMAGER_H



#if defined(Rcpp_hpp) && !defined(COMPILING_IMAGER)
#error "The file 'Rcpp.h' should not be included. Please correct to include only 'imager.h'."
#endif

#include <R.h>
#include "CImg.h"
#include <Rcpp.h>
#include "wrappers.h"

typedef cimg_library::CImg<double> CId;
typedef cimg_library::CImg<int> CIi;
typedef cimg_library::CImg<float> CIf;
#endif
