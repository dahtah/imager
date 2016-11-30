#ifndef CIMGLIST_WRAP
#define CIMGLIST_WRAP
cimg_library::CImgList<bool > sharedCImgList_bool(SEXP inp);
cimg_library::CImg<double> sharedCImg(SEXP inp);
cimg_library::CImg<bool> sharedCImg_bool(SEXP inp);
cimg_library::CImgList<double> sharedCImgList(SEXP inp);
#endif
