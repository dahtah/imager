#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

//' Label connected components.
//'
//' The algorithm of connected components computation has been primarily done
//'by A. Meijster, according to the publication:
//''W.H. Hesselink, A. Meijster, C. Bron, "Concurrent Determination of Connected Components.",
//'       In: Science of Computer Programming 41 (2001), pp. 173--194'.
//'
//' @param im an image
//' @param high_connectivity   4(false)- or 8(true)-connectivity
//'       in 2d case, and between 6(false)- or 26(true)-connectivity in 3d case. Default FALSE
//' @param tolerance Tolerance used to determine if two neighboring pixels belong to the same region.
//' @export
// [[Rcpp::export]]
NumericVector label(NumericVector im,bool high_connectivity=false,
double tolerance=0)
{
  CId img = as<CId >(im);
  img.label(high_connectivity,tolerance);
  return wrap(img);
}

//' Erode image by a structuring element.
//'
//' @param im an image
//' @param mask Structuring element.
//'       @param boundary_conditions Boundary conditions.
//' @param normalise Determines if the closing is locally normalised (default FALSE)
//'
//' @export
// [[Rcpp::export]]
NumericVector erode(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool normalise = false) {
  CId img = as<CId >(im);
  CId msk = as<CId >(mask);
  img.erode(msk,boundary_conditions,normalise);
  return wrap(img);
}

//' Erode image by a rectangular structuring element of specified size.
//' @param im an image
//'       @param sx Width of the structuring element.
//'       @param sy Height of the structuring element.
//'       @param sz Depth of the structuring element.
//'
//'
//' @export
// [[Rcpp::export]]
NumericVector erode_rect(NumericVector im,int sx,int sy,int sz=1) {
  CId img = as<CId >(im);
  img.erode(sx,sy,sz);
  return wrap(img);
}

//' Erode image by a  square structuring element of specified size.
//' @param im an image
//'       @param size size of the structuring element.
//'
//' @export
// [[Rcpp::export]]
NumericVector erode_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  img.erode(size);
  return wrap(img);
}

//' Dilate image by a structuring element.
//' @param im an image
//'      @param mask Structuring element.
//'       @param boundary_conditions Boundary conditions.
//'       @param normalise  Normalise mask (default FALSE)
//' @export
// [[Rcpp::export]]
NumericVector dilate(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool is_normalised = false) {
  CId img = as<CId >(im);
  CId msk = as<CId >(mask);
  img.dilate(msk,boundary_conditions,normalise);
  return wrap(img);
}

//' Dilate image by a rectangular structuring element of specified size.
//'
//' @param im an image
//'       @param sx Width of the structuring element.
//'       @param sy Height of the structuring element.
//'       @param sz Depth of the structuring element.
//' @export
// [[Rcpp::export]]
NumericVector dilate_rect(NumericVector im,int sx,int sy,int sz=1) {
  CId img = as<CId >(im);
  img.dilate(sx,sy,sz);
  return wrap(img);
}

//' Dilate image by a square structuring element of specified size.
//'
//' @param im an image
//'       @param size Size of the structuring element.
//' @export
// [[Rcpp::export]]
NumericVector dilate_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  img.dilate(size);
  return wrap(img);
}

//' Compute watershed transform.
//'
//'       Non-zero values are propagated to zero-valued ones according to
//'       the priority map.
//' @param im an image
//'       @param priority Priority map.
//'       @param fill_lines Sets if watershed lines must be filled or not.
//'
//' @export
// [[Rcpp::export]]
NumericVector watershed(NumericVector im,NumericVector priority, bool fill_lines=true) {
  CId img = as<CId >(im);
  CId pri = as<CId >(priority);
  img.watershed(pri,fill_lines);
  return wrap(img);
}


//' Compute Euclidean distance function to a specified value.
//'
//'        The distance transform implementation has been submitted by A. Meijster, and implements
//'        the article 'W.H. Hesselink, A. Meijster, J.B.T.M. Roerdink,
//'                     "A general algorithm for computing distance transforms in linear time.",
//'                     In: Mathematical Morphology and its Applications to Image and Signal Processing,
//'                     J. Goutsias, L. Vincent, and D.S. Bloomberg (eds.), Kluwer, 2000, pp. 331-340.'
//'         The submitted code has then been modified to fit CImg coding style and constraints.
//' @param im an image
//' @param value Reference value.
//' @param metric Type of metric. Can be <tt>{ 0=Chebyshev | 1=Manhattan | 2=Euclidean | 3=Squared-euclidean }</tt>.
//' @export
// [[Rcpp::export]]
NumericVector distance_transform(NumericVector im,double value,unsigned int metric=2)
{
  CId img = as<CId >(im);
  img.distance(value,metric);
  return wrap(img);
}


//' Morphological opening (erosion followed by dilation)
//'
//' @param im an image
//' @param mask Structuring element.
//' @param boundary_conditions Boundary conditions.
//' @param normalise Determines if the closing is locally normalised (default FALSE)
//'
//' @export
// [[Rcpp::export]]
NumericVector mopening(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool normalise = false) {
  CId img = as<CId >(im);
  CId msk = as<CId >(mask);
  img.erode(msk,boundary_conditions,normalise).dilate(msk,boundary_conditions,normalise);
  return wrap(img);
}


//' Morphological opening by a square element (erosion followed by dilation)
//'
//' @param im an image
//' @param size size of the square element
//'
//' @export
// [[Rcpp::export]]
NumericVector mopening_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  img.erode(size).dilate(size);
  return wrap(img);
}

//' Morphological closing by a square element (dilation followed by erosion)
//'
//' @param im an image
//' @param size size of the square element
//'
//' @export
// [[Rcpp::export]]
NumericVector mclosing_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  img.dilate(size).erode(size);
  return wrap(img);
}

//' Morphological closing (dilation followed by erosion)
//'
//' @param im an image
//' @param mask Structuring element.
//' @param boundary_conditions Boundary conditions.
//' @param normalise Determines if the closing is locally normalised (default FALSE)
//'
//' @export
// [[Rcpp::export]]
NumericVector mclosing(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool normalise = false) {
  CId img = as<CId >(im);
  CId msk = as<CId >(mask);
  img.dilate(msk,boundary_conditions,normalise).erode(msk,boundary_conditions,normalise);
  return wrap(img);
}

