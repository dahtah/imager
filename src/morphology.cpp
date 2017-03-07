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
//' @examples
//' imname <- system.file('extdata/parrots.png',package='imager')
//' im <- load.image(imname) %>% grayscale
//' #Thresholding yields different discrete regions of high intensity
//' regions <- isoblur(im,10) %>% threshold("97%") 
//' labels <- label(regions)
//' layout(t(1:2))
//' plot(regions,"Regions")
//' plot(labels,"Labels")
//' 
// [[Rcpp::export]]
NumericVector label(NumericVector im,bool high_connectivity=false,
double tolerance=0)
{
    CId img = as<CId >(im);
  try{
    img.label(high_connectivity,tolerance);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
NumericVector blabel(LogicalVector im,bool high_connectivity=false)
{
    CIb img = as<CIb >(im);
    CId out;
    try{
      out = img.get_label(high_connectivity,0);
    }
    catch(CImgException &e){
      forward_exception_to_r(e);
    }
    return wrap(out);
}



//' Erode/dilate image by a structuring element.
//'
//' @param im an image
//' @param size size of the structuring element.
//' @param mask Structuring element.
//' @param boundary_conditions Boundary conditions. If FALSE, pixels beyond image boundaries are considered to be 0, if TRUE one. Default: TRUE.
//' @param real_mode If TRUE, perform erosion as defined on the reals. If FALSE, perform binary erosion (default FALSE).
//' @export
//' @examples
//' fname <- system.file('extdata/Leonardo_Birds.jpg',package='imager')
//' im <- load.image(fname) %>% grayscale
//' outline <- threshold(-im,"95%")
//' plot(outline)
//' mask <- imfill(5,10,val=1) #Rectangular mask
//' plot(erode(outline,mask))
//' plot(erode_rect(outline,5,10)) #Same thing
//' plot(erode_square(outline,5)) 
//' plot(dilate(outline,mask))
//' plot(dilate_rect(outline,5,10))
//' plot(dilate_square(outline,5)) 
// [[Rcpp::export]]
NumericVector erode(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool real_mode=false) {
  CId img = as<CId >(im);
  try{
    CId msk = as<CId >(mask);
    img.erode(msk,boundary_conditions,real_mode);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


// [[Rcpp::export]]
LogicalVector berode(LogicalVector im,LogicalVector mask, bool boundary_conditions=true) {
  CIb img = as<CIb >(im);
  try{
    CIb msk = as<CIb >(mask);
    img.erode(msk,boundary_conditions,false);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(img);
}


//' @describeIn erode Erode image by a rectangular structuring element of specified size.
//' @param sx Width of the structuring element.
//' @param sy Height of the structuring element.
//' @param sz Depth of the structuring element.
//' @export
// [[Rcpp::export]]
NumericVector erode_rect(NumericVector im,int sx,int sy,int sz=1) {
  CId img = as<CId >(im);
  try{
    img.erode(sx,sy,sz);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
LogicalVector berode_rect(LogicalVector im,int sx,int sy,int sz=1) {
  CIb img = as<CIb >(im);
  try{
    img.erode(sx,sy,sz);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' @describeIn erode Erode image by a square structuring element of specified size.
//'
//' @export
// [[Rcpp::export]]
NumericVector erode_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  try{
    img.erode(size);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
LogicalVector berode_square(LogicalVector im,int size) {
  CIb img = as<CIb >(im);
  try{
    img.erode(size);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' @describeIn erode Dilate image by a structuring element.
//' @export
// [[Rcpp::export]]
NumericVector dilate(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool real_mode = false) {
  CId img = as<CId >(im);
  CId msk = as<CId >(mask);
  try{
    img.dilate(msk,boundary_conditions,real_mode);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
LogicalVector bdilate(LogicalVector im,LogicalVector mask, bool boundary_conditions=true) {
  CIb img = as<CIb >(im);
  CIb msk = as<CIb >(mask);
  try{
    img.dilate(msk,boundary_conditions,false);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' @describeIn erode Dilate image by a rectangular structuring element of specified size
//' @export
// [[Rcpp::export]]
NumericVector dilate_rect(NumericVector im,int sx,int sy,int sz=1) {
  CId img = as<CId >(im);
  try{
    img.dilate(sx,sy,sz);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


// [[Rcpp::export]]
LogicalVector bdilate_rect(LogicalVector im,int sx,int sy,int sz=1) {
  CIb img = as<CIb >(im);
  try{
    img.dilate(sx,sy,sz);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(img);
}

//' @describeIn erode Dilate image by a square structuring element of specified size
//' @export
// [[Rcpp::export]]
NumericVector dilate_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  try{
    img.dilate(size);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

// [[Rcpp::export]]
LogicalVector bdilate_square(LogicalVector im,int size) {
  CIb img = as<CIb >(im);
  try{
    img.dilate(size);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' Compute watershed transform.
//'
//' The watershed transform is a label propagation algorithm. The value of non-zero pixels will get propagated to their zero-value neighbours. The propagation is controlled by a priority map. See examples. 
//' @param im an image
//' @param priority Priority map.
//' @param fill_lines Sets if watershed lines must be filled or not.
//' @examples
//' #In our initial image we'll place three seeds 
//' #(non-zero pixels) at various locations, with values 1, 2 and 3. 
//' #We'll use the watershed algorithm to propagate these values
//' imd <- function(x,y) imdirac(c(100,100,1,1),x,y)
//' im <- imd(20,20)+2*imd(40,40)+3*imd(80,80)
//' layout(t(1:3))
//' plot(im,main="Seed image")
//' #Now we build an priority map: neighbours of our seeds 
//' #should get high priority. 
//' #We'll use a distance map for that
//' p <- 1-distance_transform(sign(im),1) 
//' plot(p,main="Priority map")
//' watershed(im,p) %>% plot(main="Watershed transform")
//' @export
// [[Rcpp::export]]
NumericVector watershed(NumericVector im,NumericVector priority, bool fill_lines=true) {
  CId img = as<CId >(im);
  CId pri = as<CId >(priority);
  try{
    img.watershed(pri,fill_lines);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
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
//' @examples
//' imd <- function(x,y) imdirac(c(100,100,1,1),x,y)
//' #Image is three white dots
//' im <- imd(20,20)+imd(40,40)+imd(80,80)
//' plot(im)
//' #How far are we from the nearest white dot? 
//' distance_transform(im,1) %>% plot
// [[Rcpp::export]]
NumericVector distance_transform(NumericVector im,double value,unsigned int metric=2)
{
  CId img = as<CId >(im);
  try{
    img.distance(value,metric);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


// [[Rcpp::export]]
NumericVector bdistance_transform(LogicalVector im,bool value=true,unsigned int metric=2)
{
  CIb img = as<CIb >(im);
  CId out;
  try{
    out = img.get_distance(value,metric);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(out);
}


//' @describeIn erode Morphological opening (erosion followed by dilation)
//' @export
// [[Rcpp::export]]
NumericVector mopening(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool real_mode = false) {
  CId img = as<CId >(im);

  try{
    CId msk = as<CId >(mask);
    img.erode(msk,boundary_conditions,real_mode).dilate(msk,boundary_conditions,real_mode);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}


//' @describeIn erode Morphological opening by a square element (erosion followed by dilation)
//' @export
// [[Rcpp::export]]
NumericVector mopening_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  try{
    img.erode(size).dilate(size);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
  }
  return wrap(img);
}

//' @describeIn erode Morphological closing by a square element (dilation followed by erosion)
//' @export
// [[Rcpp::export]]
NumericVector mclosing_square(NumericVector im,int size) {
  CId img = as<CId >(im);
  
  try{
    img.dilate(size).erode(size);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

//' @describeIn erode Morphological closing (dilation followed by erosion)
//' @export
// [[Rcpp::export]]
NumericVector mclosing(NumericVector im,NumericVector mask, bool boundary_conditions=true,bool real_mode = false) {
  CId img = as<CId >(im);

  try{
    CId msk = as<CId >(mask);
    img.dilate(msk,boundary_conditions,real_mode).erode(msk,boundary_conditions,real_mode);
    }
  catch(CImgException &e){
    forward_exception_to_r(e);
    
  }
  return wrap(img);
}

