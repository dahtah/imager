# imager 0.41 Major release

* Functions based on **plyr** are gradually being phased out in favour of ones based on **purrr** and **dplyr**. **plyr** is no longer loaded by default, which solves some conflicts, but may break existing code! If that's the case just add ```require(plyr)``` somewhere. 

* new functions imeval and imchange. These create custom execution environments for functions, and simplify image pipelines: for example,
``` imeval(boats,~ xs*.) ```
is equivalent to: ``` (Xc(boats)/width(boats))*boats ``` and creates a fading effect. 
``` imchange(boats,~ c==1,~ xs*.) ```
is the same thing but applied only to the first colour channel (R) 
   
* (experimental feature) quick-and-dirty interactive interfaces are now easy to program using the interact() function. Use it to explore filter parameters, interactive segmentation. For more sophisticated needs, use e.g. shiny. 

* (experimental feature) as.igraph methods for images and pixsets convert images into graph representations. Nodes are pixels, and edges are drawn between neighbouring pixels. On pixsets, this allows some interesting morphological operations, e.g. contour tracing. On images, it could be used for various graph-based image processing algorithms, like spectral graph clustering

  * new function "inpaint", which fills in missing parts of an image with a weighted avg. of the neighbours. Useful for salt-and-pepper-ish noise, e.g:
  ```
  im <- boats
  im[sample(nPix(im),1e4)] <- NA
  inpaint(im,1) %>% imlist(im,.) %>%
              setNames(c("before","after")) %>% 
			  plot(layout="row")
  ```
  
* isoblur now has optional na.rm argument, and can ignore missing values. 

  
* new function: colorise, to fill in regions with a certain colour. Also comes with a formula interface, just like imeval. Ex: highlight central region in image
    ``` colorise(boats,~ sqrt(xc^2+yc^2) < 140,"blue",alpha=.2) %>% plot ```
see ?colorise for more. 
	
* new function: load.dir, to load all images in a directory  

* new function: Hough transforms for circles and lines are now available, see hough_circle and hough_line
  
* most functions that take a colour argument now accept colour names, e.g.:
       ```imfill(10,10,val="red")``` or ```autocrop(im,col="black")```

* fixed problem with recent versions of ImageMagick that weren't detected properly
	
# imager 0.40.2 Minor release
	* added crop.bbox for cropping image to the bounding box of a pixset
	* updated CImg: fixes issues with Intel C compiler and libtiff giving pop-up warnings on Windows 
	* threshold now has an "adjust" argument, to adjust the auto-thresholding
	* switched to C++11 
	
# imager 0.40.1 Minor release
	* Updated CImg, imager should now compile on Solaris
	* Fixed memory access bug in px.flood

# imager 0.40 Major release
   * added pixset class to represent sets of pixels in an image (implemented as binary images). A test on an image (e.g., im > 0) results in a pixset object. Pixsets come with many convenience functions for plotting, manipulation, morphology, etc. They are described in the "pixsets" vignette. 
   * improved reductions (parmax, parmin, etc.). Most are now implemented in C++ and some run in parallel using OpenMP. A median combine operation (parmedian) has been added.
   * added load.video, save.video, make.video functions. Loading and saving videos used to be a bit fragile and platform-dependent, it should now be easier and more robust (also slower, sorry). It's now possible to load individual frames from a video. *You still need ffmpeg*. 
   * to load images from URLs imager now uses the *downloader* package, which is more robust than the previous solution
   * it's now possible to import images from the raster package and from the magick package.
   * unified RGB representations so that every function expects RGB values to be in the [0-1] range. There used to be a conflict in expectations here, with R expecting [0-1] and CImg [0-255]. *This might break existing code* (albeit in minor ways). 
   * new function implot, lets you draw on an image using R's base graphics. 
   * improved interactive functions for grabbing image regions (grabRect, grabLine, etc.)
   * improved grayscale conversion
   * improved plotting. The default is now to have a constant aspect ratio that matches the aspect ratio of the image. 
   * save.image now accepts a quality argument when saving JPEGs.
   * native support for TIFF, now supports non-integer values in TIFF files
   * rm.alpha removes alpha channel, flatten.alpha flattens it
   * imfill now accepts  colour names, e.g. `imfill(10,10,val='red')`
   * improved documentation and examples
   * added functions for conversion to/from CIELAB 
   
# imager 0.31 Minor release
   * fixed inline C++ plugin, this time for OS X systems
   * added interpolation option to plot.cimg (to prevent systematic antialiasing when plotting small images)
   
# imager 0.30 Major release. 
  * new S3 class imlist improves support for image lists 
  * enabled OpenMP. Many CImg primitives now run in parallel. 
  * experimental support for CImg's DSL via imeval, patch_summary. 
  * improved as.raster,plot. Now support palettes via colourscale argument.
  * new interactive functions for selecting image regions (grabRect,grabPoint,grabLine)
  * new patchstat function for fast extraction of patch statistics
  * new patchmatch function 
  * cleaned up the API, improved the documentation
  * fixed bug in inline C++ plugin, should now work on Windows. 
  * improved configure script. FFTW3 is now optional (should be less confusing to OSX users)


# imager 0.20. Major release
  * added a tutorial (vignette), which includes a worked-out example of multi-scale blob detection
  * new functions: 
    + iminfo gives you information on an image file (uses ImageMagick)
    + load.example loads test images
    + crop.borders (self-explanatory)
  * long computations can now be interrupted thanks to update in CImg
  * the array subset operator is now more intelligent: you do not have to specify flat dimensions, i.e. 
    `imfill(10,10)[3,1]`   does what you'd expect (i.e., the same as `imfill(10,10)[3,1,1,1]`)
  * improvements and bugs fixed in print.cimg, as.cimg, imresize, frame, plot.cimg, subim, as.raster, renorm
  * improved documentation and examples 

# imager 0.16. Bugfix + minor features
  * as.data.frame.cimg now has optional "wide" formats
  * load.image now uses readbitmap by default (should work better on Windows) 
  * Made the test for ImageMagick more stringent, which fixes a bug in load.image on certain machines
  * Added new reductions which.parmax, which.parmin 

# imager 0.15: Substantial update
  * User-visible changes:
    + the threshold function now supports auto-thresholding (based on k-means)
    + new accessor functions: imrow, imcol
    + new replacement functions: channel(im,1) <-, R(im) <- , etc.
    + new reduction function: enorm (Euclidean norm of a list of images)
    + improved as.cimg.function, pixel.grid
    + subim is now called imsub for greater consistency (imsub still available as alias)
    + improved documentation
  * Other: 
    + Reorganised R code into separate files for clarity

# 0.14 Minor update
  * Build system now uses configure
  * Fixed paths in Makevars
  * Hopeful bugfix in load.image for Windows 7 users 

# 0.13 Initial release on CRAN
