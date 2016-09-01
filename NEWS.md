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
