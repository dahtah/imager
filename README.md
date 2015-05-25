Imager is an image/video processing package for R, based on CImg, a C++ library by David Tschumperlé. CImg provides an easy-to-use and consistent API for image processing, which imager largely replicates. CImg supports images in up to four dimensions, which makes it suitable for applications like video processing/hyperspectral imaging/MRI.

## How to install

Install the devtools package if you haven't already. Then run:

	library(devtools)
	install_github("dahtah/imager")

So far the package is only available as source code, so you'll need a functional R build environment (Rtools on Windows, XCode on OS X). To build under Linux make sure you have the headers for libX11. 
You'll also need ImageMagick and ffmpeg somewhere on your path if you want to import and save images. 


## Getting started 

	tennis <- load.image(system.file('extdata/tennis_sif.mpeg',package='imager'))
	play(tennis)
	#now filter in the time direction and pipe to play
	deriche(tennis,10,axis="z") %>% play

Documentation is available [here](http://dahtah.github.io/imager/). To get a list of all package functions, run:
	ls(pos = "package:imager")



## Current status

- Most CImg functions for filtering, morphology and colourspace conversions have R interfaces. The function calls and documentation is mostly verbatim copy from the original C++ API.
- There isn't a whole lot of argument checking so you might crash your session if you try something unexpected. Please report it if that happens.
- All images are stored as standard R numeric vectors (i.e., double-precision), meaning that they take up a lot of memory. On top of that there's some duplication going on in the back-and-forth between R and C++. Don't try to process 20 min. of video.
