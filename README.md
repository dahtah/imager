[![Linux Build Status](https://travis-ci.org/dahtah/imager.png?branch=master)](https://travis-ci.org/dahtah/imager/)
[![CRAN Version](http://www.r-pkg.org/badges/version/imager)](https://cran.r-project.org/package=imager)

Imager is an image/video processing package for R, based on [CImg](http://cimg.eu/), a C++ library by David Tschumperlé. CImg provides an easy-to-use and consistent API for image processing, which imager largely replicates. CImg supports images in up to four dimensions, which makes it suitable for applications like video processing/hyperspectral imaging/MRI.

## Installing the package

Imager is now on CRAN, so

	install.packages("imager")

should do the trick. You may also want to install ImageMagick and ffmpeg, see "External Dependencies" below. 

The version of CRAN will often lag the one on github. If you'd like to install the latest version, you'll have to build the package from source. 

Install the devtools package if you haven't already. Run:

	devtools::install_github("dahtah/imager")

If that doesn't work then you're probably missing a build environment or a library, see below.


### OS X

Install [XQuartz](https://www.xquartz.org/) if you haven't already (it's required for the interactive functions). 
You'll need Xcode (OS X's development environment) to compile source packages. The FFTW library is needed, and the easiest way to install it is via [Homebrew](http://brew.sh/). Install Homebrew, then run:
	brew install fftw

### Windows

Building R packages on Windows is a bit of a pain so you're probably better off with the binary package (which may not be up-to-date). If you need the latest version of imager, you'll have to:

- Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/index.html)
- Install [additional libraries](http://www.stats.ox.ac.uk/pub/Rtools/libs.html) for Rtools. You want the package that's called "local tree". Put those libraries somewhere gcc can find them. 

### Linux

To build under Linux make sure you have the headers for libX11 and libfftw3. On my Ubuntu system this seems to be enough:

	sudo apt-get install libfftw3-dev libX11-dev


### External dependencies

OS X users need [XQuartz](https://www.xquartz.org/). 
On its own imager only supports JPEG, PNG and BMP formats. If you need support for other file types install [ImageMagick](http://www.imagemagick.org/script/binary-releases.php).
To load videos you'll need [ffmpeg](http://ffmpeg.org/download.html), no file formats are supported natively.


## Getting started 

Here's a small demo that actually demonstrates an interesting property of colour perception:

	library(imager)
	library(purrr)
	parrots <- load.example("parrots")
	plot(parrots)
	#Define a function that converts to YUV, blurs a specific channel, and converts back
	bchan <- function(im,ind,sigma=5) { 
		im <- RGBtoYUV(im)
		channel(im,ind) <- isoblur(channel(im,ind),sigma); 
		YUVtoRGB(im)
	}
	#Run the function on all three channels and collect the results as a list
	blurred <- map_il(1:3,~ bchan(parrots,.))
	names(blurred) <- c("Luminance blur (Y)","Chrominance blur (U)","Chrominance blur (V)")
	plot(blurred)
	
We're much more [sensitive to luminance edges than we are to colour edges](https://en.wikipedia.org/wiki/Chroma_subsampling). 

Documentation is available [here](http://dahtah.github.io/imager/). To get a list of all package functions, run:
	ls(pos = "package:imager")

## Important warning on memory usage

All images are stored as standard R numeric vectors (i.e., double-precision), meaning that they take up a lot of memory. It's easy to underestimate how much storage you need for videos, because they take up so little space in a compressed format. Before you can work on it in R a video has to be fully decompressed and stored as double-precision floats. To get a sense of the size, consider a low-resolution (400x300), colour video lasting 120 sec. The video might take up a few MBs when compressed. To store it in memory, you'll need:
(400x300) x (25x120) x 3
values, corresponding to (space)x(time)x(colour). In addition, each value costs 8 bytes of storage, for a grand total of 8GB of memory.

For out-of-memory processing of videos, see the experimental package [imagerstreams](https://github.com/dahtah/imagerstreams). 


## Current status

Imager is fully functional but still young, so the API might change. Open an issue on Github or email me if you've found a bug or would like to suggest a feature.

## Test pictures

Imager ships with four test pictures and a video. Two (parrots and boats) come from the [Kodak set](http://r0k.us/graphics/kodak/). Another is a sketch of birds by Leonardo, from Wikimedia. Also from Wikimedia: the Hubble Deep field. 
The test video comes from [xiph.org](https://media.xiph.org/video/derf/)'s collection.
