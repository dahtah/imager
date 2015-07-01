Imager is an image/video processing package for R, based on [CImg](http://cimg.sourceforge.net/), a C++ library by David Tschumperlé. CImg provides an easy-to-use and consistent API for image processing, which imager largely replicates. CImg supports images in up to four dimensions, which makes it suitable for applications like video processing/hyperspectral imaging/MRI.

## How to install

### The package

Install the devtools package if you haven't already. Then run:

	library(devtools)
	install_github("dahtah/imager")

So far the package is only available as source code, so you'll need a functional R build environment (Rtools on Windows, XCode on OS X). To build under Linux make sure you have the headers for libX11 and libfftw3.

### External dependencies

On its own imager only supports JPEG and PNG formats. If you need support for other file types install [ImageMagick](http://www.imagemagick.org/script/binary-releases.php).
To load videos you'll need [ffmpeg](http://ffmpeg.org/download.html), no file formats are supported natively.


## Getting started 

	tennis <- load.image(system.file('extdata/tennis_sif.mpeg',package='imager'))
	play(tennis)
	#now filter in the time direction and pipe to play
	deriche(tennis,10,axis="z") %>% play

Documentation is available [here](http://dahtah.github.io/imager/). To get a list of all package functions, run:
	ls(pos = "package:imager")

## Important warning on memory usage

All images are stored as standard R numeric vectors (i.e., double-precision), meaning that they take up a lot of memory. It's easy to underestimate how much storage you need for videos, because they take up so little space in a compressed format. Before you can work on it in R a video has to be fully decompressed and stored as double-precision floats. To get a sense of the size, consider a low-resolution (400x300), colour video lasting 120 sec. The video might take up a few MBs when compressed. To store it in memory, you'll need:
(400x300) x (25x120) x 3
values, corresponding to (space)x(time)x(colour). In addition, each value costs 8 bytes of storage, for a grand total of 8GB of memory.

Future versions of imager will have facilities for out-of-memory storage of video data, but in the meantime don't try to load large videos.


## Current status

- Most CImg functions for filtering, morphology and colourspace conversions have R interfaces. The function calls and documentation is mostly verbatim copy from the original C++ API.
- There isn't a whole lot of argument checking so you might crash your session if you try something unexpected. Please report it if that happens.
