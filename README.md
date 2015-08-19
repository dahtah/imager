Imager is an image/video processing package for R, based on [CImg](http://cimg.eu/), a C++ library by David Tschumperlé. CImg provides an easy-to-use and consistent API for image processing, which imager largely replicates. CImg supports images in up to four dimensions, which makes it suitable for applications like video processing/hyperspectral imaging/MRI.

## Installing the package

Install the devtools package if you haven't already. On Linux or OS X run:

	devtools::install_github("dahtah/imager")

If that doesn't work then you're probably missing a build environment or a library, see below.

On Windows you can grab a binary package (it's possibly outdated, see below)

	devtools::install_url("https://github.com/dahtah/imager/releases/download/snapshot_080815/imager_win.zip")



### OS X

You'll need Xcode (OS X's development environment) to compile source packages. The FFTW library is needed, and the easiest way to install it is via [Homebrew](http://brew.sh/). Install Homebrew, then run:
	brew install fftw

### Windows

Building R packages on Windows is a bit of a pain so you're probably better off with the binary package (which may not be up-to-date). If you need the latest version of imager, you'll have to:

- Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/index.html)
- Install [additional libraries](http://www.stats.ox.ac.uk/pub/Rtools/libs.html) for Rtools. You want the package that's called "local tree". Put those libraries somewhere gcc can find them. 



To build under Linux make sure you have the headers for libX11 and libfftw3.
	sudo apt-get install libfftw3-dev libX11-dev


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

For out-of-memory processing of videos, see the experimental package [imagerstreams](https://github.com/dahtah/imagerstreams). 


## Current status

Imager is fully functional but still young, so the API might change. Open an issue on Github or email me if you've found a bug or would like to suggest a feature.

## Test pictures

Imager ships with three test pictures and a video. Two (parrots and boats) come from the [Kodak set](http://r0k.us/graphics/kodak/). Another is a sketch of birds by Leonardo, from Wikimedia. The test video comes from [xiph.org](https://media.xiph.org/video/derf/)'s collection.
