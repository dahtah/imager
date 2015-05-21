imager: an R package for image processing
=========================================

Simon Barthelmé (GIPSA-lab)

R is a bit lacking in the area of image processing, where Matlab is
still king. Imager aims to make image processing work in R easier. It is
based on CImg, a C++ library by David Tschumperlé. CImg provides an
easy-to-use and consistent API for image processing, which imager
largely replicates. CImg supports images in up to four dimensions, which
makes it suitable for basic video processing/hyperspectral imaging as
well.

At this stage imager covers:

-   loading and saving images in various formats
-   displaying and plotting images
-   interpolation and resizing
-   filtering and FFTs
-   morphological operations

Quick start
-----------

Here's an example of imager in action:

    library(imager)
    im <- load.image(system.file('extdata/parrots.png',package='imager'))
    plot(im) #Parrots!

![](imager_files/figure-markdown_strict/quickstart-1.png)

    im.blurry <- isoblur(im,10) #Blurry parrots!
    plot(im.blurry)

![](imager_files/figure-markdown_strict/quickstart-2.png)

    im.xedges <- deriche(im,2,order=2,axis="x") #Edge detector along x-axis
    plot(im.xedges)

![](imager_files/figure-markdown_strict/quickstart-3.png)

    im.yedges <- deriche(im,2,order=2,axis="y") #Edge detector along y-axis
    plot(im.yedges)

![](imager_files/figure-markdown_strict/quickstart-4.png)

    #Chain operations using the pipe operator (maggritr)
    deriche(im,2,order=2,axis="x") %>% deriche(2,order=2,axis="y") %>% plot

![](imager_files/figure-markdown_strict/quickstart-5.png)

    #Another example of chaining: image gradient along x and y axes
    layout(matrix(1:2,1,2));
    grayscale(im) %>% get_gradient(axes="xy") %>% l_ply(plot)

<img src="imager_files/figure-markdown_strict/quickstart2-1.png" title="" alt="" width="12" />

    #Can load videos as well:
    tennis <- load.image(system.file('extdata/tennis_sif.mpeg',package='imager'))

    plot(tennis,frame=1)

<img src="imager_files/figure-markdown_strict/quickstart2-2.png" title="" alt="" width="12" />

How images are represented
--------------------------

Images are represented as 4D numeric arrays, which is consistent with
CImg's storage standard (it is unfortunately inconsistent with other R
libraries, like spatstat, but converting between representations is
easy). The four dimensions are labelled x,y,z,c. The first two are the
usual spatial dimensions, the third one will usually correspond to depth
or time, and the fourth one is colour. Remember the order, it will be
used consistently in imager. If you only have grayscale images then the
two extra dimensions are obviously pointless, but they won't bother you
much. Your objects will still be officially 4 dimensional, with two
trailing flat dimensions. Pixels are stored in the following manner: we
scan the image beginning at the upper-left corner, along the x axis.
Once we hit the end of the scanline, we move to the next line. Once we
hit the end of the screen, we move to the next frame (increasing z) and
repeat the process. If we have several colour channels, then once we're
done with the first colour channel we move to the next one. All in all
the different dimensions are represented in the x,y,z,c order. In R the
object is represented as a 4D array. Here's an example with a grayscale
image:

    parrots <- load.image(system.file('extdata/parrots.png',package='imager'))
    gray.parrots <- grayscale(parrots)
    dim(gray.parrots)

    ## [1] 768 512   1   1

and a colour image:

    dim(parrots)

    ## [1] 768 512   1   3

and finally a video, also in colour:

    dim(tennis)

    ## [1] 352 240 150   3

Coordinates
-----------

CImg uses standard image coordinates: the origin is at the top left
corner, with the x axis pointing right and the y axis pointing *down*.
imager uses the same coordinate system, except the origin is now (1,1)
and not (0,0) (the reason being that R indices start at 1 and not at 0).
The number of pixels along the x axis is called the width, along the y
axis it's height, along the z axis it's depth and finally the number of
colour channels is called "spectrum".

    width(parrots)

    ## [1] 768

    height(parrots)

    ## [1] 512

    depth(parrots)

    ## [1] 1

    spectrum(parrots)

    ## [1] 3

The cimg class
--------------

Imager uses the "cimg" class for its images. "cimg" is just a regular 4d
array with an S3 class tacked on so we can have custom plot, print, etc.
To promote an array to a "cimg" object, use as.cimg:

    noise <- array(runif(5*5*5*3),c(5,5,5,3)) #5x5 pixels, 5 frames, 3 colours. All noise
    noise <- as.cimg(noise)

You can treat the object as you would any other array:

    #Arithmetic
    sin(noise) + 3*noise 

    ## [1] "Image. Width: 5 pix Height 5 pix Depth 5 Colour channels 3\n"

    #Subsetting
    noise[,,,1] #First colour channel

    ## [1] "Image. Width: 5 pix Height 5 pix Depth 5 Colour channels 1\n"

    dim(noise[1:4,,,] )

    ## [1] 4 5 5 3

and you can convert it to a data.frame:

    head(as.data.frame(parrots))

    ##   x y z cc value
    ## 1 1 1 1  1   116
    ## 2 2 1 1  1   117
    ## 3 3 1 1  1   120
    ## 4 4 1 1  1   119
    ## 5 5 1 1  1   120
    ## 6 6 1 1  1   120

which makes life easier if you want to use ggplot2 for plotting.

### Displaying images and videos

To get a standard R plot use the plot function:

    plot(parrots)

![](imager_files/figure-markdown_strict/plotting-1.png)

    plot(tennis,frame=1) 

![](imager_files/figure-markdown_strict/plotting-2.png)

In addition imager provides display() (for images) and play() (for
videos), which are much faster C++ functions for quickly viewing your
results.

    imsplit(tennis,"z") %>% l_ply(plot)

<video   controls loop>
<source src="imager_files/figure-markdown_strict/animate-.webm" />
video of chunk animate</video>

Loading and saving
------------------

Use load.image and save.image. You'll most likely need imagemagick on
your path somewhere for images, and ffmpeg for videos. CImg supports
very few formats natively.

Splitting and concatenating images
----------------------------------

One often needs to perform separate computations on each channel of an
image, or on each frame, each line, etc. This can be achieved using a
loop or more conveniently using imsplit:

    imsplit(parrots,"c") #A list with three elements corresponding to the three channels

    ## [[1]]
    ## [1] "Image. Width: 768 pix Height 512 pix Depth 1 Colour channels 1\n"
    ## 
    ## [[2]]
    ## [1] "Image. Width: 768 pix Height 512 pix Depth 1 Colour channels 1\n"
    ## 
    ## [[3]]
    ## [1] "Image. Width: 768 pix Height 512 pix Depth 1 Colour channels 1\n"

    imsplit(parrots,"c") %>% laply(mean) #Mean pixel value in each channel

    ## [1] 121.6621 109.5960  75.7940

    imsplit(parrots,"x") %>% laply(mean) %>% head #Mean pixel value in each line (across all channels)

    ## [1] 93.50716 93.90885 94.28646 94.49089 94.55208 94.56445

The inverse operation is called imappend: it takes a list of images and
concatenates them along the dimension of your choice.

    #Sample functions and turn them into separate R,G,B channels
    R <- as.cimg(function(x,y) sin(cos(3*x*y)),100,100)
    G <- as.cimg(function(x,y) sin(cos(3*x*y + pi/2)),100,100)
    B <- as.cimg(function(x,y) exp(-.03*x),100,100)
    trippy <- imappend(list(R,G,B),"c") #Bind the three channels into one image
    plot(trippy)

![](imager_files/figure-markdown_strict/imappend-1.png)
