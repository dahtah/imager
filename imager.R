## ----init,echo=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE, cache=FALSE, 
               comment=NA, verbose=TRUE,dev='jpeg')

## ----quickstart,message=FALSE,cache=TRUE---------------------------------
library(imager)


file <- system.file('extdata/parrots.png',package='imager')
#system.file gives the full path for a file that ships with a R package
#if you already have the full path to the file you want to load just run:
#im <- load.image("/somedirectory/myfile.png")
im <- load.image(file)

plot(im) #Parrots!
im.blurry <- isoblur(im,10) #Blurry parrots!
plot(im.blurry)
im.xedges <- deriche(im,2,order=2,axis="x") #Edge detector along x-axis
plot(im.xedges)
im.yedges <- deriche(im,2,order=2,axis="y") #Edge detector along y-axis
plot(im.yedges)
#Chain operations using the pipe operator (from magrittr)
deriche(im,2,order=2,axis="x") %>% deriche(2,order=2,axis="y") %>% plot

## ----quickstart2,fig.width=12,cache=TRUE---------------------------------
#Another example of chaining: image gradient along x and y axes
layout(matrix(1:2,1,2));
grayscale(im) %>% get_gradient(axes="xy") %>% l_ply(plot)

#If ffmpeg is present, we can load videos as well:
tennis <- load.video(system.file('extdata/tennis_sif.mpeg',package='imager'))

plot(tennis,frame=1)
plot(tennis,frame=5)

## ----motion_example,cache=TRUE-------------------------------------------
tennis.g <- grayscale(tennis)
motion <- deriche(tennis.g,1,order=1,axis="z")^2 #Differentiate along z axis and square
combined <- list(motion/max(motion),tennis.g/max(tennis.g)) %>% imappend("x") #Paste the two videos together

## ----animate,echo=FALSE,fig.show="animate",interval=.03,fig.width=12,cache=TRUE----
imsplit(combined,"z") %>% l_ply(plot)

## ----load_save-----------------------------------------------------------
file <- system.file('extdata/parrots.png',package='imager')
parrots <- load.image(file)
#The file format is defined by the extension. Here we save as JPEG
imager::save.image(parrots,"/tmp/parrots.jpeg")
#We call imager::save.image to avoid ambiguity, as base R already has a save.image function 

## ----loading_parrots-----------------------------------------------------
library(rvest)
#Run a search query (returning html content)
search <- read_html("https://www.google.com/search?site=&tbm=isch&q=parrot")

#Grab all <img> tags, get their "src" attribute, a URL to an image
urls <- search %>% html_nodes("img") %>% html_attr("src") #Get urls of parrot pictures

#Load the first one
load.image(urls[1]) %>% plot()

## ------------------------------------------------------------------------
fname <- system.file('extdata/tennis_sif.mpeg',package='imager')
tennis <- load.video(fname)

## ------------------------------------------------------------------------
#Skip to time = 2s, and grab the next 3 frames
vid <- load.video(fname,skip.to=2,frames=4)
#Display individual frames (see imsplit below)
imsplit(vid,"z") %>% plot

## ------------------------------------------------------------------------
vid <- load.video(fname,fps=1) #1 frame per sec
#The result is 6 frames, one per sec
imsplit(vid,"z") %>% plot

## ------------------------------------------------------------------------
f <- tempfile(fileext=".avi")
save.video(vid,f)
load.video(f) %>% imsplit("z") %>% plot
unlink(f)

## ------------------------------------------------------------------------

dd <- tempdir()
iml <- map_il(seq(0,20,l=60),~ isoblur(boats,.))
#Generate file names
fnames <- sprintf("%s/image-%i.png",dd,1:length(iml))
#Save each frame in a different file
invisible(purrr::map2(iml,fnames,function(im,f) imager::save.image(im,f)))
f <- tempfile(fileext=".avi")

make.video(dd,f)


## ----plotting------------------------------------------------------------
plot(parrots)
plot(tennis,frame=1) 

## ----tmp_file,cache=TRUE-------------------------------------------------
tmp <- tempfile(fileext=".png") #Open temp. file
imager::save.image(boats,tmp) #Save image to temp. file
#Call "eog [temp file]" to open in external viewer (Linux only)
system2("eog",tmp)
#On a mac: try system2("open",tmp)
unlink(tmp) #Delete temp. file

## ----dim_gray,cache=TRUE-------------------------------------------------
parrots <- load.example('parrots')
gray.parrots <- grayscale(parrots)
dim(gray.parrots)

## ----dim_colour,cache=TRUE-----------------------------------------------
dim(parrots)

## ----dim_video,cache=TRUE------------------------------------------------
dim(tennis)

## ----dimensions,cache=TRUE-----------------------------------------------
width(parrots)
height(parrots)
depth(parrots)
spectrum(parrots)

## ----as.cimg.array,cache=TRUE--------------------------------------------
noise <- array(runif(5*5*5*3),c(5,5,5,3)) #5x5 pixels, 5 frames, 3 colours. All noise
noise <- as.cimg(noise)

## ----math_array,cache=TRUE-----------------------------------------------
#Arithmetic
sin(noise) + 3*noise 

#Subsetting
noise[,,,1] #First colour channel
dim(noise[1:4,,,] )


## ------------------------------------------------------------------------
matrix(1,10,10) %>% as.cimg

## ------------------------------------------------------------------------
1:9 %>% as.cimg

## ----as.cimg.data.frame--------------------------------------------------
df <- expand.grid(x=1:10,y=1:10,z=1,cc=1)
mutate(df,value=cos(sin(x+y)^2)) %>% as.cimg

## ----as.cimg.data.frame2-------------------------------------------------
mutate(df,value=cos(sin(x+y)^2)) %>% as.cimg(dims=c(10,10,1,1))

## ----as.df,cache=TRUE----------------------------------------------------
head(as.data.frame(parrots))

## ------------------------------------------------------------------------
head(as.array(parrots))
head(as.vector(parrots))
grayscale(parrots) %>% as.matrix %>% dim

## ------------------------------------------------------------------------
imgradient(parrots,"xy") %>% class

## ------------------------------------------------------------------------
imgradient(parrots,"xy") %>% plot

## ------------------------------------------------------------------------
imgradient(parrots,"xy") %>% as.data.frame %>% head

## ------------------------------------------------------------------------
imlist(a=parrots,b=3*parrots)
list(a=parrots,b=3*parrots) %>% as.imlist

## ----imsplit-------------------------------------------------------------
imsplit(parrots,"c") #A list with three elements corresponding to the three channels
imsplit(parrots,"c") %>% laply(mean) #Mean pixel value in each channel
imsplit(parrots,"x") %>% laply(mean) %>% head #Mean pixel value in each line (across all channels)

## ------------------------------------------------------------------------
imsplit(parrots,"x",4) %>% plot
imsplit(parrots,"y",3) %>% plot

## ------------------------------------------------------------------------
imsplit(parrots,"x",-250) %>% plot

## ----imappend------------------------------------------------------------
#Sample functions and turn them into separate R,G,B channels
R <- as.cimg(function(x,y) sin(cos(3*x*y)),100,100)
G <- as.cimg(function(x,y) sin(cos(3*x*y + pi/2)),100,100)
B <- as.cimg(function(x,y) exp(-.03*x),100,100)
trippy <- imappend(list(R,G,B),"c") #Bind the three channels into one image
plot(trippy)

## ----iiply---------------------------------------------------------------
iiply(parrots,"c",function(v) v/max(v))  %>% plot

#Same thing but longer:
#imsplit(parrots,"c") %>% llply(function(v) v/max(v) %>% imappend("c") %>% plot

## ----add_average_etc,cache=TRUE------------------------------------------
imsplit(parrots,"c") %>% add %>% plot(main="Adding colour channels")

#Use different levels of blur on the same image
blur.layers <- llply(seq(1,15,l=5),function(v) isoblur(parrots,v))

blur.layers %>% parmin %>% plot(main="Min across blur levels")
blur.layers %>% parmax %>% plot(main="Max across blur levels")
blur.layers %>% average %>% plot(main="Average across blur levels")

## ----imsub---------------------------------------------------------------
imsub(parrots,x < 30) #Only the first 30 rows
imsub(parrots,y < 30) #Only the first 30 rows
imsub(parrots,x < 30,y < 30) #First 30 columns and rows
imsub(parrots, sqrt(x) > 8) #Can use arbitrary expressions
imsub(parrots,x > height/2,y > width/2)  #height and width are defined based on the image
imsub(parrots,cc==1) #Colour axis is "cc" not "c" here because "c" is an important R function

## ------------------------------------------------------------------------
R(parrots) 
G(parrots)
B(parrots)
#R(parrots) is equivalent to channel(parrots,1)

#Set all channels to 0 except red
parrots.cp <- load.example("parrots")
G(parrots.cp) <- 0
B(parrots.cp) <- 0
plot(parrots.cp)

## ------------------------------------------------------------------------
frame(tennis,1)
#Blur frame 1
frame(tennis,1) <- isoblur(frame(tennis,1),10)

## ------------------------------------------------------------------------
imrow(R(parrots),10) %>% plot(main="Red channel along 10th row",type="l")
imcol(B(parrots),10) %>% plot(main="Blue channel along 10th line",type="l")

## ------------------------------------------------------------------------
at(parrots,x=20,y=20,cc=1:3)
color.at(parrots,x=20,y=20)

## ------------------------------------------------------------------------
im <- imfill(4,4)
dim(im) #4 dimensional, but the last two dimensions are singletons
im[,1,,] <- 1:4 #Assignment the standard way
im[,1] <- 1:4 #Shortcut

## ----denoise_blur--------------------------------------------------------
birds <- load.image(system.file('extdata/Leonardo_Birds.jpg',package='imager'))
birds.noisy <- (birds + 80*rnorm(prod(dim(birds)))) 
layout(t(1:2))
plot(birds.noisy,main="Original")
isoblur(birds.noisy,5) %>% plot(main="Blurred")

## ----denoise_aniso-------------------------------------------------------
layout(t(1:2))
plot(birds.noisy,main="Original")
blur_anisotropic(birds.noisy,ampl=1e5,sharp=1) %>% plot(main="Blurred (anisotropic)")

## ----hsl,im,fig.width=18-------------------------------------------------
parrots.hsl <- RGBtoHSL(parrots)
chan <- channels(parrots.hsl) #Extract the channels as a list of images
names(chan) <- c("H","S","L")
#Plot
layout(matrix(1:3,1,3))
l_ply(names(chan),function(nm) plot(chan[[nm]],main=nm))

## ----trippy_is_back------------------------------------------------------
YUVtoRGB(trippy) %>% plot

## ----add.colour----------------------------------------------------------
grayscale(parrots) %>% spectrum
#Image has only one channel (luminance)

grayscale(parrots) %>% add.colour %>% spectrum
#Image is still in gray tones but has R,G,B channels 

## ----resize_rotate-------------------------------------------------------
thmb <- resize(parrots,round(width(parrots)/10),round(height(parrots)/10))
plot(thmb,main="Thumbnail") #Pixellated parrots

#Same as above: negative arguments are interpreted as percentages
thmb <- resize(parrots,-10,-10)

imrotate(parrots,30) %>% plot(main="Rotating")
imshift(parrots,40,20) %>% plot(main="Shifting")
imshift(parrots,100,100,boundary=1) %>% plot(main="Shifting (Neumann boundaries)")
imshift(parrots,100,100,boundary=2) %>% plot(main="Shifting (circular)")

## ----pad-----------------------------------------------------------------
pad(parrots,axes="y",140) %>% plot
pad(parrots,axes="y",140,pos=-1) %>% plot

## ----autocrop------------------------------------------------------------
#The argument to autocrop is the colour of the background it needs to remove
pad(parrots,axes="y",140,pos=-1) %>% autocrop(c(0,0,0)) %>% plot


## ----warp_shift----------------------------------------------------------
map.shift <- function(x,y) list(x=x+10,y=y+30)
imwarp(parrots,map=map.shift) %>% plot

## ----warp_scale_forward,cache=TRUE---------------------------------------
map.scale <- function(x,y) list(x=1.5*x,y=1.5*y)
imwarp(parrots,map=map.scale) %>% plot(main="Forward mode")

## ----warp_scale_backward,cache=TRUE--------------------------------------
map.scale.bw <- function(x,y) list(x=x/1.5,y=y/1.5)
imwarp(parrots,map=map.scale.bw,direction="backward") %>% plot(main="Backward mode")

## ----warped_warp,cache=TRUE----------------------------------------------
map <- function(x,y) list(x=exp(y/600)*x,y=y*exp(-sin(x/40)))
imwarp(parrots,map=map,direction="forward") %>% plot()


## ----lag,cache=TRUE------------------------------------------------------
#Compute difference between two successive frames (at lag 1)
(imshift(tennis,delta_z=1)-tennis) %>% plot(frame=2,main="Difference betw. frames 2 and 1")

#Compute difference between frames (at lag 3)
(imshift(tennis,delta_z=3)-tennis) %>% plot(frame=4,main="Difference between frames 3 and 1")

#note that shift uses interpolation. that makes it relatively slow, but one advantage is that it allows non-integer lags:
#shift(tennis,delta_z=3.5)-tennis
#is valid


## ------------------------------------------------------------------------
flt <- as.cimg(matrix(1,4,4)) #4x4 box filter
grayscale(parrots) %>% correlate(flt) %>% plot(main="Filtering with box filter")

#Here the filter is symmetrical so convolution and correlation should be the same. Check:
a <- grayscale(parrots) %>% correlate(flt)
b <- grayscale(parrots) %>% imager::convolve(flt)
all.equal(a,b)

## ----fig.width=12--------------------------------------------------------
im <- grayscale(parrots)
layout(t(1:2))
deriche(im,sigma=4,order=2,axis="y") %>% plot(main="2nd deriv of Gaussian along y")
vanvliet(im,sigma=4,order=2,axis="y") %>% plot(main="2nd deriv of Gaussian along y")

## ------------------------------------------------------------------------
n <- 1e3
xv <- seq(0,1,l=n) #1D Grid
imp <- imdirac(c(n,1,1,1),n/2,1) #1D signal: Impulse at x = n/2
sig <- 80
#impulse response of the Deriche filter
imp.dr <- deriche(imp,sigma=sig) %>% as.vector
#impulse response of the Vanvliet-Young filter
imp.vv <- vanvliet(imp,sigma=sig) %>% as.vector 
imp.true <- dnorm(xv,sd=sig/n,m=.5) #True impulse response
plot(xv,imp.true/max(imp.true),type="l",lwd=2,xlab="x",ylab="Impulse response")
lines(xv,imp.vv/max(imp.vv),col="blue",lwd=2)
lines(xv,imp.dr/max(imp.dr),col="red",lwd=2)

## ------------------------------------------------------------------------
im <- imfill(3e3,3e3)
system.time(deriche(im,3))
system.time(vanvliet(im,3))

## ----FFT,cache=TRUE------------------------------------------------------
im <- as.cimg(function(x,y) sin(x/5)+cos(x/4)*sin(y/2),128,128)
ff <- FFT(im)
plot(ff$real,main="Real part of the transform")
plot(ff$imag,main="Imaginary part of the transform")
sqrt(ff$real^2+ff$imag^2) %>% plot(main="Power spectrum")

## ----FFT_nativeR,cache=TRUE----------------------------------------------
rff <- as.matrix(im) %>% fft
Re(rff) %>% as.cimg %>% plot(main="Real part of the transform (R's native code)")

## ----periodicsmooth,fig.width=12,cache=TRUE------------------------------
layout(t(1:2))
periodic.part(parrots) %>% plot(main="Periodic part of parrots")
(parrots- periodic.part(parrots)) %>% plot(main="Smooth residual of parrots")

