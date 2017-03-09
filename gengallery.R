##Generate images for the gallery

canny <- function()
{
    im <- load.example('parrots')  %>% imsub(x %inr% c(11,323),y%inr% c(155,425))
    nim <- (im+boundary(cannyEdges(im,alpha=.4)))
    nim <- nim/max(nim)
    imager::save.image(nim,"gallery/canny.jpg")
}

other <- function()
{
    f <- system.file("external/test.grd", package="raster")
    png("gallery/other.png")
    raster(f) %>% as.cimg %>% plot
    dev.off()
}

morpho <- function()
{
    birds <- load.example("birds") %>% imsub(y <= 350)
    im <- grayscale(birds)
    i1 <- !(im %>% threshold )
    d <- as.data.frame(im)

    m <- sample_n(d,1e4) %>% lm(value ~ x*y,data=.) 

    im.c <- im-predict(m,d)
    i2 <- !(im.c %>% threshold)
    i3 <- clean(i2,5) 
    imlist(i1,i2,i3) %>% imappend("z") %>% save.video("gallery/morpho.gif",fps=1)
}


imstats <- function()
{
    set.seed(1)
    gr <- pixel.grid(im) %>% sample_n(1e4) %>% filter(x %inr% c(5,width(im)-5),y %inr% c(5,height(im)-5))
    out <- extract_patches(im,gr$x,gr$y,5,5) %>% map(~ as.vector(.-mean(.))) %>% { do.call(rbind,.) }
    eg <- eigen(cov(out))$vec
    png("gallery/imstats.png")
    map_il(1:6,~ as.cimg(eg[,.])) %>% plot(interp=F,axes=F)
    dev.off()
}

parallel <- function()
{
    imrep(boats,3) %>% imappend("x") %>% imrep(2) %>% imappend("y") %>% imager::save.image("gallery/parallel.jpg")
}

unshuffle <- function()
{
    im <- load.image("https://camo.githubusercontent.com/7d413e9a343e8ceaa507d68a1a6e93247d5f7853/68747470733a2f2f726f62696e686f7573746f6e2e6769746875622e696f2f696d6167652d756e736872656464696e672f696d616765732f6f726967696e616c2f626c75652d686f75722d70617269732e706e67")

    ##reorder the columns of an image
    scramble <- function(im)
    {
        imsplit(im,"x") %>% { .[sample(length(.))] } %>% imappend("x") 
    }
    
    imlist(scramble(im),im) %>% imappend("z") %>% save.video("gallery/unshuffle.gif",fps=1)
}

foreground <- function()
{
    fknn <- function(X,Xp,cl,k=1)
    {
        out <- nabor::knn(X,Xp,k=k)
        cl[as.vector(out$nn.idx)] %>% matrix(dim(out$nn.idx)) %>% rowMeans
    }
    im <- load.image("https://upload.wikimedia.org/wikipedia/commons/thumb/f/fd/Aster_Tataricus.JPG/1024px-Aster_Tataricus.JPG")
#Coordinates of a foreground rectangle (x0,y0,x1,y1)
fg <- c(510,521,670,671 )
#Background
bg <- c(791,   28, 1020,  194 )
#Corresponding pixel sets
px.fg <- ((Xc(im) %inr% fg[c(1,3)]) & (Yc(im) %inr% fg[c(2,4)]))
px.bg <- ((Xc(im) %inr% bg[c(1,3)]) & (Yc(im) %inr% bg[c(2,4)]))
im.lab <- sRGBtoLab(im)
#Reshape image data into matrix with 3 columns 
cvt.mat <- function(px) matrix(im.lab[px],sum(px)/3,3)
fgMat <- cvt.mat(px.fg)
bgMat <- cvt.mat(px.bg)
labels <- c(rep(1,nrow(fgMat)),rep(0,nrow(bgMat)))
    testMat <- cvt.mat(px.all(im))
    out <- fknn(rbind(fgMat,bgMat),testMat,cl=labels,k=5)
msk <- as.cimg(rep(out,3),dim=dim(im))
    
  
    imlist(im,im*(msk==1)) %>% imappend("z") %>% imresize(.5) %>% save.video("gallery/fgbg.gif",fps=1)
}
