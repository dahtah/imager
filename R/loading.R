#Loading and saving

##' Load a video using ffmpeg
##'
##' You need to have ffmpeg on your path for this to work. This function uses ffmpeg to split the video into individual frames, which are then loaded as images and recombined.
##' Videos are memory-intensive, and load.video performs a safety check before loading a video that would be larger than maxSize in memory (default 1GB)
##' @param fname file to load
##' @param maxSize max. allowed size in memory, in GB (default max 1GB). 
##' @param skip.to skip to a certain point in time (in sec., or "hh:mm::ss" format)
##' @param frames number of frames to load (default NULL, all)
##' @param fps frames per second (default NULL, determined automatically)
##' @param extra.args extra arguments to be passed to ffmpeg (default "", none)
##' @param verbose if TRUE, show ffmpeg output (default FALSE)
##' @return an image with the extracted frames along the "z" coordinates
##' @examples
##' 
##' fname <- system.file('extdata/tennis_sif.mpeg',package='imager')
##' ##Not run
##' ## load.video(fname) %>% play
##' ## load.video(fname,fps=10) %>% play
##' ## load.video(fname,skip=2) %>% play
##' @author Simon Barthelme
##' @export
load.video <- function(fname,maxSize=1,skip.to=0,frames=NULL,fps=NULL,extra.args="",verbose=FALSE) wrap.url(fname,function(f) load.video.internal(f,skip.to=skip.to,maxSize=maxSize,frames=frames,fps=fps,extra.args=extra.args,verbose=verbose))

load.video.internal <- function(fname,maxSize=1,skip.to=0,frames=NULL,fps=NULL,extra.args="",verbose=FALSE)
{
    if (!has.ffmpeg()) stop("Can't find ffmpeg. Please install.")
    dd <- paste0(tempdir(),"/vid")
    if (!is.null(frames)) extra.args <- sprintf("%s -vframes %i ",extra.args,frames)
    if (!is.null(fps)) extra.args <- sprintf("%s -vf fps=%.4d ",extra.args,fps)
    
    arg <- sprintf("-i %s %s -ss %s ",fname,extra.args,as.character(skip.to)) %>% paste0(dd,"/image-%03d.bmp")
    tryCatch({
    dir.create(dd)
    system2("ffmpeg",arg,stdout=verbose,stderr=verbose)
    fls <- dir(dd,full.names=TRUE)
    if (length(fls)==0) stop("No output was generated")
    #Check total size
    imsz <- load.image(fls[[1]]) %>% dim %>% prod
    tsz <- ((imsz*8)*length(fls))/(1024^3)
    if (tsz > maxSize)
    {
        msg <- sprintf("Video exceeds maximum allowed size in memory (%.2d Gb out of %.2d Gb)",tsz,maxSize)
        unlink(dd,recursive=TRUE)
        stop(msg)
    }
    else
    {
        out <- map_il(fls,load.image) %>% imappend("z")
        out
        
    } },
    finally=unlink(dd,recursive=TRUE))
    
}

##' Load image from file or URL
##'
##'
##' PNG, JPEG and BMP are supported via the readbitmap package. You'll need to install ImageMagick for other formats. If the image is actually a video, you'll need ffmpeg. If the path is actually a URL, it should start with http(s) or ftp(s). 
##' 
##' @param file path to file or URL
##' @return an object of class 'cimg'
##' @examples
##' #Find path to example file from package
##' fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
##' im <- load.image(fpath)
##' plot(im)
##' #Load the R logo directly from the CRAN webpage
##' #load.image("https://cran.r-project.org/Rlogo.jpg") %>% plot
##' @export
load.image <- function(file) wrap.url(file,load.image.internal)

load.image.internal <- function(file)
{
    bmp <- try(read.bitmap(file),silent=TRUE)
    if (class(bmp) != "try-error") #Loaded succesfully
    {
        if (length(dim(bmp)) == 3) #Has colour
        {
            dim(bmp) <- c(dim(bmp)[1:2],1,dim(bmp)[3]) #Make array 4D
            }
        else 
        {
            dim(bmp) <- c(dim(bmp),1,1)
        }
        bmp <- cimg(bmp) %>% mirror("x") %>% imrotate(-90)
        bmp
    }
    else #Loading with read.bitmap has failed, try with ImageMagick
    {
        if (has.magick())
        {
            if (is.url)
            {
                load_image(file)
            }
            else
            {
                file <- normalizePath(file,mustWork=TRUE)
                load_image(file)
            }
        }
        else
        {
            stop("Unsupported file format. Please convert to jpeg/png/bmp or install image magick")
        }
    }
}

wrap.url <- function(file,fun)
{
    is.url <- grepl("^(http|ftp)s?://", file)
    if (is.url)
    {
        url <- file
        file <- tempfile()
        downloader::download(url,file)
        out <- fun(file)
        unlink(file)
        out
    }
    else
    {
        if (!file_test("-f",file))
        {
            stop("File not found")
        }
        else
        {
            fun(file)
        }
    }

}

has.ffmpeg <- function()
{
    Sys.which("ffmpeg") %>% { nchar(.) > 0 }
}

has.magick <- function()
{
    test.magick <- c('conjure','montage') %>% Sys.which %>% Filter(function(v) nchar(v) > 0,.) %>% length
    test.magick == 2
}

convert.im.fromPNG <- function(A)
    {
        A <- A*255
        d <- dim(A)
        if (length(d) == 3)
            {
                dim(A) <- c(d[1:2],1,d[3])
            }
        else
            {
                dim(A) <- c(d[1:2],1,1)
            }
        mirror(A,"x") %>% imrotate(-90)
    }

load.png <- function(file)
    {
        png::readPNG(file) %>% convert.im.fromPNG
    }

load.jpeg <- function(file)
    {
        jpeg::readJPEG(file) %>% convert.im.fromPNG
    }



##' Save image
##'
##' You'll need ImageMagick for formats other than PNG and JPEG. If the image is actually a video, you'll need ffmpeg.
##'
##' @param im an image (of class cimg)
##' @param file path to file. The format is determined by the file's name
##' @return nothing
##' @export
##' @examples
##' #Create temporary file
##' tmpF <- tempfile(fileext=".png")
##' #Save boats image
##' save.image(boats,tmpF)
##' #Read back and display
##' load.image(tmpF) %>% plot
save.image <- function(im,file)
    {
        ftype <- stringr::str_match(file,"\\.(.+)$")[1,2]
        if (ftype == "png")
        {
            save.png(im,file)
        }
        else if (ftype == "jpeg" | ftype == "jpg")
        {
            save.jpeg(im,file)
        }
        else
        {
            if (has.magick())
            {
                save_image(255*im,path.expand(file))
            }
            else
            {
                stop("Unsupported output file format. Use jpg/png or install ImageMagick")
            }
        }
    }

save.png <- function(im,file)
    {
        convert.im.toPNG(im) %>% png::writePNG(file) 
    }

save.jpeg <- function(im,file)
    {
        convert.im.toPNG(im) %>% jpeg::writeJPEG(file)
    }

convert.im.toPNG <- function(A)
    {
        if (any(A > 1) | any(A < 0))
            {
                A <-  rn(A)
            }
        A <- imrotate(A,90) %>% mirror("x") 
        dim(A) <- dim(A)[-3]
        A
    }






