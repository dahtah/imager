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
##' @seealso save.video, make.video
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
    
    arg <- sprintf("-i %s %s -ss %s ",fname,extra.args,as.character(skip.to)) %>% paste0(dd,"/image-%d.bmp")
    tryCatch({
    dir.create(dd)
    system2("ffmpeg",arg,stdout=verbose,stderr=verbose)
    fls <- dir(dd,full.names=TRUE)
    if (length(fls)==0) stop("No output was generated")
    ordr <- stringr::str_extract(fls,"(\\d)+\\.bmp") %>% stringr::str_split(stringr::fixed(".")) %>% purrr::map_int(~ as.integer(.[[1]])) %>% order
    fls <- fls[ordr]
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
##' PNG, JPEG and BMP are supported via the readbitmap package. You'll need to install ImageMagick for other formats. If the path is actually a URL, it should start with http(s) or ftp(s). 
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
    fext <- fileext(file)
    if (fext %in% c('png','bmp','jpeg','jpg'))
    {
        bmp <- read.bitmap(file)
        ##bmp <- try(read.bitmap(file),silent=TRUE)
        ## if (class(bmp) != "try-error") #Loaded succesfully
        ## {
        if (!is.null(attr(bmp,"header"))) #We have a BMP file, rescale
        {
            bmp <- bmp/255
        }
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
            is.url <- grepl("^(http|ftp)s?://", file)
            if (is.url)
            {
                load_image(file)/255
            }
            else
            {
                file <- normalizePath(file,mustWork=TRUE)
                load_image(file)/255
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
        ext <- stringr::str_extract_all(url,"\\.([A-Za-z0-9]+$)")[[1]]
        if (length(ext) > 0) file <- tempfile(fileext=ext) else file <- tempfile()
        downloader::download(url,file,mode="wb")
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
    test.magick <- c('conjure','montage','magick') %>% Sys.which %>% map_lgl(function(v) nchar(v) > 0) 
    any(test.magick)
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
##' You'll need ImageMagick for formats other than PNG and JPEG. 
##'
##' @param im an image (of class cimg)
##' @param file path to file. The format is determined by the file's name
##' @param quality (JPEG only) default 0.7. Higher quality means less compression. 
##' @return nothing
##' @seealso save.video
##' @export
##' @examples
##' #Create temporary file
##' tmpF <- tempfile(fileext=".png")
##' #Save boats image
##' save.image(boats,tmpF)
##' #Read back and display
##' load.image(tmpF) %>% plot
save.image <- function(im,file,quality=.7)
{
    if (!is.cimg(im)) {
        stop("First argument should be an image")
    }
    if (depth(im) > 1) warning("using save.image for videos is deprecated, please switch to save.video")
    ftype <- stringr::str_match(file,"\\.([^.]+)$")[1,2]
    if (is.na(ftype))
    {
        stop("Please provide a file extension (.png, .jpeg, etc.)")
    }
    if (ftype == "png")
    {
        save.png(im,file)
    }
    else if (ftype == "jpeg" | ftype == "jpg")
    {
            save.jpeg(im,file,quality=quality)
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

save.jpeg <- function(im,file,quality=.7)
    {
        convert.im.toPNG(im) %>% jpeg::writeJPEG(file,quality=quality)
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




##' Load example image
##'
##' Imager ships with five test pictures and a video. Two (parrots and boats) come from the [Kodak set](http://r0k.us/graphics/kodak/). Another (birds) is a sketch of birds by Leonardo, from Wikimedia. The "coins" image comes from scikit-image. The Hubble Deep field (hubble) is from Wikimedia.
##' The test video ("tennis") comes from [xiph.org](https://media.xiph.org/video/derf/)'s collection.
##' @param name name of the example
##' @return an image
##' @author Simon Barthelme
##' @examples
##' load.example("hubble") %>% plot
##' load.example("birds") %>% plot
##' load.example("parrots") %>% plot
##' @export
load.example <- function(name)
{
    fnames <- list(parrots="parrots.png",hubble="HubbleDeepField.jpg",
                   tennis="tennis_sif.mp4",birds="Leonardo_Birds.jpg",coins="coins.png")
    if (name %in% names(fnames))
    {
        fp <- paste0('extdata/',fnames[name]) %>% system.file(package='imager')
        if (name=="tennis")
        {
            load.video(fp)
        }
        else
        {
            load.image(fp)
        }
    }
    else
    {
        msg <- 'Unknown example picture. Available: %s'
        msg <- sprintf(msg,paste(names(fnames),collapse=","))
        stop(msg)
    }
}

##' Make/save a video using ffmpeg
##'
##' You need to have ffmpeg on your path for this to work. This function uses ffmpeg to combine individual frames into a video.
##' save.video can be called directly with an image or image list as input. 
##' make.video takes as argument a directory that contains a sequence of images representing individual frames to be combined into a video.
##' @param im an image or image list
##' @param fname name of the output file. The format is determined automatically from the name (example "a.mpeg" will have MPEG format)
##' @param dname name of a directory containing individual files
##' @param pattern pattern of filename for frames (the default matches "image-1.png", "image-2.png", etc.. See ffmpeg documentation for more).
##' @param fps frames per second (default 25)
##' @param extra.args extra arguments to be passed to ffmpeg (default "", none)
##' @param verbose if TRUE, show ffmpeg output (default FALSE)
##' @param ... extra arguments to save.video, passed on to make.video
##' @seealso load.video
##' @examples
##' ## Not run
##' ## iml <- map_il(seq(0,20,l=60),~ isoblur(boats,.))
##' ## f <- tempfile(fileext=".avi")
##' ## save.video(iml,f)
##' ## load.video(f) %>% play
##' ## #Making a video from a directory
##' ## dd <- tempdir()
##' ## for (i in 1:length(iml)) {
##' ## png(sprintf("%s/image-%i.png",dd,i));
##' ## plot(iml[[i]]); dev.off() }
##' ## make.video(dd,f)
##' ## load.video(f) %>% play
##' @author Simon Barthelme
##' @export
make.video <- function(dname,fname,pattern="image-%d.png",fps=25,extra.args="",verbose=FALSE)
{
    arg <- if (!is.null(fps)) sprintf("%s -framerate %.4d ",extra.args,fps) else ""
    arg <- sprintf("%s -i %s/%s",arg,dname,pattern)
    arg <- sprintf("%s %s -y ",arg,extra.args) %>% paste0(fname)
    system2("ffmpeg",arg,stdout=verbose,stderr=verbose)
}

#' @describeIn make.video Save a video using ffmpeg
#' @export
save.video <- function(im,fname,...)
{
    if (!has.ffmpeg()) stop("Can't find ffmpeg. Please install.")
    dd <- paste0(tempdir(),"/vid")
    tryCatch({
        dir.create(dd)
        im <- if (is.imlist(im)) im else imsplit(im,"z") 
        nms <- sprintf("%s/image-%i.png",dd,seq_along(im))
        purrr::map2(im,nms,~ save.png(.x,.y))
        make.video(dname=dd,fname=fname,...)
    }, finally=unlink(dd,recursive=TRUE))
    
}

#borrowed from pkgmaker::file_extension
fileext <- function(f)
{
    sub(".*\\.([^.]{3,4})$", "\\1", f) %>% tolower
}

                    
##' Load all images in a directory
##'
##' Load all images in a directory and return them as an image list. 
##' @param path directory to load from
##' @param pattern optional: file pattern (ex. *jpg). Default NULL, in which case we look for file extensions png,jpeg,jpg,tif,bmp. 
##' @param quiet if TRUE, loading errors are quiet. If FALSE, they are displayed. Default FALSE
##' @return an image list
##' @author Simon Barthelme
##' @examples
##' path <- system.file(package="imager") %>% paste0("/extdata")
##' load.dir(path)
##' @export
load.dir <- function(path,pattern=NULL,quiet=FALSE)
{
    fn <- dir(path,full.names=TRUE,pattern=pattern)
    nms <- dir(path,full.names=FALSE,pattern=pattern) #Lazy!!!
    if (is.null(pattern))
    {
        is.img <- fn %>% fileext %>% { . %in% c("png","jpg","jpeg","bmp","tif") }
        fn <- fn[is.img]
        nms <- nms[is.img]
    }
    li <- purrr::safely(load.image,quiet=quiet)
    map(fn,li) %>% setNames(nms) %>%
        keep(~ is.null(.$error)) %>% map_il("result")
}
