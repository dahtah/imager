#!/usr/bin/Rscript

#A script to quickly compile a single file from the package, rather than the whole thing
#only supposed to work on my machine
args <- ' -I/usr/share/R/include -DNDEBUG -Dcimg_use_openmp -fopenmp  -Dcimg_use_fftw3   -I../inst/include -DCIMG_COMPILING -Dcimg_use_rng -Dcimg_r_mode -Dcimg_use_fftw3_singlethread -Dcimg_verbosity=1   -I"~/R/x86_64-pc-linux-gnu-library/3.3/Rcpp/include" -I"~/R/x86_64-pc-linux-gnu-library/3.2/Rcpp/include"   -fpic  -O2 -fstack-protector --param=ssp-buffer-size=4 -Wformat -Werror=format-security -D_FORTIFY_SOURCE=2  -c %s.cpp -o %s.o'
fname <- commandArgs(TRUE)[1]
args <- sprintf(args,fname,fname)
system2('g++',args)
