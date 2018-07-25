#include <R.h>
#include <Rcpp.h>
#include <Rinternals.h>

// This is needed to read data from the ffmpeg pipe
extern "C"
{
    // Connections.h is a C header and uses some C++ reserved keywords
#define private prriii
#define class classs
#include <R_ext/Connections.h>
#undef private
#undef class
}

// This function reads from the video pipe chunks of block_size bytes and inputs them at the right spot in the cimg array.
// [[Rcpp::export]]
SEXP read_video(SEXP vpipe,
		SEXP cimg_array,
		SEXP nframes,
		SEXP width,
		SEXP height,
		SEXP block_size)
{
    Rconnection con = R_GetConnection(vpipe);
    int nf = INTEGER(nframes)[0];
    int wi = INTEGER(width)[0];
    int he = INTEGER(height)[0];
    size_t bsize = INTEGER(block_size)[0];
    size_t total_size = nf * wi * he * 3;
    if(bsize > total_size)
	bsize = total_size;
    unsigned char* buf = R_Calloc(total_size, unsigned char);
    size_t remaining_size = total_size;
    size_t nbread;
    R_xlen_t i = 0;
    R_xlen_t j = 0;
    R_xlen_t t = 0;
    R_xlen_t rgb = 0;
    R_xlen_t cimg_index = 0;
    while(remaining_size)
    {
	if(bsize < remaining_size)
	    bsize = remaining_size;
	nbread = con->read(buf, sizeof(unsigned char), bsize, con);
	if(nbread != bsize)
	{
	    Rcpp::Rcout << "Read " << nbread << " bytes when expecting: " << bsize << " bytes\n";
	    Rf_error("Unexpected number of bytes read");
	}
	
	for(size_t k = 0; k < bsize; k++)
	{
	    cimg_index = i + (j + (rgb * nf + t) * he) * wi;
	    INTEGER(cimg_array)[cimg_index] = buf[k];
	    rgb ++;
	    if(rgb == 3)
	    {
		rgb = 0;
		i ++;
		if(i == wi)
		{
		    i = 0;
		    j ++;
		    if(j == he)
		    {
			j = 0;
			t ++;
		    }
		}
	    }
	}
	remaining_size -= bsize;
    }
    
    R_Free(buf);
    return(cimg_array);
}
