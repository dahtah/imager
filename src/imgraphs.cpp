#include <imager.h>

using namespace Rcpp;
using namespace cimg_library;

// Return adjacency info for 2D pixset
// [[Rcpp::export]]
List bgraph(LogicalVector px)
{
  int nPix = px.length();
  CIb img=as<CIb >(px);
  int w=img.width(),h=img.height();
  std::vector<int> ii;
  std::vector<int> jj;
  std::vector<double> dst;
  
  ii.reserve(nPix);
  jj.reserve(nPix);
  dst.reserve(nPix);
  const int dx[] = {1,-1,0,1}, dy[] = {0,1,1,1},n=4;
  int ind1,ind2;
  const double s2 =sqrt(2.0);
  cimg_forXY(img,x,y)
    {
      if (img(x,y))
	{
	  ind1 = x+y*w;
	  for (int i = 0; i < n; i++)
	    {
	      if (x+dx[i]  >= 0  and x + dx[i] < w and y+dy[i]  >= 0  and y + dy[i] < h)
		{
		  ind2 = (x+dx[i])+(y+dy[i])*w;
		  if (img(ind1) and (img(ind2)))
		    {
		      ii.push_back(ind1+1);
		      jj.push_back(ind2+1);
		      dst.push_back((dx[i] != dy[i]) ? 1 : s2);
		    }
		}
	    }
	}
    }
  return List::create(_["ii"] = wrap(ii),_["jj"] =wrap(jj),_["dst"] =wrap(dst));
}
