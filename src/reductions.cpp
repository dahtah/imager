#include <imager.h>
#include "wrappers_cimglist.h"
using namespace Rcpp;
using namespace cimg_library;




// [[Rcpp::export]]
NumericVector reduce_wsum(List x,NumericVector w,bool na_rm=false)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),"xyzc",0.0);
  CIb valid(L.at(0),"xyzc",false);
  int n = x.size();
  for (int i = 0;  i < n; i++)
    {
      if (!na_rm)
	{
	  out += w(i)*L.at(i);
	}
      else
	{
	  cimg_forXYZC(out,x,y,z,c)
	    {
	      double v = L.at(i)(x,y,z,c);
	      if (!(std::isnan(v)))
		{
		  out(x,y,z,c) += w(i)*v;
		  valid(x,y,z,c) = true;
		}
	    }
	}
    }
    if (na_rm)
    {
      cimg_forXYZC(out,x,y,z,c){ if (!valid(x,y,z,c)) { out(x,y,z,c) = NA_REAL; } }
    }
  return wrap(out);
}


// [[Rcpp::export]]
NumericVector reduce_average(List x,bool na_rm=false)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),"xyzc",0.0);
  CImg<int>  nn(L.at(0),"xyzc",0);
  int n = x.size();
  for (int i = 0;  i < n; i++)
    {
      if (!na_rm)
	{
	  out += L.at(i);
	}
      else
	{
	  cimg_forXYZC(out,x,y,z,c)
	    {
	      double v = L.at(i)(x,y,z,c);
	      if (!(std::isnan(v)))
		{
		  out(x,y,z,c) += v;
		  nn(x,y,z,c)+=1;
		}
	    }
	}
    }
  out = na_rm?(out.div(nn)):(out/(double) (n));
  return wrap(out);
}



// [[Rcpp::export]]
NumericVector reduce_prod(List x,bool na_rm=false)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),"xyzc",1.0);
  CIb valid(L.at(0),"xyzc",false);
  int n = x.size();
  for (int i = 0;  i < n; i++)
    {
      if (!na_rm)
	{
	  out.mul(L.at(i));
	}
      else
	{
	  cimg_forXYZC(out,x,y,z,c)
	    {
	      double v = L.at(i)(x,y,z,c);
	      if (!(std::isnan(v)))
		{
		  out(x,y,z,c) *= v;
		  valid(x,y,z,c) = true;
		}
	    }
	}
    }
    if (na_rm)
    {
      cimg_forXYZC(out,x,y,z,c){ if (!valid(x,y,z,c)) { out(x,y,z,c) = NA_REAL; } }
    }
  return wrap(out);
}

// [[Rcpp::export]]
NumericVector reduce_minmax(List x,bool na_rm=false,bool max=true)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),"xyzc", (max?(-DBL_MAX):DBL_MAX));
  CIb valid(L.at(0),"xyzc",false);
  int n = x.size();
  double v;
  for (int i = 0;  i < n; i++)
    {
      cimg_forXYZC(out,x,y,z,c)
	{
	  v = L.at(i)(x,y,z,c);
	  if (std::isnan(v))
	    {
	      if (!na_rm)
		{
		  out(x,y,z,c) = v;
		}
	    }
	  else
	    {
	      if (na_rm)
		{
		  valid(x,y,z,c) = true;
		}
	      if (!std::isnan(out(x,y,z,c))) //Once NaN, always NaN
		{
		  if (max)
		    {
		      out(x,y,z,c) = (out(x,y,z,c) > v)? out(x,y,z,c): v;
		    }
		  else
		    {
		      out(x,y,z,c) = (out(x,y,z,c) < v)? out(x,y,z,c): v;
		    }
		}
	    }
	}
    }
  if (na_rm)
    {
      cimg_forXYZC(out,x,y,z,c){ if (!valid(x,y,z,c)) { out(x,y,z,c) = NA_REAL; } }
    }
  return wrap(out);
}


// NumericVector reduce_prod(List x,int summary = 0)
// {
//   CImgList<double> L = sharedCImgList(x);
//   CId out(L.at(0),false);
//   int n = x.size();
//   for (int i = 1;  i < n; i++)
//     {
//       out = out.mul(L.at(i));
//     }
//   return wrap(out);
// }


// [[Rcpp::export]]
NumericVector reduce_list(List x,int summary = 0)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),false);
  int n = x.size();
  cimg_pragma_openmp(parallel for cimg_openmp_if(out.size()>=65536))
  cimg_forXYZC(out,x,y,z,c)
    {
      CId vec(n,1,1,1);
      for (int i = 0; i <n; i++)
	{
	  vec[i] = L.at(i)(x,y,z,c);
	  //	  vec[i] = L.atNXYZC(i,x,y,z,c);
	}
      switch (summary)
	{
	case 1:
	  out(x,y,z,c) = vec.min(); break;
	case 2:
	  out(x,y,z,c) = vec.max(); break;
	case 3:
	  out(x,y,z,c) = vec.median(); break;
	case 4:
	  out(x,y,z,c) = vec.variance(); break;
	case 5:
	  out(x,y,z,c) = sqrt(vec.pow(2).sum()); break;
	}
    }
  return wrap(out);
}


//OpenMP seems not to do anything for these functions on certain platforms. Don't know why yet. 

// [[Rcpp::export]]
NumericVector reduce_list2(List x,int summary = 0)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),false);
  int n = x.size();
  //  cimg_pragma_openmp(parallel for collapse(2))
  cimg_forXYZC(out,x,y,z,c)
    {
      NumericVector vec(n);
      for (int i = 0; i <n; i++)
	{
	  vec(i) = L.at(i)(x,y,z,c);
	}
      //out(x,y,z,c) = mean(vec);
      switch (summary)
      	{
      	case 1:
      	  out(x,y,z,c) = min(vec); break;
      	case 2:
      	  out(x,y,z,c) = max(vec); break;
      	case 3:
      	  out(x,y,z,c) = median(vec); break;
	}
      // 	// case 4:
      // 	//   out(x,y,z,c) = vec.variance(); break;
      // 	// case 5:
      // 	//   out(x,y,z,c) = sqrt(vec.pow(2).sum()); break;
      // 	}
    }
  return wrap(out);
}


// [[Rcpp::export]]
NumericVector reduce_med(List x,bool na_rm=false)
{
  CImgList<double> L = sharedCImgList(x);
  CId out(L.at(0),false);
  int n = x.size();
  //  cimg_pragma_openmp(parallel for collapse(2))
  cimg_forXYZC(out,x,y,z,c)
    {
      NumericVector vec(n);
      for (int i = 0; i <n; i++)
	{
	  vec(i) = L.at(i)(x,y,z,c);
	}
      out(x,y,z,c) = median(vec,na_rm);
      // 	// case 4:
      // 	//   out(x,y,z,c) = vec.variance(); break;
      // 	// case 5:
      // 	//   out(x,y,z,c) = sqrt(vec.pow(2).sum()); break;
      // 	}
    }
  return wrap(out);
}


// [[Rcpp::export]]
List psort(List x,bool increasing = true)
{
  CImgList<double> L = sharedCImgList(x);
  CImgList<double> out(L,false);
  int n = x.size();
  cimg_pragma_openmp(parallel for cimg_openmp_if(out.size()>=65536))
    cimg_forXYZC(L.at(0),x,y,z,c)
    {
      CId vec(n,1,1,1),perm(n,1,1,1);
      
      for (int i = 0; i <n; i++)
	{
	  vec[i] = L.at(i)(x,y,z,c);
	  //	  vec[i] = L.atNXYZC(i,x,y,z,c);
	}
      vec.sort(perm,increasing);
      for (int i = 0; i <n; i++)
	{
	  out.at(i)(x,y,z,c) = vec[i];
	}

    }
  return wrap(out);
}

// [[Rcpp::export]]
List porder(List x,bool increasing = true)
{
  CImgList<double> L = sharedCImgList(x);
  CImgList<double> out(L,false);
  int n = x.size();
  cimg_pragma_openmp(parallel for cimg_openmp_if(out.size()>=65536))
    cimg_forXYZC(L.at(0),x,y,z,c)
    {
      CId vec(n,1,1,1),perm(n,1,1,1);
      
      for (int i = 0; i <n; i++)
	{
	  vec[i] = L.at(i)(x,y,z,c);
	  //	  vec[i] = L.atNXYZC(i,x,y,z,c);
	}
      vec.sort(perm,increasing);
      for (int i = 0; i <n; i++)
	{
	  out.at(i)(x,y,z,c) = perm[i] + 1;
	}

    }
  return wrap(out);
}


// [[Rcpp::export]]
List prank(List x,bool increasing = true)
{
  CImgList<double> L = sharedCImgList(x);
  CImgList<double> out(L,false);
  int n = x.size();
  cimg_pragma_openmp(parallel for cimg_openmp_if(out.size()>=65536))
    cimg_forXYZC(L.at(0),x,y,z,c)
    {
      CId vec(n,1,1,1),perm(n,1,1,1);
      
      for (int i = 0; i <n; i++)
	{
	  vec[i] = L.at(i)(x,y,z,c);
	  //	  vec[i] = L.atNXYZC(i,x,y,z,c);
	}
      vec.sort(perm,increasing);
      for (int i = 0; i <n; i++)
	{
	  out.at(perm(i))(x,y,z,c) = i + 1;
	}

    }
  return wrap(out);
}
