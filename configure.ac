


AC_PREREQ(2.62)


## ---[ VERSION ]-------------------------------------------------------------

AC_INIT()

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

RBIN="${R_HOME}/bin/R"

## OS checking borrowed from rgl package by Duncan Murdoch
##AC_CONFIG_AUX_DIR(src/build/autoconf)
if test `uname` = "Darwin" ; then
  darwin="yes"
  ## we want the *build* cputype and not the host one.
  cmd=`echo $CC $CFLAGS | grep -E 'x86_64|ppc64|-m64'`
  if test -n "$cmd"; then
    have_64bit="yes"
  else
    have_64bit="no"
  fi 
else
  darwin="no"
fi
## --- Darwin ----------------------------------------------------------------


# ---[ X11 ]------------------------------------------------------------------

  AC_PATH_X
  if test x$no_x = xyes ; then
    AC_MSG_ERROR([X11 not found but required, configure aborted.])
  fi
  if test -n "${x_includes}"; then
    CPPFLAGS="${CPPFLAGS} -I${x_includes}"
  fi
  if test -n "${x_libraries}"; then
    LIBS="${LIBS} -L${x_libraries} -lX11"
  else
    LIBS="${LIBS} -lX11"
  fi


if test x$L_LIB != x; then
  LIBS="${L_LIB} ${LIBS}"
fi



# from mwaved package by jrwishart
# Check if the FFTW Library exists on the system
AC_MSG_NOTICE([Checking if FFTW3 library is available using pkg-config])

## --- FFTW 3 ----------------------------------------------------------------
PKG_CHECK_MODULES(FFTW, fftw3, [HAVE_FFTW=TRUE], [HAVE_FFTW=FALSE])

if test "$HAVE_FFTW" == TRUE
then
	HAVE_FFTW='-Dcimg_use_fftw3'
	AC_MSG_RESULT([FFTW3 found!])
else
	HAVE_FFTW=''
  # Return message that package requires FFTW3 library
  AC_MSG_RESULT([FFTW library not found, please install fftw3 for better FFT support.])
fi


#check for libtiff (from rtiff package)
AC_CHECK_LIB([tiff], [TIFFOpen], [have_tiff=yes], [have_tiff=no])
if test "x${have_tiff}" = xyes; then
TIFF_LIBS="-ltiff"
TIFF_CFLAGS="-Dcimg_use_tiff"
else
  ## version for Mac OS X, static libtiff 4.0.x
  unset ac_cv_lib_tiff_TIFFOpen
  echo 'Try static libs needed on OS X'
  AC_CHECK_LIB([tiff], [TIFFOpen], [have_tiff=yes], [have_tiff=no],
               [-llzma -ljpeg -lz])
  if test "x${have_tiff}" = xyes; then
    TIFF_CFLAGS="-Dcimg_use_tiff"
    TIFF_LIBS="-ltiff -llzma -ljpeg -lz"
  else
    echo Could not find libtiff. Install libtiff for better tiff support
    TIFF_CFLAGS=""
    TIFF_LIBS=""
  fi
fi



# Check for OpenMP (from randomForests package)
dnl AC_OPENMP

dnl RBIN="${R_HOME}/bin/R"

dnl # Since some systems have broken OMP libraries we also check that the
dnl # actual package will work.
dnl ac_pkg_openmp=no
dnl if test -n "${OPENMP_CFLAGS}"; then
dnl   AC_MSG_CHECKING([whether OpenMP will work in a package])
dnl   AC_LANG_CONFTEST([AC_LANG_PROGRAM([[#include <omp.h>]], [[ return omp_get_num_threads (); ]])])
dnl   "$RBIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "$RBIN" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_openmp=yes
dnl   AC_MSG_RESULT([${ac_pkg_openmp}])
dnl fi

dnl # If ${ac_pkg_openmp} = "yes" then we have OMP, otherwise it will be "no".
dnl if test "${ac_pkg_openmp}" = no; then
dnl   OPENMP_CFLAGS=''
dnl   HAVE_OPENMP=''
dnl   AC_MSG_WARN([OpenMP does NOT appear to be supported on this system.])
dnl else
dnl   HAVE_OPENMP='-Dcimg_use_openmp'
dnl fi

dnl AC_SUBST(OPENMP_CFLAGS)
dnl AC_SUBST(HAVE_OPENMP)

# Checks for OpenMP.
#m4_include([m4/ax_openmp.m4])
#AX_OPENMP

#OpenMP detection taken from ARTP2 code, as per CRAN recommendation
#https://github.com/zhangh12/ARTP2/blob/master/configure.ac
AC_LANG(C++)
dnl this the meat of R's m4/openmp.m4
  OPENMP_[]_AC_LANG_PREFIX[]FLAGS=
  AC_ARG_ENABLE([openmp],
    [AS_HELP_STRING([--disable-openmp], [do not use OpenMP])])
  if test "$enable_openmp" != no; then
    AC_CACHE_CHECK([for $[]_AC_CC[] option to support OpenMP],
      [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp],
      [AC_LINK_IFELSE([_AC_LANG_OPENMP],
	 [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp='none needed'],
	 [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp='unsupported'
	  for ac_option in -fopenmp -xopenmp -qopenmp \
                           -openmp -mp -omp -qsmp=omp -homp \
			   -fopenmp=libomp \
                           -Popenmp --openmp; do
	    ac_save_[]_AC_LANG_PREFIX[]FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
	    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS $ac_option"
	    AC_LINK_IFELSE([_AC_LANG_OPENMP],
	      [ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp=$ac_option])
	    _AC_LANG_PREFIX[]FLAGS=$ac_save_[]_AC_LANG_PREFIX[]FLAGS
	    if test "$ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp" != unsupported; then
	      break
	    fi
	  done])])
    case $ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp in #(
      "none needed" | unsupported)
	;; #(
      *)
	OPENMP_[]_AC_LANG_PREFIX[]FLAGS=$ac_cv_prog_[]_AC_LANG_ABBREV[]_openmp ;;
    esac
  fi


AC_SUBST(OPENMP_CXXFLAGS)

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(HAVE_FFTW)
AC_SUBST(FFTW_CFLAGS)
AC_SUBST(FFTW_LIBS)

AC_SUBST(TIFF_CFLAGS)
AC_SUBST(TIFF_LIBS)


## --- Output ----------------------------------------------------------------

AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT(src/Makevars)
