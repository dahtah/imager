


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
  # Return message that package requires FFTW3 library
  AC_MSG_RESULT([FFTW library not found, please install fftw3 for better FFT support.])
fi



# Check for OpenMP (from randomForests package)
AC_OPENMP

RBIN="${R_HOME}/bin/R"

# Since some systems have broken OMP libraries we also check that the
# actual package will work.
ac_pkg_openmp=no
if test -n "${OPENMP_CFLAGS}"; then
  AC_MSG_CHECKING([whether OpenMP will work in a package])
  AC_LANG_CONFTEST([AC_LANG_PROGRAM([[#include <omp.h>]], [[ return omp_get_num_threads (); ]])])
  "$RBIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "$RBIN" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_openmp=yes
  AC_MSG_RESULT([${ac_pkg_openmp}])
fi

# If ${ac_pkg_openmp} = "yes" then we have OMP, otherwise it will be "no".
if test "${ac_pkg_openmp}" = no; then
  OPENMP_CFLAGS=''
  HAVE_OPENMP=''
  AC_MSG_WARN([OpenMP does NOT appear to be supported on this system.])
else
  HAVE_OPENMP='-Dcimg_use_openmp'
fi

AC_SUBST(OPENMP_CFLAGS)
AC_SUBST(HAVE_OPENMP)


# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(HAVE_FFTW)
AC_SUBST(FFTW_CFLAGS)
AC_SUBST(FFTW_LIBS)


## --- Output ----------------------------------------------------------------

AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT(src/Makevars)
