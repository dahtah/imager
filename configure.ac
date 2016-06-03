## autoconf script partly borrowed from rgl package by Duncan Murdoch


AC_PREREQ(2.50)

## ---[ VERSION ]-------------------------------------------------------------

AC_INIT()
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




# Check if the FFTW Library exists on the system
AC_MSG_NOTICE([Checking if FFTW3 library is available using pkg-config])

## --- FFTW 3 ----------------------------------------------------------------
PKG_CHECK_MODULES(FFTW, fftw3, [HAVE_FFTW=TRUE], [HAVE_FFTW=FALSE])

if test "$HAVE_FFTW" == TRUE
then
	AC_MSG_RESULT([FFTW3 found!])
	dnl # See if all functions exist for correct compilation
	dnl AC_MSG_NOTICE([Testing all required C level functions])
	dnl # Insert all the compiler checks
	dnl AC_CHECK_FUNCS([memset])
	dnl # Math library checks
	dnl AC_CHECK_HEADER(math.h)
	dnl AC_CHECK_LIB([m], [cos])
	dnl AC_CHECK_LIB([m], [sin])
	dnl AC_CHECK_LIB([m], [sqrt])
	dnl AC_CHECK_LIB([m], [floor])
	dnl # FFTW library checks.
	dnl AC_CHECK_LIB([fftw3], [fftw_malloc])
	dnl AC_CHECK_LIB([fftw3], [fftw_free])
	dnl AC_CHECK_LIB([fftw3], [fftw_execute])
	dnl AC_CHECK_LIB([fftw3], [fftw_destroy_plan])
	dnl AC_CHECK_LIB([fftw3], [fftw_plan_many_dft_r2c])
	dnl AC_CHECK_LIB([fftw3], [fftw_plan_dft_c2r])
	dnl AC_CHECK_LIB([fftw3], [fftw_plan_dft_r2c_1d])
else
  # Return message that package requires FFTW3 library
  AC_MSG_RESULT([FFTW library not found, please install fftw3])
fi



# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(FFTW_CFLAGS)
AC_SUBST(FFTW_LIBS)


## --- Output ----------------------------------------------------------------

AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT(src/Makevars)
