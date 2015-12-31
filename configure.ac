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


## --- Output ----------------------------------------------------------------

AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT(src/Makevars)
