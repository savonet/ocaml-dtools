AC_INIT([ocaml-dtools],[0.3.2],[savonet-users@lists.sourceforge.net])
VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

AC_BASE_CHECKS()

REQUIRES="str unix threads"
#
# Syslog
#

AC_ARG_WITH([syslog-dir],
   AS_HELP_STRING([--with-syslog-dir=path],[look for ocaml-syslog library in "path" (autodetected by default)]))
AC_ARG_ENABLE([syslog],
              AS_HELP_STRING([--disable-syslog],["don't use ocaml-syslog"]))

if test "x$enable_syslog" != "xno" ; then
AC_CHECK_OCAML_BINDING([syslog])
fi

if test "$W_SYSLOG" != ""; then
	SYSLOG_FILES="dtools_syslog.ml"
	REQUIRES="$REQUIRES syslog"
	INC="$INC `$OCAMLFIND query syslog`"
fi

# Check for mlockall
AC_CHECK_HEADER(sys/mman.h,
  [AC_CHECK_FUNC(mlockall,
    [CFLAGS="$CFLAGS -DUSE_MLOCK"])])

# substitutions to perform
AC_SUBST(REQUIRES)
AC_SUBST(SYSLOG_FILES)
AC_SUBST(INC)
AC_SUBST(VERSION)

# Finally create the Makefile and samples
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_OUTPUT
