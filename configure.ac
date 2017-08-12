dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Chez Scheme Mode],[0.1.0],[marco.maggi-ipsu@poste.it],
  [chezscheme-mode],[http://github.com/marcomaggi/chezscheme-mode/])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign subdir-objects dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

AX_CHEZ_SCHEME

AM_PATH_LISPDIR

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
