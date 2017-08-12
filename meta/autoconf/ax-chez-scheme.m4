dnl ax-chez-scheme.m4 --
dnl
dnl Finds Chez Scheme with executable installed as "chez".

AC_DEFUN([AX_CHEZ_SCHEME],
  [AC_CHECK_PROG([CHEZ_PROGRAM],[chez],[chez],[:])

  # Command  line  options  for  the executable  "chez".   The  variable
  # AX_PFLAGS is for options preselected by the building infrastructure.
  # The variable PFLAGS is for user options selected on the command line
  # of "configure" and "make", for example:
  #
  #    $ make PFLAGS="--optimize-level 3"
  #
  AS_VAR_SET(AX_PFLAGS,["--optimize-level 2"])
  AS_VAR_SET_IF(PFLAGS,,[AS_VAR_SET(PFLAGS)])
  AC_SUBST([AX_PFLAGS])
  AC_SUBST([PFLAGS])
  ])

dnl end of file
dnl Local Variables:
dnl mode: autoconf
dnl End:
