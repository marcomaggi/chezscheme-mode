## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL configure.sh

CLEANFILES		=
MAINTAINERCLEANFILES	=

## --------------------------------------------------------------------
## documentation

dist_doc_DATA			= README COPYING
AM_MAKEINFOFLAGS		= --no-split
info_TEXINFOS			= doc/chezscheme-mode.texi
doc_chezscheme_mode_TEXINFOS	= doc/fdl-1.3.texi doc/gpl-3.0.texi doc/macros.texi

## --------------------------------------------------------------------

MY_EMACS_DIRS		= \
	--directory $(srcdir)/src			\
	--directory $(builddir)/src			\
	--directory /usr/share/emacs/site-lisp		\
	--directory /usr/local/share/emacs/site-lisp

# This  variable is  reserved to  the  package author.   Users of  the
# package can set ELCFLAGS.
AM_ELCFLAGS	= --no-init-file $(MY_EMACS_DIRS)

sharelispdir		= $(datadir)/emacs/site-lisp
dist_sharelisp_LISP	= \
	src/chezscheme.el			\
	src/chezscheme-font-lock.el		\
	src/chezscheme-lists-of-symbols.el

src/chezscheme-font-lock.elc: src/chezscheme-lists-of-symbols.el

src/chezscheme.elc: src/chezscheme-font-lock.elc

### end of file
