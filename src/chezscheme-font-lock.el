;;; chezscheme-font-lock.el --- custom font locking for Chez Scheme mode

;;Copyright (C) 2008, 2009, 2010, 2011, 2012,
;;   2013, 2014, 2015, 2016, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;Copyright (C) 1986, 1987, 1988, 1997, 1998,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008  Free Software Foundation, Inc.

;;Author: Marco Maggi <marco.maggi-ipsu@poste.it>
;;Maintainer: Marco Maggi <marco.maggi-ipsu@poste.it>
;;Created: Tue Nov 18, 2008
;;Keywords: languages, lisp, faces, matching
;;Version: 1.0

;;This file is part of Marco's Home Directory

;;This program is  free software: you can redistribute  it and/or modify
;;it under the  terms of the GNU General Public  License as published by
;;the Free Software Foundation, either  version 3 of the License, or (at
;;your option) any later version.

;;This program  is distributed in the  hope that it will  be useful, but
;;WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
;;MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
;;General Public License for more details.

;;You  should have received  a copy  of the  GNU General  Public License
;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;This file should be loaded with:
;;
;;  (require 'chezscheme-font-lock)
;;
;;upon  loading variables  and  functions are  defined,  but nothing  is
;;activated.  We need to invoke the functions below from the Scheme mode
;;hook, like this:
;;
;;  (add-hook 'scheme-mode-hook
;;            'chezscheme-setup-font-locking)
;;  (add-hook 'scheme-mode-hook
;;            'chezscheme-font-lock-defined-syntaxes)
;;
;;alternatively we  can interactively  setup the configuration  with the
;;command `M-x chezscheme-setup-font-locking'.

;;; Change Log:


;;; Documentation:

;;Introduction
;;------------
;;
;;To understand what is going on read the Emacs guide (node "Font Lock")
;;and the  Elisp guide (node  "Font Lock Mode").   Here we give  a brief
;;description, cutting out the  alternatives and focusing on the choices
;;made for this customisation.
;;
;;There are a  plethora of font locking related  variables; here we have
;;to understand the two  buffer local variables `font-lock-defaults' and
;;`font-lock-keywords',    and    how    they    interact    with    the
;;`font-lock-add-keywords' function.
;;
;;Inspection
;;----------
;;
;;At any instant we can inspect the list of faces defined in a buffer by
;;issuing `M-x list-faces-display':  it will open a buffer  with a table
;;of faces and examples.
;;
;;To try the colour configuration of a face: open a buffer with the mode
;;on, do  `M-x set-face-foreground' then type  the name of  the face and
;;the name of  the colour.  The change will be  applied instantly to all
;;the text marked with the selected face.
;;
;;When debugging  keep an eye on  the "*Messages*" buffer: if  a face is
;;not recognised there should be a message there.
;;
;;To test  if a string matches  a regexp: in the  *scratch* buffer apply
;;`C-j' to:
;;
;;   (let ((rex "'[[:alnum:]-\\?]+")
;;         (text "'woppa-wippa?"))
;;      (list (string-match rex text)
;;            (match-end 0)))
;;
;;the  result  should be  a  list  of  indexes specifying  the  matching
;;portion.
;;
;;For  reasons currently  unknown  to  me: to  update  the font  locking
;;configuration  we have  to restart  Emacs and  load the  updated files
;;anew.   This is not  required with  other customisations,  for example
;;custom indentation, but with font locking that is the way it is.
;;
;;Keywords
;;--------
;;
;;To get  what we  want, our  target is  to put  The Right  Value(tm) in
;;`font-lock-keywords'.  We SHOULD NOT do it directly with:
;;
;;   (setq font-lock-keywords ...)  ;; NO!!!
;;
;;rather  we  should store  a  statically  computed, full  font  locking
;;specification into  `font-lock-defaults'; then, optionally,  we should
;;dynamically  compute  other  specifications and  activate  them  using
;;`font-lock-add-keywords'.
;;
;;The  value  of  `font-lock-keywords'  has  two parts.   The  first  is
;;computed by Emacs as selected in  "scheme.el", and we do not touch it.
;;The  second  part  is  initialised  from  `font-lock-defaults'  (whose
;;original value is copied  from the function `scheme-mode-variables' in
;;"scheme.el") and optionally updated by `font-lock-add-keywords'.
;;
;;There are 3 mutually exclusive  levels of font locking: light, medium,
;;heavy.  By  default heavy mode is  selected; we can change  this, in a
;;mode   specific  manner,   by  appropriately   setting   the  variable
;;`font-lock-maximum-decoration'.   We  do  not  go into  details  here,
;;suffice it to  say that the high level is selected  for Scheme mode by
;;doing:
;;
;;   (setq font-lock-maximum-decoration '((scheme-mode . 3)))
;;
;;for details read the Emacs guide.
;;
;;The value of `font-lock-defaults' is a list; for the full setting with
;;comments see  the `chezscheme-setup-font-locking' function  below.  Its
;;first element can be a list  of symbols, each symbol being the name of
;;a variable  holding the font  locking specification for a  level.  The
;;default for Scheme mode is:
;;
;;   (scheme-font-lock-keywords
;;    scheme-font-lock-keywords-1
;;    scheme-font-lock-keywords-2)
;;
;;we change it to:
;;
;;   (scheme-font-lock-keywords
;;    scheme-font-lock-keywords-1
;;    chezscheme-font-lock-keywords)
;;
;;and    embed   `scheme-font-lock-keywords-2'    inside    the   custom
;;specification `chezscheme-font-lock-keywords'.
;;
;;Notes on regular expressions
;;----------------------------
;;
;;We need regular expressions to match portions of Scheme code.  We must
;;remember that:
;;
;;* Basically a regular expression should  be a string like "\\(...\\)",
;;  in  which the  dots  are replaced  by the  pattern,  and the  quoted
;;  parentheses select a grouping pattern.
;;
;;* When a regular expression is composed by more, alternative, patterns
;;  we join them with a quoted bar: "...\\|...".
;;
;;* The regexp  for an  optional white space  is: "\\s-*",  where "\\s-"
;;  represents any white space character.
;;
;;* The regexp for a mandatory white space is: "\\s-+".
;;
;;* The regexp for a mandatory white  space followed by an optional open
;;  parenthesis is: "\\s-+(?".
;;
;;* The "\\<" and "\\>" match the empty strings at the beginning and end
;;  of  a "word".  What  constitutes a  "word" is  defined by  the Emacs
;;  syntax table active in Scheme mode.
;;
;;    We DO NOT  want to change the definition of  "word", because it is
;;  used, for example, with the key commands [M-f] and [M-b].
;;
;;* To  test a  set  of regexps  definitions: write  them  in a  buffer,
;;  evaluate  the buffer  with `eval-buffer',  then, in  the "*scratch*"
;;  buffer, try something like:
;;
;;     (string-match chezscheme-identifier-rex "->ciao")
;;     (string-match chezscheme-identifier-rex "ciao")
;;     (string-match chezscheme-identifier-rex "ciao-hello")
;;     (string-match chezscheme-identifier-rex "ciao,hello")
;;
;;  the return  value of `string-match' is  the index of  the first char
;;  that matched, or nil if there is no match.
;;
;;To  build optimised  regular expressions  for lists  of words,  we use
;;`regexp-opt'; notice  that it automatically quotes  special characters
;;in the input strings and it automatically adds grouping parentheses at
;;the beginning and end if the second argument is non-`nil', for example
;;`t'.  Usage example:
;;
;;  (eval-when-compile
;;    (regexp-opt '("define" "define*") 'symbols))
;;
;;To build a  regexp for a list  of words the second  argument should be
;;`words'; to  match a  list of symbols,  like the  programming language
;;keywords, the second argument should be `symbols'.
;;
;;To  match at  the  beginning of  a  Scheme form,  we  prepend an  open
;;parenthesis and an  optional white space and append  a mandatory white
;;space:
;;
;;  (concat "(\\s-*"
;;          (eval-when-compile
;;            (regexp-opt '("define" "define*") 'symbols))
;;          "\\s-+")
;;
;;Notes on faces
;;--------------
;;
;;To define and use a face we do 2 steps: define the customisation item,
;;define  a variable  holding the  name of  the face.   For unfathomable
;;reasons, we need both.
;;
;;The namespace  of faces  is separated  from the others,  so it  is not
;;mandatory to name  a face with a symbol ending  with `-face'.  Here we
;;do it nevertheless.
;;
;;We define a  set of faces with `defface' forms and  a set of variables
;;with `defconst'  forms.  The variables  can reference both a  built in
;;Emacs face or a custom face.
;;


;;;; setup

(require 'scheme)
(eval-when-compile
  (require 'chezscheme-lists-of-symbols.el))


;;;; custom faces

(defface chezscheme-language-keywords-face
  `((t (:foreground "aquamarine3")))
  "Scheme mode custom face used for custom keywords."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-language-keywords-face
  'chezscheme-language-keywords-face
  "Scheme mode custom face used for custom keywords.")

;;; --------------------------------------------------------------------

(defface chezscheme-language-functions-face
  `((t (:foreground "LightSteelBlue")))
  "Scheme mode custom face used for custom functions."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-language-functions-face
  'font-lock-builtin
  "Scheme mode custom face used for custom functions.")

;;; --------------------------------------------------------------------

(defface chezscheme-unquoted-identifier-face
  `((t (:foreground "salmon")))
  "Scheme mode custom face used for unquoted symbols."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-unquoted-identifier-face
  'chezscheme-unquoted-face
  "Scheme mode custom face used for unquoted symbols.")

;;; --------------------------------------------------------------------

(defface chezscheme-quoted-identifier-face
  `((t (:foreground "aquamarine")))
  "Scheme mode custom face used for unquoted symbols."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-quoted-identifier-face
  'chezscheme-quoted-identifier-face
  "Scheme mode custom face used for unquoted symbols.")

;;; --------------------------------------------------------------------

(defface chezscheme-unsafe-identifier-face
  `((t (:foreground "pink")))
  "Scheme mode custom face used for unquoted symbols."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-unsafe-identifier-face
  'chezscheme-unsafe-identifier-face
  "Scheme mode custom face used for identifiers of unsafe bindings.")

;;; --------------------------------------------------------------------

(defface chezscheme-non-validating-identifier-face
  `((t (:foreground "pink")))
  "Scheme mode custom face used for unquoted symbols."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-non-validating-identifier-face
  'chezscheme-non-validating-identifier-face
  "Scheme mode custom face used for identifiers of non-validating bindings.")

;;; --------------------------------------------------------------------

(defface chezscheme-pattern-variable-face
  `((t (:foreground "gold")))
  "Scheme mode custom face for syntax pattern variables."
  :group 'scheme
  :group 'custom-faces)

(defconst chezscheme-pattern-variable-face
  'chezscheme-pattern-variable-face
  "Scheme mode custom face for syntax pattern variables.")

;;; --------------------------------------------------------------------

(defconst chezscheme-language-conditions-face
  'font-lock-constant-face
  "Scheme mode custom face used for conditions in R6RS.")

;;; --------------------------------------------------------------------

(defface my-font-lock-brace-face
  `((t :foreground "aquamarine2"))
  "Chezscheme Scheme mode custom face used for brace parentheses."
  :group 'scheme
  :group 'custom-faces)

(defconst my-font-lock-brace-face
  'my-font-lock-brace-face
  "Chezscheme Scheme mode custom face used for brace parentheses.")


;;;; regular expressions

(defconst chezscheme-identifier-rex
  (eval-when-compile
    chezscheme-identifier-internal-rex)
  "Regexp to match Scheme language ASCII identifiers.")

(defconst chezscheme-pattern-variable-rex
  (eval-when-compile
    (concat "\\?" chezscheme-identifier-internal-rex))
  "Regexp to match Scheme language pattern variables.")

(defconst chezscheme-unsafe-identifier-rex
  (eval-when-compile
    (concat "\\$" chezscheme-identifier-internal-rex))
  "Regexp to match Scheme language unsafe binding identifiers.")

(defconst chezscheme-non-validating-identifier-rex
  (eval-when-compile
    (concat "\\~" chezscheme-identifier-internal-rex))
  "Regexp to match Scheme language non-validating binding identifiers.")

(defconst chezscheme-unquoted-identifier-rex
  (eval-when-compile
    (concat "\\," chezscheme-identifier-internal-rex))
  "Regexp to match Scheme language unquoted symbols.")

(defconst chezscheme-quoted-identifier-rex
  (eval-when-compile
    (concat "'" chezscheme-identifier-internal-rex))
  "Regexp to match Scheme language unquoted symbols.")

(defconst chezscheme-constant-rex
  (eval-when-compile
    (concat "\\("
	    (regexp-opt chezscheme-constants-list t)
	    "\\|"
	    "\\|" "'()"
	    "\\(#[0-9]+[#=]\\)" ;This is for "#0#", "#0=" and the like.
	    "\\|"
	    "^#!" (regexp-opt '("eof" "ikarus" "r6rs" "chezscheme") 'words)
	    "\\)"))
  "Regexp to match Scheme language constants.")

;;; --------------------------------------------------------------------

(defconst chezscheme-keywords-rex
  (eval-when-compile
    (concat "(\\s-*" (regexp-opt chezscheme-r6rs-keywords-list 'symbols)))
  "Regexp to match Scheme language keywords at the beginning of a form.")

(defconst chezscheme-aux-syntaxes-rex
  (eval-when-compile
    (regexp-opt (append chezscheme-aux-syntaxes-list
			chezscheme-chezscheme-aux-syntaxes-list)
		'symbols))
  "Regexp to match Scheme language auxiliary syntaxes.")

(defconst chezscheme-functions-rex
  (eval-when-compile
    (regexp-opt (append chezscheme-r6rs-functions-list
			chezscheme-custom-functions-list)
		'symbols))
  "Regexp to match Scheme language functions.")

(defconst chezscheme-conditions-rex
  (eval-when-compile
    (regexp-opt chezscheme-r6rs-condition-list 'symbols))
  "Regexp to match Scheme language conditions.")

(defconst chezscheme-custom-keywords-rex
  (eval-when-compile
    (concat "(\\s-*" (regexp-opt chezscheme-custom-syntaxes-list 'symbols)))
  "Regexp to match Scheme language keywords from custom libraries.")

(defconst chezscheme-custom-functions-rex
  (eval-when-compile
    (concat "(\\s-*" (regexp-opt chezscheme-custom-functions-list 'symbols)))
  "Regexp to match Scheme language functions from custom libraries.")

(defconst chezscheme-warning-rex
  (eval-when-compile
    (regexp-opt chezscheme-warning-list 'words))
  "Regexp to match warning strings in Scheme language source files.")


;;;; font locking configuration
;;
;;To test the syntaxes try the following code:
;;
;; (define O 1)
;; (define {O <byte>} 1)
;; (define (O a b) 123)
;; (define ({O <byte>} a b) 123)
;; (define-method (O a b) 123)
;; (define-method ({O <byte>} a b) 123)
;; (case-define ciao
;;   ((a) 123))
;; (case-define {ciao <byte>}
;;   ((a) 123))
;; (define-values (a b c)
;;   (values 1 2 3))
;; (define-values ({a <byte>} {b <byte>} {c <byte>})
;;   (values 1 2 3))
;; (define-constant-values (a b c)
;;   (values 1 2 3))
;; (define-constant-values ({a <byte>} {b <byte>} {c <byte>})
;;   (values 1 2 3))
;; (define-constant ciao
;;   123)
;; (define-constant {ciao <byte>}
;;   123)
;; (define-inline-constant ciao
;;   123)
;; (define-inline-constant {ciao <byte>}
;;   123)
;; #vu8(1) #vs8(1)
;; #vu16l(1) #vu16b(1) #vu16n(1) #vs16l(1) #vs16b(1) #vs16n(1)
;; #vu32l(1) #vu32b(1) #vu32n(1) #vs32l(1) #vs32b(1) #vs32n(1)
;; #vu64l(1) #vu64b(1) #vu64n(1) #vs64l(1) #vs64b(1) #vs64n(1)
;; #vf4l(1) #vf4b(1) #vf4n(1) #vf8l(1) #vf8b(1) #vf8n(1)
;; #vc4l(1) #vc4b(1) #vc4n(1) #vc8l(1) #vc8b(1) #vc8n(1)
;; #ve(ascii "a") #ve(latin1 "a") #ve(hex "a") #ve(base64 "a")
;; #ve(percent-encoding "a")
;; #ve(utf8 "a") #ve(utf16le "a") #ve(utf16be "a") #ve(utf16n "a")
;;

;;NOTE!!!
;;
;;* The regexp `chezscheme-identifier-internal-rex' is not wrapped in the
;;  grouping parentheses  "\\(" and  "\\)", so we  should wrap  it here.
;;  The regexp makes use of the shy grouping "\\(?:", "\\)".
;;

(defconst chezscheme-font-lock-specials
  ;;This  is a modified  version of  what is  in "scheme.el",  GNU Emacs
  ;;22.3.
  ;;
  `( ;;This does DEFINE and DEFINE-METHOD for function bindings only.
    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("define" "define/std" "define/typed" "define-method") 'symbols)
		"\\s-+(" ;any whitespace and open paren
		"{?"	 ;optional open brace
		"\\s-*"	 ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    ;;This does DEFINE and DEFINE-CONSTANT for variable bindings only.
    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("define" "define-constant" "define-inline-constant") 'symbols)
		"\\s-+"   ;any whitespace
		"{?"	  ;optional open brace
		"\\s-*"	  ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face nil t))

    ;;This does  various definitions when  an open brace is  accepted to
    ;;tag the identifier.
    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("case-define" "case-define/std" "case-define/typed"
			      "case-define*" "define*"
			      "define-values" "define-constant-values"
			      "define-inline"
			      "define-integrable" "define-returnable")
			    'symbols)
		"\\s-+(?" ;any whitespace and declared object
		"(?"	  ;option open paren
		"\\s-*"	  ;optional white space separator
		"{?"	  ;option open brace
		"\\s-*"	  ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    ;;This does various  definitions when an open brace  is NOT accepted
    ;;to tag the identifier.
    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("define-predicate-procedure-argument-validation"
			      "define-predicate-return-value-validation"
			      "define-enumeration"
			      "define-argument-validation"
			      "define-auxiliary-syntaxes"
			      "define-generic-definer" "define-generic*-definer"
			      "define-generic" "define-generic*"
			      "define-type")
			    'symbols)
		"\\s-+(?" ;any whitespace and declared object
		"(?"	  ;option open paren
		"\\s-*"	  ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("define-syntax" "define-syntax*"
			      "define-syntax-rule"
			      "define-fluid-syntax" "define-fluid-override"
			      "define-alias")
			    'symbols)
		"\\s-+" ;white space separator
		"(?"    ;optional open paren
		"\\s-*" ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 chezscheme-language-keywords-face nil t))

    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("define-label-type" "define-mixin-type" "define-interface-type" "define-struct")
			    'symbols)
		"\\s-+" ;white space separator
		"(?"    ;optional open paren
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))

    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("define-record-type" "define-condition-type")
			    'symbols)
		"\\s-+" ;white space separator
		"(?"	;optional open paren
		"\\s-*" ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-type-face nil t))

    ;;Tagged identifier with brace syntax.
    (,(eval-when-compile
	(concat "\\(?:{\\|brace\\)" ;open brace
		"\\s-*"		    ;optional white space separator
		"\\(" chezscheme-identifier-internal-rex "\\)"
		))
     ;;Which face should we use?  I cannot decide!  Emacs defines a font
     ;;lock    face   specifically    for   variables    being   defined
     ;;`font-lock-variable-name-face',  so  we  should use  it.   (Marco
     ;;Maggi; Thu Mar 6, 2014)
     ;;
     ;;Here `keep' means: keep the fontification from a previous match.
     (1 font-lock-variable-name-face keep))

    ;; This is for named let.
    (,(eval-when-compile
	(concat "(let\\s-+" "\\(" chezscheme-identifier-internal-rex "\\)"))
     1 font-lock-function-name-face)

    ;;This is  for class specifiers.  We  want to accept both  the clean
    ;;identifier between angular parentheses:
    ;;
    ;;   <fixnum>
    ;;
    ;;and  the prefixed  identifier,  which usually  (in  my own  Chezscheme
    ;;Scheme code) has a prefix separated with a dot:
    ;;
    ;;   built-in.<fixnum>
    ;;
    (,(eval-when-compile
	(concat "\\<\\("
		"\\(?:" chezscheme-identifier-internal-rex "\\.\\)?"
		"<" chezscheme-identifier-internal-rex ">"
		"\\)\\>"))
     1 font-lock-type-face)
    ;;;("\\<\\(?:[a-zA-Z\-]+\.\\)?<\\sw+>\\>" . font-lock-type-face)

    ;; Scheme `:' and `#:' keywords as builtins.
    ("\\<#?:\\sw+\\>" . font-lock-builtin-face)

    ;;Condition object types.
    (,(eval-when-compile
	(concat "\\<\\("
		"\\(?:" chezscheme-identifier-internal-rex "\\.\\)?"
		"&" chezscheme-identifier-internal-rex
		"\\)\\>"))
     1 font-lock-type-face)

    ;; Braces.
    ("\\([\\{\\}]\\)"
     (1 my-font-lock-brace-face))

    ;;This does numeric bytevectors.
    (,(eval-when-compile
	(concat "\\(#"
		(regexp-opt '("vu8"  "vs8"
			      "vu16l" "vu16b" "vu16n" " vs16l" "vs16b" "vs16n"
			      "vu32l" "vu32b" "vu32n" " vs32l" "vs32b" "vs32n"
			      "vu64l" "vu64b" "vu64n" " vs64l" "vs64b" "vs64n"
			      "vf4l" "vf4b" "vf4n" "vf8l" "vf8b" "vf8n"
			      "vc4l" "vc4b" "vc4n" "vc8l" "vc8b" "vc8n")
			    'symbols)
		"\\)(\\s-*"))
     (1 font-lock-constant-face))

    ;;This does bytevector datums with encoding.
    (,(eval-when-compile
	(concat "\\(#ve\\)"
		"(\\s-*"
		"\\("
		(regexp-opt '("ascii" "latin1" "hex" "base64" "percent-encoding"
			      "utf8" "utf16le" "utf16be" "utf16n")
			    'symbols)
		"\\)"))
     (1 font-lock-constant-face)
     (2 font-lock-constant-face))

    ;; ("(\\([\\*\\+/<=>\\-]\\|\\(?:<=\\)\\|\\(?:>=\\)\\)\\>"
    ;;  1 font-lock-keyword-face)
    )
  "Special font locking for Scheme mode.

Defines special font lock rules for functions, variables, macros,
classes, methods definitions.  Deals  with named lets, class type
symbols and symbols whose name starts with a colon.")

;;Tips:
;;
;;* The first elements in the list are used first to highlight stuff; in
;;  normal mode  subsequent elements do not  override the fontification.
;;  So order does matter.
;;
;;* The following specification:
;;
;;     (REGEXP 1 font-lock-keyword-face)
;;
;;  means  take  the first  submatch  (the  thing  enclosed in  grouping
;;  parenthesis   "\\(...\\)")    in   the   REGEXP    and   apply   the
;;  `font-lock-keyword-face'.
;;
;;The form  `eval-when-compile' could  be used to  speed up  building of
;;this alist,  but it  requires the  lists of words  to be  available at
;;compile time, not  at load time.  This prevents  usage of variables to
;;hold  lists  of words.   Here  we  give  priority to  flexibility  and
;;readability over speed.
;;
(defconst chezscheme-font-lock-static-keywords
  `(;;(,chezscheme-unquoted-identifier-rex	. chezscheme-unquoted-identifier-face)
    (,chezscheme-quoted-identifier-rex	. chezscheme-quoted-identifier-face)
    (,chezscheme-pattern-variable-rex	. chezscheme-pattern-variable-face)
    (,chezscheme-unsafe-identifier-rex	. chezscheme-unsafe-identifier-face)
    (,chezscheme-non-validating-identifier-rex	. chezscheme-non-validating-identifier-face)
    (,chezscheme-constant-rex		0 font-lock-constant-face t)
    (,chezscheme-conditions-rex		0 chezscheme-language-conditions-face)
    ;;This one has a regexp pattern including the open paren, so we must
    ;;set the SUBEXP field to 1.
    (,chezscheme-keywords-rex		1 font-lock-keyword-face)
    (,chezscheme-aux-syntaxes-rex	0 font-lock-keyword-face)
    (,chezscheme-functions-rex		0 font-lock-builtin-face)
    ;;This one has a regexp pattern including the open paren, so we must
    ;;set the SUBEXP field to 1.
    (,chezscheme-custom-keywords-rex	1 font-lock-keyword-face) ;;chezscheme-language-keywords-face)
    (,chezscheme-custom-functions-rex	0 chezscheme-language-functions-face)
    (,chezscheme-warning-rex		0 font-lock-warning-face))
  "Custom font locking keywords for Scheme mode.

This variable is made buffer local whenever it is set.")

(defvar chezscheme-font-lock-keywords
  (append chezscheme-font-lock-specials
	  chezscheme-font-lock-static-keywords)
  "Custom font lock keywords for Scheme mode.")


;;;; main hook

(defun chezscheme-setup-font-locking ()
  "Configure font locking for Chez Scheme."
  (interactive)
  ;;(make-variable-buffer-local 'chezscheme-font-lock-keywords)
  (setq font-lock-defaults
	'( ;;Configuration for the font  locking levels.
	  ;;
	  ;;When a  list value is  used: at font  locking initialisation
	  ;;time a level  (1, 2, 3) of font locking  is selected and the
	  ;;corresponding specification in the  following list is stored
	  ;;into    `font-lock-keywords'.
	  ;;
	  ;;NOTE For some reason this setting does NOT work; so below we
	  ;;set `font-lock-keywords' directly.
	  (scheme-font-lock-keywords	 ;default, level 1
 	   scheme-font-lock-keywords-1	 ;level 2
	   chezscheme-font-lock-keywords) ;level 3

	  ;;Keywords-only    argument,   nil    means:    do   syntactic
	  ;;fontification.
	  nil

	  ;;Case-fold argument, non-nil means: ignore case.
	  t

	  ;;Syntax-alist   argument:   setup    a   syntax   table   for
	  ;;fontification.
	  (("+-*/.,<>=!?$%_&~^:" . "w"))

	  ;;Syntax-begin argument:  a function  that moves point  to the
	  ;;beginning of the top-level enclosing form.
	  beginning-of-defun

	  ;;This cons makes `font-lock-mark-block-function' buffer local
	  ;;and sets it to `mark-defun'.  It is a function that selects,
	  ;;as  a region,  a  block of  code  to be  highlighted by  the
	  ;;function`font-lock-fontify-block'.
	  (font-lock-mark-block-function . mark-defun)))
  ;;For debugging purposes, just in case setting `font-lock-defaults' as
  ;;done above does  not work, we can try the  following direct setting.
  ;;We should not do it for normal operations.
  ;;
  ;;(setq font-lock-keywords chezscheme-font-lock-keywords)
  )


;;;; done

(provide 'chezscheme-font-lock)

;;; chezscheme-font-lock.el ends here
