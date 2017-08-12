;;; chezscheme.el --- Chez Scheme Scheme major mode

;; Copyright (C) 2013-2017 Marco Maggi

;; Author: Marco Maggi <marco.maggi-ipsu@poste.it>
;; Created: Tue Dec 10, 2013
;; Time-stamp: <2017-08-12 16:01:32 marco>
;; Keywords: languages

;; This file is part of Chez Scheme Mode.
;;
;; This program is free software:  you can redistribute it and/or modify
;; it under the terms of the  GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is  distributed in the hope that it  will be useful, but
;; WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;; General Public License for more details.
;;
;; You should  have received a  copy of  the GNU General  Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;

;;; Change Log:

;;

;;; Code:

(require 'scheme)
(require 'chezscheme-font-lock)


;;;; helper functions

(defun chezscheme-p-string-insert-and-indent (STRING)
  "Insert a string in the current buffer and indent it."
  (interactive)
  (save-excursion
    (let ((BEGIN (point)))
      (insert STRING)
      (indent-region BEGIN (point) nil))))

(defun chezscheme-p-look-at-previous-char (REGULAR-EXPRESSION-STRING)
  "Match a regular expression against the previous buffer position.

Evaluate `looking-at' with the  given regular expression over the
char that preceeds the current point.  Return the boolean result.
The point is repositioned to the starting point."
  (save-excursion
    (backward-char)
    (looking-at REGULAR-EXPRESSION-STRING)))

(defun chezscheme-p-add-prefix-to (STRING PREFIX)
  "Add a `PREFIX' to the beginning of each line in `STRING'."
  (apply 'concat (mapcar #'(lambda (LINE) (format "%s%s\n" PREFIX LINE))
			 (split-string STRING "\n"))))

(defun chezscheme-insert-custom-indentation nil
  "Insert a customisable line for custom indentation of Scheme syntax."
  (interactive)
  (open-line 1)
  (insert ";; eval: (put 'XXX 'scheme-indent-function 1)"))


;;;; imenu customisation

(defconst chezscheme-imenu-generic-expression
  `(
;;; functions, generic functions, methods

    ;; (define           (the-func ---) ---)
    ;; (define*          (the-func ---) ---)
    ;; (define/typed     (the-func ---) ---)
    ;; (define/checked   (the-func ---) ---)
    ;; (define/std       (the-func ---) ---)
    ;; (define/friend    (the-func ---) ---)
    ;; (define/overload  (the-func ---) ---)
    ;; (define-generic   (the-func ---) ---)
    ;; (define-method    (the-func ---) ---)
    ;;
    ;; (define           the-thing ---)
    ;; (define*          the-thing ---)
    ;; (define-generic   the-thing ---)
    ;; (define-method    the-thing ---)
    (nil ;;"Functions and Variables"
     ,(concat "^("
     	      (regexp-opt '("define" "define*"
     			    "define/std" "define/typed" "define/checked" "define/friend" "define/overload"
     			    "define-generic" "define-method")
     			  'symbols)
     	      "\\s-+(?{?\\(\\sw+\\)")
     ;;This number  is the 1-based  index of the  matching subexpression
     ;;that  matches  the  variable  name.  We  need  to  remember  that
     ;;`regexp-opt'  with second  operand  "symbol"  generates a  single
     ;;matching  subexpression  "\\(  ...   \\)"   and  uses  a  lot  of
     ;;non-matching subexpressions "\\(?: ... \\)".
     2)

    ;; (define-constant		the-thing ---)
    ;; (define-inline-constant	the-thing ---)
    (nil
     "^(define\\(\\(-inline\\)?-constant\\)\\s-+\\(\\sw+\\)" 3)

    ;; (case-define the-func ---)
    ;; (case-define* the-func ---)
    ;; (case-define/std the-func ---)
    ;; (case-define/typed the-func ---)
    ;; (case-define/checked the-func ---)
    (nil
     "^(case-define\\(\\|\\*\\|/std\\|/typed\\|/checked\\)\\s-+\\(\\sw+\\)" 2)

;;; types, labels and classes

    ;; (define-label-type <the-tag> ---)
    ;; (define-mixin-type <the-tag> ---)
    ;; (define-type       <the-tag> ---)
    (nil
     ,(concat "^("
	      (regexp-opt '("define-type" "define-record-type" "define-label-type" "define-mixin-type") 'symbols)
	      "\\s-+(?\\(\\sw+\\)")
     ;;This number  is the 1-based  index of the  matching subexpression
     ;;that  matches  the  variable  name.  We  need  to  remember  that
     ;;`regexp-opt'  with second  operand  "symbol"  generates a  single
     ;;matching  subexpression  "\\(  ...   \\)"   and  uses  a  lot  of
     ;;non-matching subexpressions "\\(?: ... \\)".
     2)

;;; syntax definitions

    ;; (define-syntax  (the-macro stx) ---)
    ;; (define-syntax* (the-macro stx) ---)
    ;; (define-inline  (<the-macro> . args) ---)
    ;; (define-syntax  the-macro ---)
    ;; (define-syntax* the-macro ---)
    ;; (define-syntax-rule (the-macro ---) ---)
    (nil ;;"Macros"
     "^(define-\\(syntax\\(\\|\\*\\|-rule\\)\\|inline\\)\\s-+(?\\(\\sw+\\)" 3)

    ;; (define-generic-definer  the-macro ---)
    ;; (define-generic*-definer the-macro ---)
    (nil ;;"Macros"
     "^(define-generic\\*?-definer\\s-+\\(\\sw+\\)" 1)

    ;; (define-argument-validation (name ---) ---)
    (nil ;;"Argument Validations"
     "^(define-argument-validation\\s-+(\\(\\sw+\\)" 1))

  "Customise imenu for Chez Scheme mode.

For details on how to use it see `imenu-generic-expression'.")


;;;; file templates

(defun chezscheme-file-template-library nil
  "Return a string representing a Chezscheme library template."
  (concat "
\
\#!r6rs
\(library \(" (chezscheme-p-stripped-file-name) "\)
  \(export\)
  \(import \(chezscheme\)\)

\
;;;; code


\
;;;; done

\#| end of library |\# )

;;; end of file
"))

;;; --------------------------------------------------------------------

(defun chezscheme-file-template-test nil
  "Return a string representing a Chez Scheme test program template."
  (concat "
\
\#!r6rs
\(import \(chezscheme\)
  \(checks\)\)

\(check-set-mode! 'report-failed\)
\(check-display \"*** testing Chez Scheme libraries: stuff\\n\"\)

\
\(parametrise ((check-test-name	'base))

  \(check
      \(let \(\)
        #f\)
    => #f\)

  \#t\)

\
;;;; done

\(check-report\)

;;; end of file
"))

;;; --------------------------------------------------------------------

(defun chezscheme-file-template-program nil
  "Return a string representing a Chez Scheme program template."
  (concat "
\
#!r6rs
\(import \(chezscheme\)\)

\
;;;; code


\
;;;; done

;;; end of file
"))

;;; --------------------------------------------------------------------

(defconst chezscheme-templates-alist
  `(("library"		. chezscheme-file-template-library)
    ("test"		. chezscheme-file-template-test)
    ("program"		. chezscheme-file-template-program))
  "Alist of Chez Scheme file templates used by `chezscheme-auto-insert-template'.")

;;; --------------------------------------------------------------------

(defconst chezscheme-p-software-license-GPL3 "
This  program  is free  software:  you  can redistribute  it
and/or modify it  under the terms of the  GNU General Public
License as published by the Free Software Foundation, either
version  3 of  the License,  or (at  your option)  any later
version.

This  program is  distributed in  the hope  that it  will be
useful, but  WITHOUT ANY WARRANTY; without  even the implied
warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
PURPOSE.   See  the  GNU  General Public  License  for  more
details.

You should  have received a  copy of the GNU  General Public
License   along   with    this   program.    If   not,   see
<http://www.gnu.org/licenses/>.
"
  "The GPL3 preamble to put in the head of source code files.")

;;; --------------------------------------------------------------------
;;; customisable functions

(defvar chezscheme-default-coding-system
  'utf-8-unix
  "Customisable file coding system.")

(defvar chezscheme-insertions-copyright-owner-full-name-function
  #'(lambda ()
      user-full-name)
  "Customisable reference to function returning the copyright owner full name.

Reference      to     function      to     be      invoked     by
`chezscheme-compose-file-header' whenever a copyright owner full name
is   needed.   The   default   returns  the   current  value   of
`user-full-name'.

Notice that the function must be set with:

  (setq chezscheme-insertions-copyright-owner-full-name-function
    #'(lambda () ...))

and called with:

  (funcall chezscheme-insertions-copyright-owner-full-name-function)
")

(defvar chezscheme-insertions-copyright-owner-email-function
  #'(lambda ()
      user-mail-address)
  "Customisable reference to function returning the copyright year.

Reference      to     function      to     be      invoked     by
`chezscheme-compose-file-header'  whenever   a  copyright   email  is
needed.    The    default   returns   the   current    value   of
`user-mail-address'.

Notice that the function must be set with:

  (setq chezscheme-insertions-copyright-owner-email-function
    #'(lambda () ...))

and called with:

  (funcall chezscheme-insertions-copyright-owner-email-function)
")

(defvar chezscheme-file-header-part-of-collection
  '(("chezscheme"	. "Chez Scheme"))
  "Completions alist for the \"Part of:\" file header.

It is used by  `chezscheme-compose-file-header'.  The keys and values
must be strings.")

;;; --------------------------------------------------------------------

(defun chezscheme-compose-file-header (&optional INDENTATION_PREFIX)
  "Return the header of a source code file.

The argument INDENTATION_PREFIX is a prefix added to the inserted
text, it should the opening of a line comment."
  (when (not INDENTATION_PREFIX)
    (setq INDENTATION_PREFIX (read-string "Header indentation prefix: ")))
  (let ((PARTOF			(let* ((THING  (completing-read "Part of: " chezscheme-file-header-part-of-collection))
				       (PARTOF (assoc THING chezscheme-file-header-part-of-collection)))
				  (if PARTOF
				      (cdr PARTOF)
				    THING)))
	(CREATION_DATE		(format-time-string "%a %b %e, %Y"))
	(COPYRIGHT_OWNER	(funcall chezscheme-insertions-copyright-owner-full-name-function))
	(COPYRIGHT_YEAR		(format-time-string "%Y"))
	(DESCRIPTION		(read-string "One line description: ")))
    (chezscheme-p-add-prefix-to
     (concat "\nPart of: "	PARTOF
	     "\nContents: "	DESCRIPTION
	     "\nDate: "		CREATION_DATE
	     "\n\nAbstract\n\n\n\nCopyright (C) " COPYRIGHT_YEAR " " COPYRIGHT_OWNER
	     " <" (funcall chezscheme-insertions-copyright-owner-email-function) ">\n"
	     chezscheme-p-software-license-GPL3) INDENTATION_PREFIX)))

(defun chezscheme-p-stripped-file-name nil
  (let ((FILENAME (buffer-file-name (current-buffer))))
    (and (eq FILENAME nil) (setq FILENAME (read-string "File name: ")))
    (file-name-sans-extension (file-name-nondirectory FILENAME))))

(defun chezscheme-auto-insert-template ()
  "Insert a Chez Scheme template file.

This function  is meant to  be associated to the  file extensions
.sps and .sls as follows:

   (setq auto-insert-alist
     (append auto-insert-alist
             '((\"\\.chez\\.\\(sls\\|sps\\)\\'\" . chezscheme-auto-insert-template))))

"
  (interactive)
  (insert (concat ";;; -*- coding: " (symbol-name chezscheme-default-coding-system) "  -*-\n"))
  (insert (chezscheme-compose-file-header ";;;"))
  (insert (let ((funcname (cdr (assoc (completing-read "Type of file: " chezscheme-templates-alist nil t "library")
				      chezscheme-templates-alist))))
	    (funcall funcname)))
  (goto-char (point-min))
  (set-buffer-file-coding-system chezscheme-default-coding-system))


;;;; custom indentation functions
;;
;;To understand what is going on read the following:
;;
;;* Documentation of `calculate-lisp-indent' in "lisp-mode.el".
;;
;;* Documentation  of `parse-partial-sexp'  in  the  "elisp" guide,  the
;;  STATE argument of this function appears to be a `parse-partial-sexp'
;;  return value.
;;
;;* Reverse engineer `scheme-indent-function' in "scheme.el", the custom
;;  indent function is invoked by its last form:
;;
;;	(funcall method state indent-point normal-indent)
;;
;;Special note: this works with GNU Emacs 21.3 (first half of 2007).
;;Special note: this works with GNU Emacs 21.4.2 (Fri Oct 12, 2007).
;;Special note: this works with GNU Emacs 22.3.1 (Tue Nov 11, 2008).

;;We  define  a  replacement  for  the  indentation  function.   Such  a
;;replacement must  work with  all of  the following  ways to  issue the
;;command:  `indent-region'   (C-M-\),  `lisp-indent-line'   (tab  key),
;;`indent-sexp' (C-M-q).
;;
;;Recall  that `sexp'  in Emacs  jargon  is an  expression delimited  by
;;parentheses.
;;
;;In the  first two  cases (`indent-region' and  `lisp-indent-line') the
;;custom  indentation  function can  return  a  number representing  the
;;column to  indent a line  to: this function  is invoked once  for each
;;line.
;;
;;But in  the latter  case (`indent-sexp'): if  the return value  of the
;;custom  indentation function  is  a number,  the indentation  function
;;itself is invoked  only once for the first line of  the sexp, and that
;;number is taken as column number to indent each of the following lines
;;in the sexp.
;;
;;If the  behaviour with `indent-sexp' is  not what we want:  we have to
;;make the indentation function return a list of two elements, the first
;;being the  column to indent the  current line to and  the second being
;;the `(point)' at the beginning of the containing sexp.
;;
;;There is  no way inside  the indentation function to  understand which
;;case we are in.
;;
;;Example: for  the `compensate' form we  always return the  list of two
;;arguments: testing reveals that it seems to work.

;;; --------------------------------------------------------------------
;;; helper custom indentation functions

(defun chezscheme-compute-body-indent (state indent-point normal-indent)
  "Compute the default indentation of the body of a Scheme form.

This  function  is meant  to  be  used  as first  operation  when
computing the indent space in Scheme code.  The arguments are the
ones of `scheme-indent-function'.

This function  can be used  in the indent functions  that compute
spaces for specific forms.

Compute the default body indent level for the forms of a sexp and
place the  point at the  first char of  the form to  be indented.
Return  a  cons  pair  holding  the number  of  spaces  for  body
indentation and the point of the containing sexp.

As an example let's say we want to indent:

  (with-compensations
      (compensate ...)
      (compensate ...)
    (this)
    (that))

the indent function should be:

   (defun chezscheme-indent-with-compensations (state indent-point normal-indent)
     (let* ((values (chezscheme-compute-body-indent state indent-point normal-indent))
            (body-indent           (car values))
            (containing-sexp-point (cdr values)))
       (list (if (looking-at \"compensate\")
                 (+ 2 body-indent)
               body-indent)
             containing-sexp-point)))

which using the facilities of this module can be defined as:

   (defun chezscheme-indent-with-compensations (state indent-point normal-indent)
     (chezscheme-indent-by-regexp \"compensate\" body-indent containing-sexp-point))

and it is meant to be used like this:

  (put 'with-compensations
       'scheme-indent-function 'chezscheme-indent-with-compensations)
"
  (let ((containing-sexp-start (elt state 1))
	containing-sexp-point
	containing-sexp-column
        body-indent)
    ;;Move to the  start of containing sexp,  calculate its indentation,
    ;;store its point  and move past the function symbol  so that we can
    ;;use `parse-partial-sexp'.   `lisp-indent-function' guarantees that
    ;;there is at least one word  or symbol character following the open
    ;;paren of the containing sexp.
    (forward-char 1)
    (goto-char containing-sexp-start)
    (setq containing-sexp-point (point))
    (setq containing-sexp-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-sexp-column))
    (forward-char 1) ;Move past the open paren.
    (forward-sexp 1) ;Move to the next sexp.

    ;;Now go back  to the beginning of the line  holding the indentation
    ;;point.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case ()
		    (progn
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;;Point is sitting on the first char of the line.
    (forward-to-indentation 0) ;Move to the first non-blank char.
    (forward-char 1)	       ;Move past the open paren.
    ;;Now point is sitting on first character of sexp.
    (cons body-indent containing-sexp-point)))

(defmacro chezscheme-indent-by-regexp (REGEXP MATCH-INDENT DEFAULT-INDENT)
  "Unhygienic helper macro to compute indentation columns for Scheme forms.

Expand  to a  form that  can be  used to  compute and  return the
indent space for specific forms.

REGEXP  must  be a  regular  expression  that  matches the  first
identifier of the form.

MATCH-INDENT must  be an expression that evaluates  to the number
of spaces to indent if we are looking at REGEXP.

DEFAULT-INDENT must be an expression that evaluates to the number
of spaces to indent if we are *not* looking at REGEXP.

See  the  definition  of `chezscheme-indent-compensate',  and  the
other functions like it, for a usage example."
  `(let* ((values (chezscheme-compute-body-indent state indent-point normal-indent))
	  (body-indent           (car values))
	  (containing-sexp-point (cdr values)))
     (list (if (looking-at ,REGEXP)
	       ,MATCH-INDENT
	     ,DEFAULT-INDENT) containing-sexp-point)))

;;; --------------------------------------------------------------------
;;; indent functions for special forms

(defun chezscheme-indent-to-body (state indent-point normal-indent)
  "Indent function for Scheme forms requiring no indentation.

The arguments STATE, INDENT-POINT and NORMAL-INDENT have the same
meaning of the ones to `scheme-indent-function'.  Indent the body
of a  form to the default  body indent colum; this  is useful for
forms whose arguments are actual bodies."
  (let* ((values (chezscheme-compute-body-indent state indent-point normal-indent))
	 (body-indent           (car values))
	 (containing-sexp-point (cdr values)))
    (list body-indent containing-sexp-point)))

(defun chezscheme-indent-compensate (state indent-point normal-indent)
  "Indent function for the Scheme form `compensate' defined by Nausicaa.

The arguments STATE, INDENT-POINT and NORMAL-INDENT have the same
meaning   of  the   ones  to   `scheme-indent-function'.   Indent
`compensate' forms like this:

  (compensate
      (body)
      (body)
    (with
     (other)
     (other))

to be used as:

  (put 'compensate 'scheme-indent-function 'chezscheme-indent-compensate)

in the Scheme mode hook."
  (chezscheme-indent-by-regexp "\\<with\\>" body-indent (+ 2 body-indent)))

;;; --------------------------------------------------------------------
;;; indentation rules

;;See the documentation of `chezscheme-setup-indentation'.

(defconst chezscheme-indent-r6rs
  '((assertion-violation		. 1)
    (assp				. 1)
    (call-with-bytevector-output-port	. 1)
    (call-with-current-continuation	. 1)
    (call-with-input-file		. 1)
    (call-with-output-file		. 1)
    (call-with-port			. 1)
    (call-with-string-output-port	. 1)
    (call-with-values			. 1)
    (call/cc				. 1)
    (condition				. 0)
    (define-condition-type		. 2)
    (error				. 1)
    (exists				. 1)
    (export				. 0)
    (filter				. 1)
    (find				. 1)
    (fold-left				. 1)
    (fold-right				. 1)
    (for-all				. 1)
    (for-each				. 1)
    (guard				. 1)
    (if					. 2)
    (import				. 1)
    (let-syntax				. 1)
    (let*-syntax			. 1)
    (let-values				. 1)
    (letrec				. 1)
    (letrec*				. 1)
    (letrec-syntax			. 1)
    (library				. 1)
    (map				. 1)
    (memp				. 1)
    (partition				. 1)
    (program				. 1)
    (protocol				. 0)
    (fields				. 0)
    (remp				. 1)
    (syntax-case			. 2)
    (syntax-rules			. 1)
    (syntax-violation			. 1)
    (syntax-violation/internal-error	. 1)
    (unless				. 1)
    (when				. 1)
    (with-exception-handler		. 1)
    (with-syntax			. 1))
  "List of indentation rules for R6RS Scheme forms.")

(defconst chezscheme-indent-chezscheme
  '((assert-signature					. 1)
    (assert-signature-and-return			. 1)
    (begin0						. 1)
    (begin-for-syntax					. 0)
    (catch						. 1)
    (case-define					. 1)
    (case-define/std					. 1)
    (case-define/typed					. 1)
    (case-define/checked				. 1)
    (case-define*					. 1)
    (case-endianness					. 1)
    (case-identifiers					. 1)
    (case-lambda					. 0)
    (case-lambda/std					. 0)
    (case-lambda/typed					. 0)
    (case-lambda/checked				. 0)
    (case-lambda*					. 0)
    (cast-signature					. 1)
    (named-case-lambda					. 1)
    (named-case-lambda/std				. 1)
    (named-case-lambda/typed				. 1)
    (named-case-lambda/checked				. 1)
    (named-case-lambda*					. 1)
    (case-type						. 1)
    (check						. 1)
    (check-ec						. 2)
    (check-for-assertion-violation			. 1)
    (check-for-expression-return-value-violation	. 1)
    (check-for-procedure-argument-violation		. 1)
    (check-for-procedure-signature-argument-violation	. 1)
    (check-for-procedure-signature-return-value-violation . 1)
    (check-for-procedure-argument-consistency-violation	. 1)
    (compensate						. chezscheme-indent-compensate)
    (coroutine						. 1)
    (custom-printer					. 0)
    (define-maker					. 2)
    (define-syntax*					. 1)
    (define-type					. 1)
    (destructor-protocol				. 0)
    (do*						. 2)
    (dolist						. 1)
    (dotimes						. 1)
    (inherit						. 1)
    (internal-body					. 0)
    (internal-define					. 2)
    (define/std						. 1)
    (define/typed					. 1)
    (define/checked					. 1)
    (define/friend					. 1)
    (lambda/std						. 1)
    (lambda/typed					. 1)
    (lambda/checked					. 1)
    (lambda*						. 1)
    (let/std						. 1)
    (let/checked					. 1)
    (let*/std						. 1)
    (let*/checked					. 1)
    (letrec/std						. 1)
    (letrec/checked					. 1)
    (letrec*/std					. 1)
    (letrec*/checked					. 1)
    (let-values/std					. 1)
    (let-values/checked					. 1)
    (let*-values					. 1)
    (let*-values/std					. 1)
    (let*-values/checked				. 1)
    (named-lambda*					. 2)
    (named-lambda					. 2)
    (named-lambda/std					. 2)
    (named-lambda/typed					. 2)
    (named-lambda/checked				. 2)
    (eval-for-expand					. 0)
    (fluid-let-syntax					. 1)
    (format						. 1)
    (hashtable-map-keys					. 1)
    (hashtable-map-entries				. 1)
    (hashtable-for-each-entry				. 1)
    (hashtable-for-each-key				. 1)
    (hashtable-for-all-keys				. 1)
    (hashtable-for-all-entries				. 1)
    (hashtable-exists-key				. 1)
    (hashtable-exists-entry				. 1)
    (hashtable-find-key					. 1)
    (hashtable-find-entry				. 1)
    (hashtable-fold-keys				. 1)
    (hashtable-fold-entries				. 1)
    (make-parameter					. 1)
    (module						. 1)
    (monitor						. 1)
    (non-reinstatable-violation				. 1)
    (concurrently					. 0)
    (parameterize					. 1)
    (parameterise					. 1)
    (parametrise					. 1)
    (public						. 0)
    (protected						. 0)
    (private						. 0)
    (recursion						. 1)
    (receive						. 2)
    (receive/std					. 2)
    (receive/checked					. 2)
    (returnable						. 0)
    (record-type-printer-set!				. 1)
    (constructor					. 1)
    (constructor-signature				. 0)
    (destructor						. 1)
    (super-protocol					. 0)
    (custom-predicate					. 0)
    (type-predicate					. 0)
    (equality-predicate					. 0)
    (comparison-procedure				. 0)
    (hash-function					. 0)
    (try						. 1)
    (unwind-protect					. 1)
    (virtual-method					. 1)
    (seal-method					. 1)
    (with-unwind-protection				. 1)
    (with-unwind-handler				. 1)
    (with-escape-handlers-stack				. 0)
    (with-escape-handler				. 1)
    (with-blocked-exceptions				. 1)
    (with-current-dynamic-environment			. 1))
  "List of indentation rules for Chez Scheme forms.")

(defconst chezscheme-indent-srfi
  '((and-let*				. 1)
    (any				. 1)
    (every				. 1)
    (list-index				. 1))
  "List of indentation rules for SRFI Scheme forms.")

(defconst chezscheme-indent-custom
  ;;FIXME This needs a cleanup.  (Marco Maggi; Wed Jan 7, 2015)
  '((ensure						. 2)
    (exists1						. 1)
    (expression-return-value-violation			. 1)
    (for						. 1)
    (for-all1						. 1)
    (for-each-in-order					. 1)
    (for-each1						. 1)
    (let*-constant-values				. 1)
    (let*-constants					. 1)
    (let*-keywords					. 3)
    (let*-syntax					. 1)
    (let*-values					. 1)
    (let-connectors					. 1)
    (let-constant-values				. 1)
    (let-constants					. 1)
    (let-contract					. 1)
    (let-keywords					. 3)
    (let-optional					. 2)
    (let-optional*					. 2)
    (let-sexp						. 3)
    (let-sexp*						. 3)
    (let-vectors					. 1)
    (let/cc						. 1)
    (letrec*-constants					. 1)
    (letrec*-keywords					. 3)
    (letrec-constants					. 1)
    (letrec-keywords					. 3)
    (loop-upon-list					. 2)
    (make						. 1)
    (make*						. 1)
    (make-from-fields					. 1)
    (make-instance					. 1)
    (map-in-order					. 1)
    (map1						. 1)
    (match						. 1)
    (match-define					. 1)
    (match-define*					. 1)
    (match-let						. 1)
    (match-let*						. 1)
    (method-call					. 1)
    (mk.define-maker					. 2)
    (unwinding-call/cc					. 1)

    (define-syntax-parameter				. 1)
    (syntax-parameterize				. 1)
    (syntax-parameterise				. 1)
    (syntax-parametrise					. 1)

    (open-file-input-port				. 1)
    (raise-non-continuable-standard-condition		. 1)
    (procedure-argument-violation			. 1)
    (procedure-signature-argument-violation		. 1)
    (procedure-signature-return-value-violation		. 1)
    (procedure-arguments-consistency-violation		. 1)
    (r6.make-token-lexer				. 1)
    (rec						. 1)
    (receive-and-return					. 2)
    (receive-and-return/std				. 2)
    (receive-and-return/checked				. 2)
    (syntax-match					. 2)
    (method						. 1)
    (case-method					. 1)
    (method/overload					. 1)
    (method-prototype					. 1)
    (set-identifier-object-spec!			. 1)
    (set-identifier-callable-spec!			. 1)
    (set-identifier-unsafe-variant!			. 1)
    (set-predicate-procedure-argument-validation!	. 1)
    (set-predicate-return-value-validation!		. 1)
    (set-rtd-printer!					. 1)
    (set-rtd-destructor!				. 1)
    (set-struct-type-printer!				. 1)
    (set-record-type-printer!				. 1)
    (set-struct-type-destructor!			. 1)
    (set-record-type-destructor!			. 1)
    (stale-when						. 1)
    (string-for-each					. 1)
    (string-map						. 1)
    (submodule						. 1)
    (syntax-rules*					. 1)
    (syntax-case*					. 2)
    (tagged-binding-violation				. 1)
    (trace-let						. 2)
    (trace-define					. 1)
    (tag-case						. 1)
    (unsafe-cast-signature				. 1)
    (vector-and-fold-left				. 1)
    (vector-and-fold-left*				. 1)
    (vector-and-fold-left/stx				. 1)
    (vector-and-fold-left*/stx				. 1)
    (vector-and-fold-left/with-index			. 1)
    (vector-and-fold-right				. 1)
    (vector-and-fold-right*				. 1)
    (vector-and-fold-right/stx				. 1)
    (vector-and-fold-right*/stx				. 1)
    (vector-and-fold-right/with-index			. 1)
    (vector-any						. 1)
    (vector-every					. 1)
    (vector-exists					. 1)
    (vector-for-all					. 1)
    (vector-fold-left					. 1)
    (vector-fold-right					. 1)
    (vector-find					. 1)
    (vector-fold-left					. 1)
    (vector-fold-left*					. 1)
    (vector-fold-left/stx				. 1)
    (vector-fold-left*/stx				. 1)
    (vector-fold-right					. 1)
    (vector-fold-right*					. 1)
    (vector-fold-right/stx				. 1)
    (vector-fold-right*/stx				. 1)
    (vector-for-each					. 1)
    (vector-map						. 1)
    (virtual-method					. 1)
    (while						. 1)
    (until						. 1)
    (with-aliases					. 1)
    (with-amb-exhaustion-handler			. 1)
    (with-arguments-validation				. 2)
    (with-dangerous-arguments-validation		. 2)
    (with-compensations					. 0)
    (with-compensations/on-error			. 0)
    (with-compensation-handler				. 1)
    (with-general-c-strings				. 1)
    (with-general-c-strings/false			. 1)
    (with-general-c-buffers				. 1)
    (with-general-c-buffers/false			. 1)
    (with-connector-values				. 1)
    (with-connectors					. 1)
    (with-deferred-exceptions-handler			. 1)
    (with-keywords					. 1)
    (with-implicits					. 1)
    (with-inferior-process-environment			. 1)
    (with-label-shadowing				. 1)
    (with-outer-contracts				. 1)
    (with-process-environment				. 1)
    (with-shared-object					. 1)
    (with-slots						. 1)
    (with-tagged-arguments-validation			. 2)
    (with-true-property					. 1)
    (with-vectors					. 1)
    (with-record-accessors				. 2)
    (with-record-mutators				. 2)
    (with-record-accessors/mutators			. 2)
    (with-tags						. 1)
    (with-fields					. 1)
    (with-namespace					. 1)
    (with-reference-counting				. 1)
    (with-result					. 0)
    (handler-case					. 1)
    (ignore-errors					. 0)
    (handler-bind					. 1)
    (restart-case					. 1)
    (with-condition-restarts				. 1)
    (raise-undefined-restart-error			. 1)
    (signal-restarts-control-error			. 1)
    (with-return-to-signal-on-unhandled-exception	. 0)
    (let-sexp-variables					. 1))
  "List of indentation rules for custom Scheme forms.")

;;; --------------------------------------------------------------------

(defun chezscheme-setup-indentation ()
  "Configure custom indentation for Scheme mode.

Custom indentation  (see \"progmodes/scheme.el\" for  the default
indentation);  the number  argument  of the  `put'  forms is  the
number of forms that have deep  indentation, the rest of the form
will have small indentation.  Think about `do':

 (do ((i 0 (1+ i)))
     ((> i 4))
   (do-something))

it is defined like this:

 (put 'do 'scheme-indent-function 2)

because we  want the first 2  forms to be deeply  indented, while
the others have to have small indentation.
"
  (interactive)
  (make-local-variable 'scheme-indent-function)
  (dolist (list-of-indent-spec (list chezscheme-indent-r6rs
				     chezscheme-indent-chezscheme
				     chezscheme-indent-srfi
				     chezscheme-indent-custom))
    (dolist (indent-spec list-of-indent-spec)
      (put (car indent-spec) 'scheme-indent-function (cdr indent-spec)))))


;;;; major mode definition

;;;###autoload
(define-derived-mode chezscheme-mode
  scheme-mode "Chez Scheme"
  "Major mode for Chez Scheme source code.
\\{chezscheme-mode-map}"
  (setq scheme-program-name "petite-chez")
  (setq imenu-generic-expression chezscheme-imenu-generic-expression)
  (chezscheme-setup-font-locking)
  (chezscheme-setup-indentation))

(provide 'chezscheme)
;;; chezscheme.el ends here
