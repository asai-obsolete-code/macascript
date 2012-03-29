
;; (ql:quickload :alexandria)
;; (ql:quickload :cl-match)
;; (ql:quickload :anaphora)


;; (load "maca-core")
;; (load "fundamentals")
;; (load "functions")
;; (load "iterations")
;; (load "conditions")
;; (load "ops")
;; (load "literals")
;; (load "misc")

(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora))

;;(proclaim '(optimize (debug 3)))
(declaim (optimize (debug 3)))
(in-package :maca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewriter

(defparameter *reserved*
  '(break case catch continue default delete
    do else finally for function if in instanceof
    new return switch this throw try typeof var
    void while with 
    ;; future reserved
    abstract boolean byte char class const debugger
    double enum export extends final float goto implements
    import int interface long native package private protected
    public short static super synchronized throws transient volatile))

(defparameter *aliases*
  `((t . true)
    (on . true)
    (yes . true)
    (off . false)
    (no . false)
    (null . undefined)
    (not . !)
    (and . &&)
    (or . |\|\||)
    (and= . &=)
    (not= . ^=)
    (or= . |\|=|)
    (@ . this)
    (newline . #\Newline)
	(indent . #\Tab)
    (space   . #\Space)
    (colon . #\:)
    (period . #\.)
    (semicolon . #\;)
    (comma . #\,)
    (lbrace . #\{)
    (rbrace . #\})
    (lbracket . #\[)
    (rbracket . #\])
    (lparen . #\()
    (rparen . #\))
    (comment . \\))
  "alist for aliasing the constants such as \"on\" \"yes\".")

(defparameter *recompile-compiler* nil)
(defparameter *customs* nil)
(defparameter *conditions* nil)
(defparameter *core-ops* nil)
(defparameter *fundamentals* nil)
(defparameter *iteraters* nil)
(defparameter *objects* nil)
(defparameter *fundamentals* nil)
(defparameter *miscellaneous* nil)
(defparameter *functions* nil)

;; core macros

(defun show-patterns ()
  (append *customs*
		  *core-ops*
		  *functions*
		  *iteraters*
		  *conditions*
		  *objects*
		  *fundamentals*
		  *miscellaneous*))


(defstruct closure
  (indentation 0)
  (arguments nil)
  (variables nil)
  (initializations nil)
  (inline-lambda nil))

(defmacro recompile ()
  `(defun m-compile (env lst &key return)
	 (macrolet ((rewrite (name is-value &rest args)
				  `(values (,name env return ,@args) ,is-value ',name)))
	   (match lst
		 ,@(copy-tree *customs*)
		 ,@(copy-tree *core-ops*)
		 ,@(copy-tree *functions*)
		 ,@(copy-tree *iteraters*)
		 ,@(copy-tree *conditions*)
		 ,@(copy-tree *objects*)
		 ,@(copy-tree *fundamentals*)
		 ,@(copy-tree *miscellaneous*)))))

(defmacro defmaca (name-or-name-and-options args &body body)
  (if (symbolp name-or-name-and-options)
	  `(defmaca-builder (,name-or-name-and-options) ,args ,body)
	  `(defmaca-builder ,name-or-name-and-options ,args ,body)))

(defmacro defmaca-builder ((name &key (return (gensym "return")) (environment (gensym "e"))) args body)
  (declare (ignorable return))
  (with-gensyms (definition)
    `(progn
	   (defun ,name (,environment ,return ,@args)
		 (declare (ignorable ,return ,environment))
		 (symbol-macrolet ((+cl+ (car ,environment))
						   (+indentation+ (closure-indentation (car ,environment)))
						   (+variables+ (closure-variables (car ,environment)))
						   (+initializations+ (closure-initializations (car ,environment)))
						   (+inline-lambda+ (closure-inline-lambda (car ,environment)))
						   (+arguments+ (closure-arguments (car ,environment))))
		   (let ((,definition (progn ,@body)))
			 (m-compile ,environment ,definition))))
	   ,@(if *recompile-compiler*
			 `((format t "recompiling m-compile ...")
			   (recompile)
			   (format t "done.~%")))
	   #',name)))

(defun maca-format (value)
  (format t "~{~a~}~%"
		  (mapcar #'(lambda (arg)
					  (let ((token (or (cdr (assoc arg *aliases*)) arg)))
						(typecase token
						  (null "")
						  (symbol (let ((str (symbol-name token)))
									(if (every #'(lambda (c) (or (not (both-case-p c))
																 (upper-case-p c)))
											   str)
										(string-downcase str)
										str)))
						  (t token))))
				  (remove 'compiled (flatten value)))))

(defmacro maca-compile (&body body)
  (if (typep (car body) 'atom)
	  `(m-compile (list (make-closure)) ',@body)
	  `(m-compile (list (make-closure)) '(global (,@body)))))

(defmacro maca (&body script)
  `(maca-format (maca-compile ,@script)))