
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

(proclaim '(optimize (debug 3)))
(in-package :maca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defun uniquify (lst &key (test #'eq))
  (labels ((rec (unique rest)
			 (if rest
				 (if (member (car rest) unique :test test)
					 (rec unique (cdr rest))
					 (rec (cons (car rest) unique) (cdr rest)))
				 unique)))
    (rec nil lst)))

(defun not-uniquep (lst &key (test #'eq))
  (if lst
      (or (member (car lst) (cdr lst) :test test)
		  (not-uniquep (cdr lst) :test test))
      nil))

(defun not-unique (lst &key (test #'eq))
  (aif (not-uniquep lst :test test)
       (cons (car it) (not-unique (cdr lst) :test test))))

(defun uniquep (lst &key (test #'eq))
  (not (not-uniquep lst :test test)))

(defmacro rmapcar (lst &body fn)
  (when (cdr fn) (error "not valid rmapcar"))
  `(mapcar ,@fn ,lst))

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

(defparameter *recompile-compiler* t)
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
  `(defun m-compile (s env lst &key temp need-value)
	 (macrolet ((rewrite (name &rest args)
				  `(values (,name s env temp need-value ,@args) ',name)))
	   (if s
		   (match lst
			 ,@(copy-tree *customs*)
			 ,@(copy-tree *core-ops*)
			 ,@(copy-tree *functions*)
			 ,@(copy-tree *iteraters*)
			 ,@(copy-tree *conditions*)
			 ,@(copy-tree *objects*)
			 ,@(copy-tree *fundamentals*)
			 ,@(copy-tree *miscellaneous*))
		   (with-output-to-string (s)
			 (match lst
			   ,@(copy-tree *customs*)
			   ,@(copy-tree *core-ops*)
			   ,@(copy-tree *functions*)
			   ,@(copy-tree *iteraters*)
			   ,@(copy-tree *conditions*)
			   ,@(copy-tree *objects*)
			   ,@(copy-tree *fundamentals*)
			   ,@(copy-tree *miscellaneous*)))))))

(defmacro defmaca (name-or-name-and-options args &body body)
  (if (symbolp name-or-name-and-options)
	  `(defmaca-builder (,name-or-name-and-options) ,args ,body)
	  `(defmaca-builder ,name-or-name-and-options ,args ,body)))

(defmacro defmaca-builder ((name &key
								 (stream (gensym "s"))
								 (environment (gensym "e"))
								 (temporary-return (gensym "tmp")))
						   args body)
  (with-gensyms (definition)
    `(progn
	   (defun ,name (,stream ,environment ,temporary-return need-value ,@args)
		 (declare (ignorable need-value))
		 (symbol-macrolet ((+cl+ (car ,environment))
						   (+indentation+ (closure-indentation (car ,environment)))
						   (+variables+ (closure-variables (car ,environment)))
						   (+initializations+ (closure-initializations (car ,environment)))
						   (+inline-lambda+ (closure-inline-lambda (car ,environment)))
						   (+arguments+ (closure-arguments (car ,environment))))
		   (let* ((next-need-value nil)
				  (,definition (progn ,@body)))
			 (declare (ignorable next-need-value))
			 (m-compile ,stream ,environment ,definition ,temporary-return next-need-value))))
	   ,@(if *recompile-compiler*
			 `((format t "recompiling m-compile ...")
			   (recompile)
			   (format t "done.~%")))
	   #',name)))

(defmacro maca (&body body)
  `(progn
     (multiple-value-bind (value type)
		 ,(if (typep (car body) 'atom)
			  `(m-compile t (list (make-closure)) ',@body)
			  `(m-compile t (list (make-closure)) '(global ,@body)))
       (declare (ignore value))
       (format t "~%")
       type)))
