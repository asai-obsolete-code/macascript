(ql:quickload :alexandria)
(ql:quickload :cl-match)
(ql:quickload :anaphora)

(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora))

(proclaim '(optimize (debug 3) (safety 3)))
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

(defparameter *recompile-compiler* nil)

(defstruct closure
  (indentation 0)
  (arguments nil)
  (variables nil)
  (initializations nil)
  (inline-lambda nil))

(defmacro recompile ()
  `(defun m-compile (s env need-value lst)
	 (macrolet ((rewrite (name &rest args)
				  `(values (,name s env need-value ,@args) ',name)))
	   (match lst
		 ,@(copy-tree *customs*)
		 ,@(copy-tree *core-ops*)
		 ,@(copy-tree *functions*)
		 ,@(copy-tree *iteraters*)
		 ,@(copy-tree *conditions*)
		 ,@(copy-tree *objects*)
		 ,@(copy-tree *fundamentals*)
		 ,@(copy-tree *miscellaneous*)))))

;; you can perform some bad behavior on "env". it is intentional
(defmacro defmaca (name args &body body)
  (with-gensyms (s definition)
    `(progn
	   (defun ,name (,s env need-value ,@args)
		 (symbol-macrolet ((+cl+ (car env))
						   (+indentation+ (closure-indentation (car env)))
						   (+variables+ (closure-variables (car env)))
						   (+initializations+ (closure-initializations (car env)))
						   (+inline-lambda+ (closure-inline-lambda (car env)))
						   (+arguments+ (closure-arguments (car env))))
		   (let* ((next-need-value nil)
				  (,definition (progn ,@body)))
			 (m-compile ,s env next-need-value ,definition))))
	   ,@(if *recompile-compiler*
			 `((format t "recompiling m-compile ...")
			   (recompile)
			   (format t "done.~%"))))))

(defmacro maca (&body body)
  `(progn
     (multiple-value-bind (value type)
		 ,(if (typep (car body) 'atom)
			  `(m-compile t (list (make-closure)) t ',@body)
			  `(m-compile t (list (make-closure)) t '(global ,@body)))
       (declare (ignore value))
       (format t "~%")
       type)))
