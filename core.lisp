
(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora :cl-ppcre)
  (:export "MACA"
		   "M-COMPILE"
		   "MACA-COMPILE"
		   "MACA-FORMAT"
		   "RECOMPILE"
		   "PATTERNS"
		   "*RECOMPILE-COMPILER*"))

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
    (quote . #\')
    (lbrace . #\{)
    (rbrace . #\})
    (lbracket . #\[)
    (rbracket . #\])
    (lparen . #\()
    (rparen . #\))
    (comment . \\))
  "alist for aliasing the constants such as \"on\" \"yes\".")

(defmacro define-parameters (&rest names)
  `(progn ,@(loop for name in names collect
				 `(defparameter ,name nil))))

(define-parameters
	*recompile-compiler* *ops* 
  *customs* *conditions* *conditions2* 
  *fundamentals* *iteraters* *objects* *fundamentals* 
  *miscellaneous* *functions* *cont* *class* *delay*)

;; core macros

(defun patterns ()
  (append *ops*
		  *fundamentals*
		  *functions*
		  *iteraters*
		  *conditions*
		  *conditions2*
		  *objects*
		  *class*
		  *delay*
		  *cont*
		  *customs*
		  *miscellaneous*))

(defstruct closure "this represents the current top-level closure in
the currently processing javascript context.

arguments :: not currently used.

variables :: list of symbols. Variables declared with 'var' are stored
in this list.

initializations :: list of assignment script. When a variable is
declared with 'var' and it has the initialization argument, then the
assignment script will bepushed into this list.

inline-lambda :: list of maca lambda lists. When an inline-function is
defined, the lambda list will be stored here.

function-lambda :: not currently used. (meant to store which keyword is 
defined as a function)

"
  (arguments nil)						; unused now
  (variables nil)						;
  (initializations nil)
  (inline-lambda nil)
  (function-lambda nil)					; unused now
  (indentation 0)
  (type-assertions nil)
  (static-classes nil))

(defmacro recompile ()
  `(defun m-compile (env lst &key return)
	 (macrolet 
		 ((rewrite (name &rest args)
			(with-gensyms (definition is-value-option)
			  `(multiple-value-bind (,definition ,is-value-option)
				   (,name env return ,@args)
				 (values ,definition ,is-value-option ',name)))))
	   (match lst ,@(patterns)))))

(defmacro defmaca (name-or-name-and-options args &body body)
  (if (symbolp name-or-name-and-options)
	  `(defmaca-builder (,name-or-name-and-options) ,args ,body)
	  `(defmaca-builder ,name-or-name-and-options ,args ,body)))

(defmacro defmaca-builder ((name &key
								 (return (gensym "RETURN"))
								 (environment (gensym "ENV"))
								 (is-value nil)
								 (inherit-return-value nil))
						   args body)
  (declare (ignorable return is-value))
  (with-gensyms (definition is-value-option)
    `(progn
	   (defun ,name (,environment ,return ,@args)
		 (declare (ignorable ,return ,environment
							 ,@(loop for arg in args
								  if (and (symbolp arg)
										  (not (member arg '(&key &rest &optional))))
								  collect arg
								  else
								  if (listp arg)
								  collect (car arg))))
		 (let ((+cl+ (car ,environment)))
		   (with-slots ((+variables+ variables)
						(+initializations+ initializations)
						(+inline-lambda+ inline-lambda)
						(+arguments+ arguments)
						(+function-lambda+ function-lambda)) +cl+
			 (multiple-value-bind (,definition
								   ,is-value-option)
				 (progn ,@body)
			   (declare (ignorable ,is-value-option))
			   (values (m-compile ,environment ,definition
								  ,@(if inherit-return-value
										`(:return ,return)
										nil))
					   ,(or is-value is-value-option))))))
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
						  (symbol
						   (let ((str (symbol-name token)))
							 (let ((chunks
									(split "-"
										   (if (every
												#'(lambda (c)
													(or (not (both-case-p c))
														(upper-case-p c)))
												str)
											   (string-downcase str)
											   str))))
							   (format nil "~a~{~a~}"
									   (car chunks)
									   (mapcar #'string-capitalize
											   (cdr chunks))))))
						  (t token))))
				  (remove 'compiled
						  (flatten value)))))

(defmacro maca-compile (&body body)
  `(progn 
	 ;;(setf *indentation* 0)
	,(if (every (of-type 'atom) body)
		`(m-compile (list (make-closure)) '(comma ,@body))
		`(m-compile (list (make-closure)) '(global ,@body)))))

(defmacro maca (&body script)
  `(maca-format (maca-compile ,@script)))