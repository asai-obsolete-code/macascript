(in-package :maca)

(defun value (arg)
  (list 'value arg))

(defmacro value-if (need-value arg)
  `(if ,need-value
	   (list 'value ,arg)
	   ,arg))


;; --------------------------------
;; fundamentals

;; these operators are just meant to be used by the compiler
;; don't use it
(defparameter *fundamentals*
  '(;;((list 'glue 'nil 'semicolon 'newline) (rewrite m-glue nil))
	((list* 'glue clauses)              (rewrite m-glue clauses))
	((list 'paren clause)               (rewrite m-paren clause))
	((list 'bracket clause)             (rewrite m-bracket clause))
	((list* 'comma clauses)             (rewrite m-comma clauses))
	((list 'blk clause)                 (rewrite m-block clause))
	((list 'value arg)                  (rewrite m-need-value arg))
	((list '// str)                     (rewrite m-comment str))))

(defmaca (m-glue :stream s :environment env) (args)
  (loop for arg in args do
	   (let ((arg (or (cdr (assoc arg *aliases*)) arg)))
		 (typecase arg
		   (null nil)
		   (cons (m-compile s env arg))
		   (string (format s "\"~a\"" arg))
		   (symbol (let ((str (symbol-name arg)))
					 (cond
					   ((char= (aref str 0) #\@) (m-compile s env arg))
					   ((every #'(lambda (c) (or (not (both-case-p c))
												 (upper-case-p c))) str)
						(write-string (string-downcase str) s))
					   (t (write-string str s)))))
		   (t (format s "~a" arg))))))

(defmaca m-paren (arg)
  `(glue lparen ,arg rparen))

(defmaca m-bracket (arg)
  `(glue lbracket ,arg rbracket))

(defmaca m-block (arg)  
  `(glue lbrace newline ,arg rbrace))

(defmaca m-comma (args)
  `(glue ,@(mapcan
			#'(lambda (arg) (list arg 'comma))
			(butlast args))
		 ,(if need-value
			  `(value ,@(last args))
			  (car (last args)))))

(defmaca (m-comment :stream s) (string)
  (format s "/* ~a */~%" string))

(defmaca m-need-value (arg)
  (setf next-need-value t)
  arg)
