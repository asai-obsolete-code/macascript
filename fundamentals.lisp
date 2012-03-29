(in-package :maca)

;; --------------------------------
;; fundamentals

;; these operators are just meant to be used by the compiler
;; don't use it
(defparameter *fundamentals*
  '(((list* 'compiled clauses)          (values lst nil 'compiled))
	((list* 'glue clauses)              (rewrite m-glue nil clauses))
	((list 'paren clause)               (rewrite m-paren nil clause))
	((list 'bracket clause)             (rewrite m-bracket nil clause))
	((list* 'comma clauses)             (rewrite m-comma t clauses))
	((list 'blk clause)                 (rewrite m-block nil clause))
	((list '// str)                     (rewrite m-comment nil str))))

(defmaca (m-glue :environment env) (args)
  `(compiled 
	,@(mapcar #'(lambda (arg)
				  (cond
					((consp arg) (m-compile env arg))
					((and
					  (symbolp arg)
					  (char= (aref (symbol-name arg) 0) #\@))
					 (m-compile env `(this > ,(make-symbol (subseq (symbol-name arg) 1)))))
					((stringp arg)
					 (format nil "\"~a\"" arg))
					(t arg)))
			  args)))

(defmaca m-paren (arg)
  `(glue lparen ,arg rparen))

(defmaca m-bracket (arg)
  `(glue lbracket ,arg rbracket))

(defmacro with-indent (env &body body)
  (with-gensyms (contents)
	`(let ((,contents nil))
	   (incf +indentation+)
	   (setf ,contents (m-compile ,env (progn ,@body)))
	   (decf +indentation+)
	   ,contents)))

(defmaca (m-block :environment env) (arg)
  `(glue lbrace 
		 ,(with-indent env arg)
		 (newline-and-indent)
		 rbrace))

(defmaca m-comma (args)
  `(glue ,@(mapcan
			#'(lambda (arg) (list arg 'comma))
			(butlast args))
		 ,@(last args)))

(defmaca m-comment (string)
  `(compiled ,(format nil "~t/* ~a */~%" string)))
