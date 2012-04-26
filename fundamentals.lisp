(in-package :maca)

;; --------------------------------
;; fundamentals

;; these operators are just meant to be used by the compiler
;; don't use it
(defparameter *fundamentals*
  '(((list) (values nil t 'null))
	((list* 'compiled clauses)          (values lst t 'compiled))
	((list* 'glue clauses)              (rewrite m-glue clauses))
	((list 'paren clause)               (rewrite m-paren clause))
	((list 'stringify clause)           (rewrite m-stringify clause))
	((list 'bracket clause)             (rewrite m-bracket clause))
	((list* 'comma clauses)             (rewrite m-comma clauses))
	((list '// str)                     (rewrite m-comment str))
	((list 'blk clause)                 (rewrite m-block clause))))

(defmaca (m-glue :environment env :return tmp :is-value t) (args)
  (let ((body `(compiled 
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
						  args))))
	(if tmp
		`(var ,tmp ,body)
		body)))

(defmaca (m-paren :is-value t) (arg)
  `(glue lparen ,arg rparen))

(defmaca (m-stringify :is-value t) (arg)
  `(glue quote ,arg quote))

(defmaca (m-bracket :is-value t) (arg)
  `(glue lbracket ,arg rbracket))

(defmaca (m-block :environment env) (arg)
  `(glue lbrace
		 (// ,(format nil "indentation level: ~a"
					  (env-indents env)))
		 ,@(let (contents)
				(incf (closure-indentation (car env)))
				(setf contents (m-compile env `(sentences ,@arg)))
				(decf (closure-indentation (car env)))
				contents)
		 (newline-and-indent)
		 rbrace))

(defmaca (m-comma :is-value t) (args)
  `(glue ,@(mapcan
			#'(lambda (arg) (list arg 'comma))
			(butlast args))
		 ,@(last args)))

(defmaca (m-comment :is-value t) (string)
  `(compiled ,(format nil "~t/* ~a */" string)))
