(in-package :maca)


(defun indents (n)
  (loop for i below n collect 'indent))


(defparameter *customs* nil)
(defparameter *miscellaneous*
  '( ;;for the evaluation of single atom at the top level
	((type string str) (values `(compiled ,(format nil "\"~a\"" str)) t 'string))
	((list 'newline-and-indent)
	 (values
	  `(compiled newline ,@(indents *indentation*))
	  t 'newline-and-indent))
	((type atom val) (values (m-compile env `(glue ,val)) t (type-of val)))
	
	((list* (type atom op) arguments)
	 (multiple-value-bind (lmb found-cl) (search-lambda op env)
	   (if lmb
		   (rewrite m-inline-function-call nil arguments lmb found-cl)
		   (rewrite m-function-call t op arguments))))
	((list) (values nil t 'null))
	((list* sentences)                        (rewrite m-sentences nil sentences))))

(defmaca (m-function-call :environment env) (op args)
  (with-set-temps-in-list (env args temps)
	`(glue ,op (paren (comma ,@temps)))))


(defmaca (m-inline-function-call :environment env :return return-as)
	(args lmb found-cl)
  (destructuring-bind (lambda-list . body) lmb
	(let ((temps (mapcar #'(lambda (x) (declare (ignore x)) (gensym "tmp")) lambda-list))
		  (copying-script nil))
	  (loop
		 for arg in args
		 for temp in temps
		 for param in lambda-list
		 do (setf body (subst temp param body))
		 do (push temp (closure-variables found-cl))
		 do (push `(= ,temp ,arg) copying-script))
	  `(,@copying-script
		,@(if return-as 
			  (1-or-2-line-set-temp body return-as)
			  body)))))

(defparameter *non-sentence-ops*
  '(var if for switch while do))

(defmaca m-sentences (sents)
  `(glue ,@(mapcar
			#'(lambda (sent) 
				;; (ifmatch (when (member keyword *non-sentence-ops*)
				;; 		   (list* keyword _)) sent
				;; 	sent
				  `(glue (newline-and-indent) ,sent semicolon));)
			sents)))

(recompile)
(setf *recompile-compiler* t)