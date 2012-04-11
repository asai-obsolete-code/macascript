(in-package :maca)


(defun indents (n)
  (loop for i below n collect 'indent))

(defparameter *miscellaneous*
  '( ;;for the evaluation of single atom at the top level
	((type string str)
	 (values `(compiled ,(format nil "\"~a\"" str)) t 'string))
	((list 'newline-and-indent)
	 (values
	  `(compiled newline ,@(indents *indentation*))
	  t 'newline-and-indent))
	((type atom val) 
	 (values (m-compile env `(glue ,val)) t 'atom))

	;; ((list* 'sentence sentence) (rewrite m-js-sentence sentence))
	;; ((list* 'non-sentence sentence) (rewrite m-js-non-sentence sentence))

	((list* (type atom op) arguments)
	 (multiple-value-bind (lmb found-cl) (search-lambda op env)
	   (if lmb
		   (rewrite m-inline-function-call arguments lmb found-cl)
		   (rewrite m-function-call op arguments))))
	((list) (values nil t 'null))
	((list* sentences) (rewrite m-sentences sentences))))



(defmaca (m-function-call :environment env :is-value t) (op args)
  ;; (if (some #'(lambda (arg) (eq (car arg) 'with-cc)))
  ;; 	  (let ((actual-args 
  ;; 			 (mapcar
  ;; 			  #'(lambda (arg)
  ;; 				  (if (eq (car arg) 'with-cc)
  ;; 					  (with-gensyms (value-arg)
  ;; 						(m-compile env arg)
  ;; 						value-arg)
  ;; 					  arg)) args)))
  ;; 		(with-set-temps-in-list (env actual-args temps)
  ;; 		  `(glue ,op (paren (comma ,@temps))))

	  ;; uses continuation-passing-style
	  
	  (with-set-temps-in-list (env args temps)
		`(glue ,op (paren (comma ,@temps))))))






(defmaca (m-inline-function-call :environment env :return return-as)
	(args lmb found-cl)
  (destructuring-bind (lambda-list . body) lmb
	(let ((temps (mapcar #'(lambda (x) (declare (ignore x)) (gensym "tmp"))
						 lambda-list))
		  (copying-script nil))
	  (loop
		 for arg in args
		 for temp in temps
		 for param in lambda-list
		 do (setf body (subst temp param body))
		 do (push temp (closure-variables found-cl))
		 do (push `(= ,temp ,arg) copying-script))
	  (break "~a" return-as)
	  (if return-as 
		  `(,@copying-script
			,@(1-or-2-line-set-temp body return-as))
		  (with-set-temp env (body) body)))))

(defparameter *non-sentence-ops*
  '(var if for switch while do))

;; (defmaca m-js-sentence (sent)
;;   `(glue (newline-and-indent) ,sent semicolon))
;; (defmaca m-js-non-sentence (sents)
;;   `(glue (newline-and-indent) ,sent))
(defmaca m-sentences (sents)
  `(glue ,@(mapcar
			#'(lambda (sent) 
				`(glue (newline-and-indent) ,sent semicolon))
			sents)))
  ;; `(glue ,@sents))

(recompile)
(setf *recompile-compiler* t)