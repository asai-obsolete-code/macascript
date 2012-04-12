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

	((list* '--- (type atom op) (as chain-arguments (list* (list* _))))
	 (rewrite m-chain-function-call op chain-arguments))

	((list* (type atom op) arguments)
	 (multiple-value-bind (lmb found-cl) (search-lambda op env)
	   (if lmb
		   (rewrite m-inline-function-call arguments lmb found-cl)
		   (rewrite m-function-call op arguments))))


	((list) (values nil t 'null))
	((list* sentences) (rewrite m-sentences sentences))))

;; this is currently necessary for with-cc operation
;;   (this will be fixed in the future improvement)

;; (maca (= (|Object| >> call-lambda)
;; 		 (-> (fn)
;; 			 (fn > call this)))
;; 	  (var call-lambda
;; 		 (-> (fn)
;; 			 (fn > call this))))

(defmaca (m-chain-function-call :is-value t) (op chain-arguments)
  `(glue (,op ,@(car chain-arguments))
		 ,@(mapcar #'(lambda (args) `(paren (comma ,@args)))
				   (cdr chain-arguments))))

(defmaca (m-function-call :environment env :is-value t) (op args)
  (let (handlers actual-args value-args)
	(loop for arg in args do
		 (if (and (typep arg 'list)
				  (eq (car arg) 'with-cc))
			 (with-gensyms (value-arg)
			   (push (m-compile env arg) handlers)
			   (push value-arg actual-args)
			   (push value-arg value-args))
			 (push arg actual-args)))
	(if handlers
		(let ((result 
			   (with-set-temps-in-list 
				   (env (reverse actual-args) temps)
				 `(,op ,@temps))))
		  (loop for arg in value-args do
			   (setf result `(-> (,arg) ,result)))
		  (loop for handler in (reverse handlers) do
			   (setf result 
					 `(paren (glue
							  (paren ,handler)
							  (paren ,result)))))
		  result)
		(with-set-temps-in-list (env args temps)
		  `(glue ,op (paren (comma ,@temps)))))))


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
	  ;;(break "~a" return-as)
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
				(if (eq (car sent) 'var)
					sent
					`(glue (newline-and-indent) ,sent semicolon)))
			sents)))
  ;; `(glue ,@sents))

(recompile)
(setf *recompile-compiler* t)