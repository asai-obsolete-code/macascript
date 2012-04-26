(in-package :maca)

;; -----------------------------
;; function and function calls

(defparameter *functions*
  '(((list* 'global sentences)          (rewrite m-global sentences))
	((list* 'function name (as type (or '-> '-/>)) (list* args) body)
	 (rewrite m-function-declaration name type args body))
	((list* '-> (list* args) body)      (rewrite m-function args body))
	((list* '-> (list* args) body)      (rewrite m-function args body))
	((list* '=> (list* args) body)      (rewrite m-inherit-this-function args body))
	((list* '-/> (list* args) body)     (rewrite m-procedure-function args body))
	((list* '=/> (list* args) body)     (rewrite m-inherit-this-procedure-function args body))
	((list* '-/ name (list* args) body) (rewrite m-inline-function name args body))))

(defmacro check-args (args &body body)
  `(let ((args (flatten ,args)))
     (cond ((not-uniquep args)        (error "duplicated args: ~a" (not-unique args)))
		   ((notevery #'symbolp args) (error "invalid args:~a" args))
		   (t ,@body))))

;; ----------------------------------------------------------------
;; m-global and m-function, m-procedure-function has the similar definitions
;; 

(defmaca (m-global :environment env :is-value t) (body)
  (compile-let* env
	  ((compiled-body body))
	(declare (ignore compiled-body))
	`(sentences ,@(aif +variables+
					   `((glue var space (comma ,@(uniquify it)))))
				,@(nreverse +initializations+)
				,@body)))

;; (defmaca (m-global :environment env :is-value t) (body)
;;   (compile-let* env 
;; 	  ((compiled-body `(sentences ,@body))
;; 	   (compiled-header 
;; 		(let ((vars (aif +variables+
;; 						 `(glue var space (comma ,@(uniquify it)))))
;; 			  (init (nreverse +initializations+)))
;; 		  (if (or vars init)
;; 			  `(sentences ,vars	,@init)))))
;; 	`(glue ,compiled-header ,compiled-body)))

;; the most primitive javascript function
(defmaca (m-procedure-function :environment env :is-value t) (args body)
  (check-args args
	(compile-let* env
		((func-header `(glue function (paren (comma ,@args)))))
	  (let* ((cl (make-closure :arguments args))
			 (new-env (cons cl env)))
		;; search/push the variables 
		(compile-let* new-env ((compiled-body body))
		  (declare (ignore compiled-body))
		  `(glue ,func-header
				 (blk (,@(aif (closure-variables cl)
							  `((glue var space (comma ,@(uniquify it)))))
					   ,@(nreverse (closure-initializations cl))
					   ,@body))))))))

(defmaca (m-function :is-value t) (args body)
  `(-/> ,args
		,(butlast body)
		(return ,(car (last body)))))

;; ----------------------------------------------------------------
;; inherit functions

(defmaca (m-inherit-this-function :is-value t) (args body)
  (let ((this (gensym "t"))
		(fn (gensym "f")))
    (insert-initialization
	 +cl+
     this 'this 
     fn `(-> ,args ,@(subst this 'this body)))
    fn))

(defmaca (m-inherit-this-procedure-function :is-value t) (args body)
  (let ((this (gensym "t"))
		(fn (gensym "f")))
    (insert-initialization
	 +cl+
     this 'this 
     fn `(-/> ,args ,@(subst this 'this body)))
    fn))


;; ----------------------------------------------------------------
;; m-function-declaration and m-inline-function pushes the definition
;;  and lambda list to the slot of the current closure.
;; It will be used in the compilation to produce a compile-time error.

(defmaca m-inline-function (name args body)
  (setf (getf +inline-lambda+ name) (cons args body))
  nil)

(defmaca m-function-declaration (name type args body)
  (setf (getf +function-lambda+ name) (list type args body))
  `(var ,name (,type ,args ,@body)))
