(in-package :maca)

;; -----------------------------
;; function and function calls

(defparameter *functions*
  '(((list* 'global sentences)          (rewrite m-global sentences))
	((list* 'function name (as type (or '-> '-/>)) (list* args) body)
	 (rewrite m-function-declaration name type args body))
	((list* '->prim (list* args) body)      (rewrite m-primitive-function args body))
	((list* '-> (list* args) body)      (rewrite m-function args body))
	((list* '=> (list* args) body)      (rewrite m-inherit-this-function args body))
	((list* '-/> (list* args) body)     (rewrite m-procedure-function args body))
	((list* '=/> (list* args) body)     (rewrite m-inherit-this-procedure-function args body))
	((list* '-/ name (list* args) body) (rewrite m-inline-function name args body))))

(defun check-args (args)
  (let ((args (flatten args)))
	(cond ((not-uniquep args)        (error "duplicated args: ~a" (not-unique args)))
		  ((notevery #'symbolp args) (error "invalid args:~a" args)))))

;; ----------------------------------------------------------------
;; m-global and m-function, m-procedure-function has the similar definitions
;; 

(defmaca (m-global :environment env :is-value t) (body)
  (compile-let* env 					;push the variables
	  ((compiled-body body))
	(declare (ignore compiled-body))
	`(sentences ,@(aif +variables+
					   `((glue var space (comma ,@(uniquify it)))))
				,@(nreverse +initializations+)
				,@body)))

;; the most primitive javascript function
(defmaca (m-primitive-function :environment env :is-value t) (args body)
  (check-args args)
  (let ((cl (make-closure :arguments args)))
	  ;; search/push the variables 
	  (compile-let* (cons cl env) 
		  ((func-header `(glue function (paren (comma ,@args))))
		   (compiled-body body))
		(declare (ignore compiled-body))
		`(glue ,func-header
			   (blk (,@(aif (closure-variables cl)
							`((glue var space (comma ,@(uniquify it)))))
					   ,@(nreverse (closure-initializations cl))
					   ,@body))))))

(defun rmember (list object)
  (nreverse (cdr (member object (reverse list)))))

(defmaca (m-procedure-function :is-value t) (args body)
  (let ((rest (member '&rest args))
		(keys (member '&key args))
		(butrest (rmember args '&key))
		(butkeys (rmember args '&rest)))
	(when (and rest keys)
	  (error "you can't use &key and &rest at the same time : ~a" args))
	(when (cddr rest)
	  (error "superfluous arguments after the &rest argument: ~a" rest))
	(let* ((regular-args (or butrest butkeys args))
		   (len (length regular-args)))
	  `(->prim ,regular-args
			   ,(if rest `(var ,(cadr rest) (-array >> slice > (call arguments ,len))))
			   ,@(if keys
					 (with-gensyms (keyword-obj)
					   `((var ,keyword-obj (arguments > ,len))
						 ,@(loop for key-name in keys collect
							 `(var ,key-name (,keyword-obj > ,key-name))))))
			   ,@body))))

(defmaca (m-function :is-value t) (args body)
  `(-/> ,args
		,(butlast body)
		(return ,(car (last body)))))

;; ----------------------------------------------------------------
;; inherit functions

(defmaca (m-inherit-this-function :is-value t) (args body)
  (with-gensyms (this inherited-fn)
    (insert-initialization
	 +cl+
     this 'this 
     inherited-fn `(-> ,args ,@(subst this 'this body)))
    inherited-fn))

(defmaca (m-inherit-this-procedure-function :is-value t) (args body)
  (with-gensyms (this inherited-fn)
    (insert-initialization
	 +cl+
     this 'this 
     inherited-fn `(-/> ,args ,@(subst this 'this body)))
    inherited-fn))


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
