(in-package :maca)

;; -----------------------------
;; function and function calls

(defparameter *functions*
  '(((list 'global sentences)          (rewrite m-global sentences))
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

(defmaca (m-global :environment env :stream s) (body)
  (let ((str (m-compile nil env `(,@(butlast body) (return ,@(last body))))))
	(m-compile s env `(blk (glue ,(aif +variables+ `((glue var space (comma ,@(uniquify it)))))
								 (,@(nreverse +initializations+)))))
	(write-string str s)
	nil))

(defmaca (m-function :environment env :stream s) (args body)
  (check-args args
	(m-compile s env `(glue function (paren (comma ,@args))))
	(let ((cl (make-closure :arguments args)))
	  (let ((str (m-compile nil (cons cl env) `(,@(butlast body) (return ,@(last body))))))
		(m-compile s env `(blk (glue ,(aif +variables+ `((glue var space (comma ,@(uniquify it)))))
									 (,@(nreverse +initializations+)))))
		(write-string str s)
		nil))))

(defmaca (m-procedure-function :environment env :stream s) (args body)
  (check-args args
	(m-compile s env `(glue function (paren (comma ,@args))))
	(let ((cl (make-closure :arguments args)))
	  (let ((str (m-compile nil (cons cl env) body)))
		(m-compile s env `(blk (glue ,(aif +variables+ `((glue var space (comma ,@(uniquify it)))))
									 (,@(nreverse +initializations+)))))
		(write-string str s)
		nil))))

;; ----------------------------------------------------------------
;; inherit functions

(defun insert-initialization (cl &rest var-val-plist)
  (labels ((rec (plist)
			 (let ((var (car plist))
				   (val (cadr plist)))
			   (push var (closure-variables cl))
			   (push `(= ,var ,val) (closure-initializations cl)))
			 (when (cddr plist)
			   (rec (cddr plist)))))
    (rec var-val-plist)))

(defmaca m-inherit-this-function (args body)
  (let ((this (gensym "t"))
		(fn (gensym "f")))
    (insert-initialization
	 +cl+
     this 'this 
     fn `(-> ,args ,@(subst this 'this body)))
    fn))

(defmaca m-inherit-this-procedure-function (args body)
  (let ((this (gensym "t"))
		(fn (gensym "f")))
    (insert-initialization
	 +cl+
     this 'this 
     fn `(-/> ,args ,@(subst this 'this body)))
    fn))

(defmaca m-inline-function (name args body)
  (setf (getf +inline-lambda+ name) (cons args body))
  nil)

;; ----------------------------------------------------------------
;; inline function

(defun search-lambda (name env)
  (if env
      (aif (getf (closure-inline-lambda (car env)) name)
		   (values it (car env))
		   (search-lambda name (cdr env)))
      (values nil nil)))

(defun expand-inline (name args env)
  (multiple-value-bind (lmb found-cl) (search-lambda name env)
    (when lmb
      (destructuring-bind (lambda-list . body) lmb
		(let ((temps (mapcar #'(lambda (x) (declare (ignore x)) (gensym "TMP"))
							 lambda-list))
			  (copying-script nil))
		  (loop
			 for arg in args
			 for temp in temps
			 for param in lambda-list
			 do (setf body (subst temp param body))
			 do (push temp (closure-variables found-cl))
			 do (push `(= ,temp ,arg) copying-script))
		  `(paren (comma ,@copying-script
						 ,@body)))))))

(defmaca (m-function-call :environment env) (op args)
  (aif (expand-inline op args env)
       it
       `(glue ,op (paren (comma ,@args)))))

