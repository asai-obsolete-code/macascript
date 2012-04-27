
(in-package :maca)

;; test whether a list is unique 

(defun uniquify (lst &key (test #'eq))
  (labels ((rec (unique rest)
			 (if rest
				 (if (member (car rest) unique :test test)
					 (rec unique (cdr rest))
					 (rec (cons (car rest) unique) (cdr rest)))
				 unique)))
    (rec nil lst)))

(defun not-uniquep (lst &key (test #'eq))
  (if lst
      (or (member (car lst) (cdr lst) :test test)
		  (not-uniquep (cdr lst) :test test))
      nil))

(defun not-unique (lst &key (test #'eq))
  (aif (not-uniquep lst :test test)
       (cons (car it) (not-unique (cdr lst) :test test))))

(defun uniquep (lst &key (test #'eq))
  (not (not-uniquep lst :test test)))



(defun atom-or-op (arg)
  (or (atom arg) (atom (car arg))))

(defun 1-or-2-line-set-temp (arg temp)
  (if (atom-or-op arg)
	  `((var ,temp ,arg))
	  `(,@(butlast arg)
		  (var ,temp ,@(last arg)))))

(defun 1-or-2-line (arg)
  (if (atom-or-op arg)
      (list arg)
      arg))

(defmacro with-set-temp (env (&rest scripts) &body body)
  "search the arguments in args for a non-value statement,
 convert it to assign the value of the statement to a temporary variable,
 then finally put the variable where the statement used to be.
 this will ease the effort of converting any non-value statement
 like js' if, switch, while into the returnable, assignable statement."
  (let ((binds (mapcar #'(lambda (script)
						   (cons script (gensym "BINDING")))
					   scripts)))
	`(let ,(mapcar #'cdr binds)
	   `(glue 
		 ,,@(mapcar
			 #'(lambda (bind)
				 (let ((script (car bind))
					   (compiled-script-slot (cdr bind)))
				   `(with-gensyms (setter-temp)
					  (multiple-value-bind (compiled is-value type)
						  (m-compile ,env ,script :return setter-temp)
						(declare (ignorable type))
						(if is-value 
							(progn
							  (setf ,compiled-script-slot compiled)
							  nil)
							(progn
							  (setf ,compiled-script-slot setter-temp)
							  compiled))))))
			 binds)
		 ,(let* ,(mapcar #'(lambda (bind)
							 (list (car bind) (cdr bind)))
						 binds)
				,@body)))))

(defmacro compile-let* (env args &body body)
  `(let* ,(mapcar #'(lambda (arg) 
					  (if (/= 2 (length arg))
						  (error "irregal arglist ~a" arg)
						  `(,(car arg) (m-compile ,env ,(cadr arg)))))
				  args)
	 ,@body))

(defun insert-initialization (cl &rest var-val-plist)
  "helper function for inserting initialization of variables."
  (labels ((rec (plist)
			 (let ((var (car plist))
				   (val (cadr plist)))
			   (push var (closure-variables cl))
			   (push `(= ,var ,val) (closure-initializations cl)))
			 (when (cddr plist)
			   (rec (cddr plist)))))
    (rec var-val-plist)))

(defun search-lambda (name env)
  (if env
      (aif (getf (closure-inline-lambda (car env)) name)
		   (values it (car env))
		   (search-lambda name (cdr env)))
      (values nil nil)))

(defmacro with-set-temps-in-list ((env lst temps-name) &body body)
  "search the arguments in args for a non-value statement,
 convert it to assign the value of the statement to a temporary variable,
 then finally put the variable where the statement used to be.
 this will ease the effort of converting any non-value statement
 like js' if, switch, while into the returnable, assignable statement."
  (with-gensyms (temp-syms body-args compiled-args var-list)
	`(let* ((,temp-syms
			 (mapcar #'(lambda (arg)
						 (declare (ignore arg))
						 (gensym "ARG-NAME"))
					 ,lst))
			,body-args ,var-list
			(,compiled-args
			 (mapcar #'(lambda (arg argtmp)
						 (multiple-value-bind (compiled is-value type)
							 (m-compile ,env arg :return argtmp)
						   (declare (ignorable type))
;; (break "arg: ~a~%compiled: ~a~%return-as: ~a~%is-value: ~a~%type: ~a"
;; 		  arg compiled argtmp is-value type)
						   (if is-value 
							   (progn (push compiled ,body-args) nil)
							   (progn (push argtmp ,body-args)
									  (push argtmp ,var-list)
									  compiled))))
					 ,lst ,temp-syms)))
	   (let ((,temps-name (nreverse ,body-args)))
		 (appendf +variables+ ,var-list)
		 `(glue ,@,compiled-args
				,,@body)))))

(defun env-args (env)
  (loop for cl in env collect (closure-variables cl)))
(defun env-indents (env)
  (loop for cl in env sum (closure-indentation cl)))


(defmacro with-check-cc ((args) &body body)
"search the arguments in args for with/cc and
 convert it to the continuation call."
  (with-gensyms (handlers actual-args value-args)
	(let ((body-inner (subst `(reverse ,actual-args) args body)))
	  `(let (,handlers ,actual-args ,value-args)
		 (loop for arg in ,args do
			  (if (and (typep arg 'list)
					   (eq (car arg) 'with-cc))
				  (with-gensyms (value-arg)
					(push (m-compile env arg) ,handlers)
					(push value-arg ,actual-args)
					(push value-arg ,value-args))
				  (push arg ,actual-args)))
		 (if ,handlers
			 (progn 
			   (let ((result ,@body-inner))
				 (loop
					for arg in ,value-args
					for handler in (reverse ,handlers)
					do
					  (setf result `(-> (,arg) ,result))
					do
					  (setf result `(--- (paren ,handler) (,result))))
				 result))
			 ,@body)))))