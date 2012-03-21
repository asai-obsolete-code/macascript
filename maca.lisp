(ql:quickload :alexandria)
(ql:quickload :cl-match)
(ql:quickload :anaphora)

(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora))

(proclaim '(optimize (debug 3)))

(in-package :maca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

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

(defun prints (&rest args)
  (format nil "~{~a~}" args))

(defmacro defw (name args &rest contents)
  `(defun ,name ,args
     (prints ,@contents)))

(defmacro prints-if (condition then &optional else)
  `(if ,condition
       (prints ,@then)
       (prints ,@else)))

(defun gensym-js (&optional (thing "G"))
  (symbol-name (gensym thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewriter

;; helpers


(defparameter *reserved*
  '(break case catch continue default delete
    do else finally for function if in instanceof
    new return switch this throw try typeof var
    void while with 
    ;; future reserved
    abstract boolean byte char class const debugger
    double enum export extends final float goto implements
    import int interface long native package private protected
    public short static super synchronized throws transient volatile))

(defparameter *aliases*
  `((t . true)
    (on . true)
    (yes . true)
    (off . false)
    (no . false)
    (null . undefined)
    (@ . this)
	(newline . #\Newline)
	(space   . #\Space)
	(colon . #\:)
	(period . #\.)
	(semicolon . #\;)
	(comma . #\,)
	(lbrace . #\{)
	(rbrace . #\})
	(lparen . #\()
	(rparen . #\)))
  "alist for aliasing the constants such as \"on\" \"yes\".")

(defparameter *env* nil
  "contains lists of variables enclosed in each closure. 
each element represents a closure and it contains variable list.")



(defmacro defmaca (name args &body body)
  (with-gensyms (s)
	`(defun ,name ,(cons s args)
	   (m-compile ,s ,@body))))

;; (defmacro m-compile-if (s condition &body thenelse)
;;   `(if ,condition
;;        (m-compile ,s ,(car thenelse))
;;        (m-compile ,s ,@(cdr thenelse))))

;; (m-compile-if a
;;   `(paren ,a)
;;   `(comma ,a ,a))

(defun m-glue (s args)
  (format s "~{~a~}"
		  (mapcar
		   #'(lambda (arg)
			   (let ((true-arg (or (cdr (assoc arg *aliases*)) arg)))
;; 				 (break "~a" (type-of true-arg))
				 (typecase true-arg
				   (null "") 
				   (cons
;; 					(break "~a ~a" true-arg (m-compile nil true-arg))
					(m-compile nil true-arg))
				   (string (format nil "\"~a\"" true-arg))
				   (t (format nil "~(~a~)" true-arg)))))
		   args)))


(defmaca m-paren (arg)
  `(glue lparen ,arg rparen))

(defmaca m-block (arg)
  `(glue lbrace newline ,arg rbrace))
(defmaca m-comma (args)
  `(glue ,@(butlast 
			(mapcan
			 #'(lambda (arg) (list arg 'comma))
			 args))))
(defmaca m-sentences (sents)
  `(glue ,@(mapcar
			#'(lambda (sent) `(glue ,sent semicolon newline))
			sents)))

;; (defun m-paren (arg) 
;;   (format nil "(~%~a)" (m-compile arg)))
;; (defun m-block (arg)
;;   (format nil "{~%~a}" (m-compile arg)))
;; (defun m-comma (args)
;;   (format nil "~{~a~^,~}" (mapcar #'m-compile args)))
;; (defun m-sentences (sents)
;;   (format nil "~{~a;~%~}" (mapcar #'m-compile sents)))

;; -----------------------------
;; keywords

(defmaca m-var (var &optional val)
  `(glue var space
		 ,(if val `(= ,var ,val) var)))

;; (defun m-var (var &optional val)
;;   (prints-if val
;; 	     ("var " (m-compile `(= ,var val)))
;; 	     ("var " (m-compile var))))

;; -----------------------------
;; function and function calls
(defmaca m-function-call (op args)
  `(glue ,op (paren (comma ,@args))))
;; -  (m-compile op)
;; -  (m-compile `(paren (comma ,@(mapcar #'m-compile args)))))

(defun m-function (s args body)
  (let ((args (flatten args)))
    (cond ((not-uniquep args)
		   (error (prints "duplicated argument:" (not-unique args))))
		  ((some #'(lambda (a) (not (symbolp a))) args)
		   (error (prints "invalid argument")))
		  (t 
		   (m-compile s `(glue function (paren (comma ,@args))
							   (blk 
								(,@(butlast body)
								  (return ,(car (last body)))))))))))

;; ;; not implemented
;; (defw m-inherit-this-function (args body)
;;   "function(" (m-args args) "){"
;;   (m-sentences (butlast body))
;;   (m-return (car (last body)))
;;   "}")

;; (defw m-procedure-function (args body)
;;   "function(" (m-args args) "){"
;;   (m-sentences (butlast body))
;;   (m-return (car (last body)))
;;   "}")

;; (defw m-inline-function (args body)
;;   "function(" (m-args args) "){"
;;   (m-sentences (butlast body))
;;   (m-return (car (last body)))
;;   "}")

;; -----------------------------
;; iteration and conditional expression

(defmaca m-? (condition then &optional (else 'undefined))
  `(glue (paren ,condition) ? (paren ,then) colon (paren ,else)))

(defmaca m-exist-? (thing)
  `(glue (? (!= thing null) thing (void 0))))

(defmaca m-if (condition then &optional else)
;;   (break "~a ~a" condition then else)
  `(glue if (paren ,condition) (blk ,then) 
		 ,(when else `(else (blk ,else)))))

(defun m-iter-array (s val array body &optional (key (gensym-js)))
  (let ((len (gensym "l"))
		(ref (gensym "ref")))
    (m-compile s
	 `((var (comma ,key ,val
				   (= ,ref ,array)
				   (= ,len (,array > length))))
	   (glue for
			 (paren ((= key 0)
					 (< ,key ,len)
					 (++ key)))
			 (blk 
			  ((= ,val (,ref > ,key))
			   ,@body)))))))


;; -----------------------------
;; try/catch expression

(defmaca m-try (body error-var error &optional finally)
  `(glue try 
		 (blk ,body)
		 catch (paren ,error-var)
		 (blk ,error)
		 ,@(when finally
				 `(finally (blk ,finally)))))
			 

;; -----------------------------
;; math and operators

(defparameter *assignments*
  '(= += -= *= /= <<= >>= >>>= &= ^= ))	;|= 
(defmaca m-assignments (op to from)
  `(glue ,to space ,op space ,from))

(defparameter *infixes* 
  '(+ - * / % << >> >>> && in)) 		;||

(defun m-infix (s op vars)
  (m-compile-if s (third vars)
	`(paren (glue ,(car vars) space ,op space (,op ,@(cdr vars))))
	`(paren (glue ,(car vars) space ,op space ,(cadr vars)))))

(defparameter *comparisons* 
  '(== != === !== > < >= <=))

;; (defmaca m-comparison-primitive (op var1 var2)
;;   `(paren (glue ,var1 ',op ,var2)))

(defun m-comparison (s op vars)
  (m-compile-if s (third vars)
	`(glue (paren (glue ,(first vars) ,op ,(second vars)))
		   &&
		   (,op ,@(cdr vars)))
	`(paren (glue ,(first vars) ,op ,(second vars)))))
     

(defparameter *mono-ops*
  '(++ -- ^ ~ ! 
    new set get typeof instanceof
    void delete return))
(defmaca m-mono-ops (op val)
  `(glue ,op space ,val))

;; -----------------------------
;; array and object literals

(defun plist-to-alist (plist)
  (labels ((rec (p a)
			 (if p
				 (rec (cddr p) 
					  (cons (cons (car p) (cadr p)) a))
				 a)))
	(rec plist nil)))

(defun m-obj (s key-value-plist)
;;   (break "~A" (plist-to-alist key-value-plist))
;;   (break "~A" (length key-value-plist))
  (if (oddp (length key-value-plist))
	  (error "invalid object literal")
	  (let* ((alist (plist-to-alist key-value-plist))
			 (pairs (mapcar #'(lambda (cons)
								`(glue ,(car cons) colon ,(cdr cons)))
							alist)))
;; 		(break "~A" alist)
		(m-compile s `(blk (comma ,@pairs))))))

;;   (format nil "{~%~{~a:~a~^,~%~}~%}"
;; 	  (mapcar #'(lambda (val) (if (keywordp val) val (m-compile val)))
;; 		  key-value-lst)))


;; (defun m-obj (key-value-lst)
;;   (format nil "{~%~{~a:~a~^,~%~}~%}"
;; 	  (mapcar #'(lambda (val) (if (keywordp val) val (m-compile val)))
;; 		  key-value-lst)))

(defmaca m-accessor (obj accessor)
  `(glue ,obj period
		 ,(if (cdr accessor)
			  accessor
			  (car accessor))))

;; (defw m-accessor (obj accessor)
;;   (m-compile obj) "." (if (cdr accessor) 
;; 			  (m-compile accessor)
;; 			  (m-compile (car accessor))))

;; (defmacro print-with-environtment (envname &body body)
;;   `(prints (format nil "~{~a~}" 
;; 		   (m-compile 
;; 		    (mapcar #'(lambda (varname) `(var ,varname))
;; 			    ,envname)))
;; 	   ,@body))

(defun m-exist-accessor (s obj accessor) ; &key env
  (let ((ref (gensym))
		(child (car accessor)))
	(m-compile s `(glue (? (!= (paren (= ,ref (,obj > ,child)))
							   null)
						   ,(if (cdr accessor)
								`(,ref . ,(cdr accessor))
								ref)
						   (void 0))))))

(defmaca m-prototype-accessor (obj accessor)
  `(glue ,obj period prototype period
		 ,(if (cdr accessor) 
			  accessor
			  (car accessor))))

;; -----------------------------
;; main compilation

;; todo:
;;   add an enviromental valiable
;;   add "must-return-value" option

(defun m-compile (s lst)
  (macrolet ((rewrite (name &rest args)
			   `(values (,name s ,@args) ',name)))
	(match lst
	  ;; these operators are just meant to be used by the compiler
	  ;; don't use it
	  ((list* 'glue clauses)    (rewrite m-glue clauses))
	  ((list 'paren clause)     (rewrite m-paren clause))
	  ((list* 'comma clauses)   (rewrite m-comma clauses))
	  ((list 'blk clause)     (rewrite m-block clause))

	  ;;     ((when (assoc val *aliases*) (type atom val)) (cdr (assoc val *aliases*)))
	  ((type atom val) (values (m-glue s (list val)) (type-of val)))
	  ((list 'var (type symbol v1)) (rewrite m-var v1))
	  ((list 'var (type symbol v1) v2)  (rewrite m-var v1 v2))
	  ((list* 'var _ rest)              (error "invalid variable name"))
	  ((when (member op *assignments*) (list op v1 v2))  (rewrite m-assignments op v1 v2))
	  ((when (member op *infixes*)     (list* op vars))  (rewrite m-infix op vars))
	  ((when (member op *mono-ops*)    (list op var))    (rewrite m-mono-ops op var))
	  ((when (member op *comparisons*) (list* op vars))  (rewrite m-comparison op vars))
	  ((list '? thing)                 (rewrite m-exist-? thing))
	  ((list '? cond then else)                 (rewrite m-? cond then else))
	  ;;     ;; ((list 'set var val)                      (rewrite m-set var val))
	  ((list* '-> (list* args) body)            (rewrite m-function args body))
	  ;;     ((list* '=> (list* args) body)            (rewrite m-inherit-this-function args body))
	  ;;     ((list* '-/> (list* args) body)           (rewrite m-procedure-function args body))
	  ;;     ((list* '-/  (list* args) body)           (rewrite m-inline-function args body))
	  ;;     ;; ((list* 'for val 'in array body)          (rewrite m-iter-array val nil array))
	  ;;     ;; ((list* 'for val key 'in array body)      (rewrite m-iter-array val key array))
	  ;;     ;; ((list* 'for val 'of array)               (rewrite m-iter-obj val nil array))
	  ;;     ;; ((list* 'for val key 'of array)           (rewrite m-iter-obj val key array))
	  ;;     ;; ((list* 'for 'own val key 'of array)      (rewrite m-iter-obj val key array :own t))
	  ((list 'if cond then)                     (rewrite m-if cond then))
	  ((list 'if cond then else)                (rewrite m-if cond then else))
	  ((list 'try body 'catch (list error-var) error)              (rewrite m-try body error-var error))
	  ((list 'try body 'catch (list error-var) error 'finally fin) (rewrite m-try body error-var error fin))
	  ((list* 'try body _)                   (error "invalid try-catch statement"))
	  ((list* obj '> accessor)                  (rewrite m-accessor obj accessor))
	  ((list* obj '? accessor)                  (rewrite m-exist-accessor obj accessor))
	  ((list* obj '-> accessor)                 (rewrite m-prototype-accessor obj accessor))
	  ((list* (type keyword key) rest)          (rewrite m-obj lst))
	  ;;     ((quote 
	  ((list* (type atom op) arguments)         (rewrite m-function-call op arguments))
	  ((list* sentences)                        (rewrite m-sentences sentences))
	  )))

(defmacro maca (&body body)
  `(progn
	 ,(if (= (length body) 1)
		  `(m-compile t ',@body)
		  `(m-compile t ',body))
	 (format t "~%")))
