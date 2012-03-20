(ql:quickload :alexandria)
(ql:quickload :cl-match)
(ql:quickload :anaphora)

(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora))

(proclaim '(optimize (debug 3)))

(in-package :maca)

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

(defparameter *constants*
  '((t . true)
    (on . true)
    (yes . true)
    (off . false)
    (no . false)
    (null . undefined)
    (@ . this))
  "alist for aliasing the constants such as \"on\" \"yes\".")

(defparameter *env* nil
  "contains lists of variables enclosed in each closure. 
each element represents a closure and it contains variable list.")

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

;; (defmacro defw (name args &rest contents)
;;   (let ((args (append args (unless (member '&key args) '(&key)) '(must-return-value))))
;;     `(defun ,name ,args
;;        (prints ,@contents))))

(defun gensym-js (&optional (thing "G"))
  (symbol-name (gensym thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewriter

;; helpers

(defparameter *maca-rules* nil
  "alist of name-rule pair")

(defmacro defmaca (name args rule &body body)
  (declare (ignore rule))
  `(defun ,name ,args
     (m-compile ,@body)))

(defun m-glue (args)
  (m-compile arg))
(defw m-paren (arg) "(" (m-compile arg) ")")
(defw m-block (arg) "{" (m-compile arg) "}")
(defun m-comma (&rest args)
  (format nil "~{~a~^,~}" (mapcar #'m-compile args)))
(defun m-sentences (sents)
  (format nil "~{~a;~%~}" (mapcar #'m-compile sents)))

;; -----------------------------
;; keywords

(defun m-var (var &optional val)
  (prints-if val
	     ("var " (m-compile `(= ,var val)))
	     ("var " (m-compile var))))

;; (defw m-var (var val)
;;   (if must-return-value
;;       (prints "(var " (m-compile var) "=" (m-compile val) ")")
;;       (prints "var " (m-compile var) "=" (m-compile val))))


(defw m-return (&optional value)
  "return " (m-compile value))

;; -----------------------------
;; function and function calls
(defw m-function-call (op args)
  (m-compile op)
  (m-compile `(paren (comma ,@(mapcar #'m-compile args)))))

(defun m-args (&rest args)
  (let ((args (flatten args)))
    (cond ((not-uniquep args)
	   (error (prints "duplicated argument in the list:" (not-unique args))))
	  ((some #'(lambda (a) (not (symbolp a))) args)
	   (error (prints "invalid argument in the list")))
	  (t (m-compile `(comma ,@args))))))

(defw m-function (args body)
  "function"
  (m-compile `(paren (comma ,args)))
  (m-compile `(blk ,(butlast body) (return ,(car (last body))))))

;; not implemented
(defw m-inherit-this-function (args body)
  "function(" (m-args args) "){"
  (m-sentences (butlast body))
  (m-return (car (last body)))
  "}")

(defw m-procedure-function (args body)
  "function(" (m-args args) "){"
  (m-sentences (butlast body))
  (m-return (car (last body)))
  "}")

(defw m-inline-function (args body)
  "function(" (m-args args) "){"
  (m-sentences (butlast body))
  (m-return (car (last body)))
  "}")

;; -----------------------------
;; iteration and conditional expression

(defw m-? (condition then &optional (else 'undefined))
  (m-compile condition) "?" (m-compile then) ":" (m-compile else))

(defun m-if (condition then &optional else)
  (prints-if else
	     ("if" (m-compile `(paren ,condition))
		   (m-compile `(blk ,@then))
		   "else" 
		   (m-compile `(blk ,@else)))
	     ("if" (m-compile `(paren ,condition))
		   (m-compile `(blk ,@then)))))

(defw m-iter-array (val array body &optional (key (gensym-js)))
  (let ((len (gensym-js "l"))
	(ref (gensym-js "ref")))
    (m-compile `((var ,key)
		 (var ,len (,array > length))
		 (var ,ref)))
    "for"
    (m-compile `(paren ((= key 0)
			(< ,key ,len)
			(++ key))))
    (m-compile `(blk (= ,ref (,array > ,key)))
	       ,@body)))

;; -----------------------------
;; math and operators

(defparameter *assignments*
  '(= += -= *= /= <<= >>= >>>= &= ^= ))	;|= 
(defw m-assignments (op to from)
  (m-compile to) " " op " " (m-compile from))

(defparameter *infixes* 
  '(+ - * / % << >> >>> &&)) 		;||
(defun m-infix (op vars)
  (aif (cdr vars)
       (prints "(" (m-compile (car vars)) " " op " " (m-infix op it) ")")
       (m-compile (car vars))))

(defparameter *comparisons* 
  '(== != === !== > < >= <=))

(defw m-comparison-primitive (op var1 var2)
  "(" (m-compile var1) " " op " " (m-compile var2) ")")

(defun m-comparison (op vars)
  (if (third vars)
      (prints (m-comparison-primitive op (first vars) (second vars))
	      " && "
	      (m-comparison op (cdr vars)))
      (m-comparison-primitive op (first vars) (second vars))))

(defparameter *mono-ops*
  '(++ -- ^ ~ ! 
    new set get typeof instanceof void delete var))
(defw m-mono-ops (op val)
  op " (" (m-compile val) ")")

;; -----------------------------
;; array and object literals
(defun m-obj (key-value-lst)
  (format nil "{~%~{~a:~a~^,~%~}~%}"
	  (mapcar #'(lambda (val) (if (keywordp val) val (m-compile val)))
		  key-value-lst)))

(defw m-accessor (obj accessor)
  (m-compile obj) "." (if (cdr accessor) 
			  (m-compile accessor)
			  (m-compile (car accessor))))

(defmacro print-with-environtment (envname &body body)
  `(prints (format nil "~{~a~}" 
		   (m-compile 
		    (mapcar #'(lambda (varname) `(var ,varname))
			    ,envname)))
	   ,@body))

(defun m-exist-accessor (obj accessor &key env)
  (let ((ref (gensym-js))
	(child (car accessor)))
    (setf (car accessor) ref)
    (if env (push ref env))
    (prints "("
	    (m-? `(!= (paren (= ,ref (,obj > ,child))) null)
		 (if (cdr accessor)
		     accessor
		     ref))
	    ")")))

(defw m-prototype-accessor (obj accessor)
  (m-compile obj) ".prototype." (if (cdr accessor) 
			  (m-compile accessor)
			  (m-compile (car accessor))))

;; -----------------------------
;; main compilation

(defmacro rewrite (name &rest args)
  `(values (,name ,@args) ',name))

;; todo:
;;   add an enviromental valiable
;;   add "must-return-value" option
(defun m-compile (lst)
  (if (null *env*)
      *env*
  (match lst
    ;; these operators are just meant to be used by the compiler
    ;; don't use it
    ((list 'paren clause)     (rewrite m-paren clause))
    ((list* 'comma clauses)   (rewrite m-comma clauses))
    ((list* 'blk clauses)       (rewrite m-block clauses))
    ((list* 'glue clauses)       (rewrite m-glue clauses))

    ((when (assoc val *constants*) (type atom val)) (cdr (assoc val *constants*)))
    ((type atom val) (values val 'atom))
    ((list 'var (or (type symbol v1)
		    (type string v1)
		    (type keyword v1)))  (rewrite m-var v1))
    ((list 'var (or (type symbol v1)
		    (type string v1)
		    (type keyword v1)) v2)  (rewrite m-var v1 v2))
    ((list* 'var _ rest)              (error "invalid variable name"))
    ((when (member op *assignments*) (list op v1 v2))  (rewrite m-assignments op v1 v2))
    ((when (member op *infixes*)     (list* op vars))  (rewrite m-infix op vars))
    ((when (member op *mono-ops*)    (list op var))    (rewrite m-mono-ops op var))
    ((when (member op *comparisons*) (list* op vars))  (rewrite m-comparison op vars))
    ((list (as op 'in) v1 v2)                 (rewrite m-comparison-primitive op v1 v2))
    ((list '? cond then else)                 (rewrite m-? cond then else))
    ;; ((list 'set var val)                      (rewrite m-set var val))
    ((list* '-> (list* args) body)            (rewrite m-function args body))
    ((list* '=> (list* args) body)            (rewrite m-inherit-this-function args body))
    ((list* '-/> (list* args) body)           (rewrite m-procedure-function args body))
    ((list* '-/  (list* args) body)           (rewrite m-inline-function args body))
    ;; ((list* 'for val 'in array body)          (rewrite m-iter-array val nil array))
    ;; ((list* 'for val key 'in array body)      (rewrite m-iter-array val key array))
    ;; ((list* 'for val 'of array)               (rewrite m-iter-obj val nil array))
    ;; ((list* 'for val key 'of array)           (rewrite m-iter-obj val key array))
    ;; ((list* 'for 'own val key 'of array)      (rewrite m-iter-obj val key array :own t))
    ((list 'if cond then)                     (rewrite m-if cond then))
    ((list 'if cond then else)                (rewrite m-if cond then else))
    ;; ((list 'try body 'catch (list var) error)              (rewrite m-try body var error nil))
    ;; ((list 'try body 'catch (list var) error 'finally fin) (rewrite m-try body var error fin))
    ;; ((list* 'try body _)                   (error "invalid try-catch statement"))
    ((list* obj '> accessor)                  (rewrite m-accessor obj accessor))
    ((list* obj '? accessor)                  (rewrite m-exist-accessor obj accessor))
    ((list* obj '-> accessor)                 (rewrite m-prototype-accessor obj accessor))
    ((list* (type keyword key) rest)          (rewrite m-obj lst))
    ((quote 
    ((list* (type atom op) arguments)         (rewrite m-function-call op arguments))
    ((list* sentences)                        (rewrite m-sentences sentences))
    ))


(defmacro maca (&body body)
  (if (= (length body) 1)
      `(m-compile ',@body)
      `(m-compile ',body)))

;; atom
(maca a)

;; constants
(maca undefined)

;; assignments
(maca (= number 3))
(maca (var number 3))
;; (maca (var (+ 2 1) 3))

;; infix
(maca (>>> number 3))
(maca (+ a 3))
(maca (+ a 3 (>>> number 3)))

;; comparison
(maca (< number 50))
(maca (== 5 3))
(maca (== 5 3 4))
(maca (>= 5 3 4))

;; mono-ops
(maca (new (number)))
(maca (typeof 5))

;; in
(maca (in 5 array))

;; function definition
(maca (-> (a b c)
	  (= d (- a b c))
	  (+ a b c d)))

;; conditional expression
(maca (if (a b c)
	  (= d (- a b c))))		;one-line
(maca (if (a b c)
	  (= d (- a b c))
	  (= d (- a b c))))		;one-line else
(maca (if (a b c)
	  ((= d (- a b c))
	   (= d (- a b c)))
	  (= e (+ a b c d))))		;multi-line then

(maca (if (a b c)			;multi-line both 
	  ((= d (- a b c))
	   (= e (+ a b c d)))
	  ((= e (+ a b c d))
	   (= d (- a b c)))))

(maca (? a b c))

;; object accessor
(maca (obj > attibute))
(maca (obj > child > grandchild))
(maca (granpa > parent > obj > child > grandchild))
(maca (obj > (child) > (grandchild)))
(maca (obj > (child) > grandchild))
(maca (obj -> child))
(maca (obj -> (method)))

(maca (a ? b))
(maca (a ? b ? c))

(maca (a > b ? c ? d > (e) > (f) ?  g -> h))

