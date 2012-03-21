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
	(lbracket . #\[)
	(rbracket . #\])
	(lparen . #\()
	(rparen . #\)))
  "alist for aliasing the constants such as \"on\" \"yes\".")

;; core macro

(defmacro defmaca (name args &body body)
  (with-gensyms (s definition)
	`(defun ,name (,s env ,@args)
	   (let ((,definition ,@body))		;body will be evaluated first
		 (m-compile ,s env ,definition))))) ;so you can perform some bad behavior on "env". it is intentional

(defun m-glue (s env args)
  (format s "~{~a~}"
		  (mapcar
		   #'(lambda (arg)
			   (let ((true-arg (or (cdr (assoc arg *aliases*)) arg)))
				 (typecase true-arg
				   (null "") 
				   (cons (m-compile nil env true-arg))
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

;; -----------------------------
;; keywords

(defmaca m-var (var &optional val)
  `(glue var space
		 ,(if val `(= ,var ,val) var)))

;; -----------------------------
;; function and function calls

(defmaca m-function-call (op args)
  `(glue ,op (paren (comma ,@args))))

(defmaca m-function (args body)
  (let ((args (flatten args)))
    (cond ((not-uniquep args)
		   (error (prints "duplicated argument:" (not-unique args))))
		  ((some #'(lambda (a) (not (symbolp a))) args)
		   (error (prints "invalid argument")))
		  (t 
		   `(glue function (paren (comma ,@args))
				  (blk 
				   (,@(butlast body)
					  (return ,(car (last body))))))))))

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
  `(glue (? (!= thing null) ,thing (void 0))))

(defmaca m-if (condition then &optional else)
  `(glue if (paren ,condition) (blk ,then) 
		 ,(when else `(else (blk ,else)))))

(defmaca m-iter-array (val array body &optional (key (gensym)))
  (let ((len (gensym "l"))
		(ref (gensym "ref")))
	`((var ,key)
	  (var ,val)
	  (var ,ref ,array)
	  (var ,len (,ref > length))
	  (glue for
			(paren ((= ,key 0)
					(< ,key ,len)
					(++ ,key)))
			(blk 
			 ((= ,val (,ref -. ,key))
			  ,@body))))))

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

(defmaca m-infix (op vars)
  `(paren (glue ,(car vars) space ,op space
				,(if (third vars) 
					 `(,op ,@(cdr vars))
					 (cadr vars)))))

(defparameter *comparisons* 
  '(== != === !== > < >= <=))

(defmaca m-comparison (op vars)
  (if (third vars)
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

(defmaca m-obj (key-value-plist)
  (if (oddp (length key-value-plist))
	  (error "invalid object literal")
	  (let* ((alist (plist-to-alist key-value-plist))
			 (pairs (mapcar #'(lambda (cons)
								`(glue ,(car cons) colon ,(cdr cons)))
							alist)))
		`(blk (comma ,@pairs)))))

(defmaca m-direct-accessor (obj accessor)
  `(glue ,obj
		 lbracket
		 ,(if (cdr accessor)
			  accessor
			  (car accessor))
		 rbracket))


(defmaca m-accessor (obj accessor)
  `(glue ,obj period
		 ,(if (cdr accessor)
			  accessor
			  (car accessor))))

(defmaca m-exist-accessor (obj accessor) ; &key env
  (let ((ref (gensym))
		(child (car accessor)))
	`(glue (? (!= (paren (= ,ref (,obj > ,child)))
							   null)
						   ,(if (cdr accessor)
								`(,ref . ,(cdr accessor))
								ref)
						   (void 0)))))

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

(defun m-compile (s env lst)
  (macrolet ((rewrite (name &rest args)
			   `(values (,name s env ,@args) ',name)))
	(match lst
	  ;; these operators are just meant to be used by the compiler
	  ;; don't use it
	  ((list* 'glue clauses)    (rewrite m-glue clauses))
	  ((list 'paren clause)     (rewrite m-paren clause))
	  ((list* 'comma clauses)   (rewrite m-comma clauses))
	  ((list 'blk clause)     (rewrite m-block clause))

	  ((type atom val) (values (m-glue s env (list val)) (type-of val)))
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
	  ((list* 'for val 'in array body)          (rewrite m-iter-array val array body))
	  ((list* 'for val key 'in array body)      (rewrite m-iter-array val array body key))
	  ;;     ;; ((list* 'for val 'of array)               (rewrite m-iter-obj val nil array))
	  ;;     ;; ((list* 'for val key 'of array)           (rewrite m-iter-obj val key array))
	  ;;     ;; ((list* 'for 'own val key 'of array)      (rewrite m-iter-obj val key array :own t))
	  ((list 'if cond then)                     (rewrite m-if cond then))
	  ((list 'if cond then else)                (rewrite m-if cond then else))
	  ((list 'try body 'catch (list error-var) error)              (rewrite m-try body error-var error))
	  ((list 'try body 'catch (list error-var) error 'finally fin) (rewrite m-try body error-var error fin))
	  ((list* 'try body _)                   (error "invalid try-catch statement"))
	  ((list* obj '-. accessor)                  (rewrite m-direct-accessor obj accessor))
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
		  `(m-compile t nil ',@body)
		  `(m-compile t nil ',body))
	 (format t "~%")))
