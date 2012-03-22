(ql:quickload :alexandria)
(ql:quickload :cl-match)
(ql:quickload :anaphora)

(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora))

(proclaim '(optimize (debug 3)))
(in-package :maca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

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

(defmacro rmapcar (lst &body fn)
  (when (cdr fn) (error "not valid rmapcar"))
  `(mapcar ,@fn ,lst))

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
    (not . !)
    (and . &&)
    (or . |\|\||)
    (and= . &=)
    (not= . ^=)
    (or= . |\|=|)
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
    (rparen . #\))
    (comment . \\))
  "alist for aliasing the constants such as \"on\" \"yes\".")
;; |
;; core macro

;; you can perform some bad behavior on "env". it is intentional
(defmacro defmaca (name args &body body)
  (with-gensyms (s definition)
    `(defun ,name (,s env ,@args)
       (let ((,definition 
	      (progn
		,@body)))
	 (m-compile ,s env ,definition)))))


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
(defun m-redundant-paren (s env arg)
  (m-compile s env `(paren ,arg)))
(defmaca m-bracket (arg)
  `(glue lbracket ,arg rbracket))
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

(defun m-comment (s env string)
  (declare (ignore env))
  (format s "/* ~a */~%" string))
(defun m-raw-string (s env string)
  (declare (ignore env s))
  string)

;; -----------------------------
;; keywords

(defmaca m-var (var &optional val)
  (push var (closure-variables (car env)))
  (if val
      `(= ,var ,val)
      nil))

(defmaca m-alias-this (val)
  (let ((property (make-symbol (subseq (symbol-name val) 1))))
  `(this > ,property)))

(defmaca m-array (args)
  `(bracket (comma ,@args)))

;; -----------------------------
;; function and function calls

(defmaca m-function-call (op args)
  `(glue ,op (paren (comma ,@args))))


(defstruct closure
  (arguments nil)
  (variables nil)
  (initializations nil)
  (inline-lambda nil))

(defmacro compile-in-advance (env script)
  `(list 'raw-string 
	 (with-output-to-string (s)
	   (m-compile s ,env ,script))))

(defmacro check-args (args &body body)
  `(let ((args (flatten ,args)))
     (cond ((not-uniquep args)        (error "duplicated args: ~a" (not-unique args)))
	   ((notevery #'symbolp args) (error "invalid args:~a" args))
	   (t ,@body))))

(defmaca m-global (body)
  (let* ((cl (make-closure))
	 (raw (compile-in-advance (cons cl env) body)))
    `(,(aif (closure-variables cl) `(glue var space (comma ,@(uniquify it))))
       ,@(closure-initializations cl)
       ,raw)))


(defmaca m-function (args body)
  (check-args args
    `(glue function (paren (comma ,@args))
	   ,(let* ((cl (make-closure :arguments args))
		   (raw (compile-in-advance (cons cl env)
					    `(,@(butlast body)
						(return ,(car (last body)))))))
		  `(blk (,(aif (closure-variables cl) `(glue var space (comma ,@(uniquify it))))
			  ,@(closure-initializations cl)
			  ,raw))))))

(defmaca m-procedure-function (args body)
  (check-args args
    `(glue function (paren (comma ,@args))
	   ,(let* ((cl (make-closure :arguments args))
		   (raw (compile-in-advance (cons cl env) body)))
		  `(blk (,(aif (closure-variables cl) `(glue var space (comma ,@(uniquify it))))
			  ,@(closure-initializations cl)
			  ,raw))))))

(defmaca m-inherit-this-function (args body)
  (let ((this (gensym "t"))
	(fn (gensym "f")))
    (push this (closure-variables (car env)))
    (push fn (closure-variables (car env)))
    (push `(= ,this this) (closure-initializations (car env)))
    (push `(= ,fn (-> ,args
		      ,@(subst this 'this body)))
	  (closure-initializations (car env)))
    fn))

;; (defun inline (op args)
  

(defmaca m-inline-function (name args body)
  (setf (getf (closure-inline-lambda (car env)) name) (cons args body)))
  
;; -----------------------------
;; iteration and conditional expression

(defmaca m-? (condition then &optional (else 'undefined))
  `(glue (paren ,condition) ? (paren ,then) colon (paren ,else)))

(defmaca m-if-? (thing then &optional else)
  `(if (and (!== ,thing null)
	    (!== (typeof ,thing) "undefined"))
       ,then
       ,else))

(defmaca m-assign-if-exist (to from)
  `(if? ,to
	(= ,to ,from)))

(defmaca m-if (condition then &optional else)
  `(glue if (paren ,condition) (blk ,then) 
	 ,(when else `(else (blk ,else)))))

(defmaca m-for (begin condition next body)
  `(glue for (paren (,begin ,condition ,next))
	 (blk ,body)))

(defmaca m-iter-array (val array body &key (key (gensym)))
  (let ((len (gensym "l"))
	(ref (gensym "ref")))
    `((var ,key)
      (var ,val)
      (var ,ref ,array)
      (var ,len (,ref > length))
      (for ((= ,key 0)
	    (< ,key ,len)
	    (++ ,key))
	(= ,val (,ref -. ,key))
	,@body))))

(defmaca m-iter-obj (val obj body &key (key (gensym)) own)
  (let ((ref (gensym "ref")))
    `((var ,key)
      (var ,val)
      (var ,ref ,obj)
      (glue for (paren (in ,key ,obj))
	    (blk 
	     ((= ,val (,ref > ',key))
	      ,@(when own `((if (! (,val > (hasOwnProperty ,key))) (continue))))
	      ,@body))))))

(defmaca m-while (condition body)
  `(glue while (paren ,condition)
	 (blk ,body)))

(defmaca m-do-while (condition body)
  `(glue do (blk ,body)
	 while
	 (paren ,condition)))

(defmaca m-label (name body)
  `(glue label ,name colon
	 (blk ,body)))

(defmaca m-switch (val conditions)
  `(glue switch (paren ,val)
	 (blk ,conditions)))

(defmaca m-cases (vals body)
  `(glue ,@(mapcan #'(lambda (val) (list 'newline 'case 'space val 'colon)) vals)
	 ,@body break))
(defmaca m-case (val body)
  `(glue case space ,val colon ,@body break))

(defmaca m-default (body)
  `(glue default colon ,body))

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
  '(= += -= *= /= <<= >>= >>>= &= ^= |\|=| and= or= not=))	; 
;; |

(defmaca m-assignments (op to from)
  `(glue ,to space ,op space ,from))

(defparameter *infixes* 
  '(+ - * / % << >> >>> in && |\|\|| and or))

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
  '(++ -- ^ ~ ! not
    new set get typeof instanceof
    void delete))
(defmaca m-mono-ops (op &optional val)
  `(paren (glue ,op space ,@(when val (list val)))))

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

;; input: (obj > 'child > grandchild ...)
;; output: obj[child].grandchild...
;; obj: obj
;; more: ( > grandchild ... )
;; ( nil > grandchild ... )
(defmaca m-direct-accessor (obj child more)
  `(glue ,obj (bracket ,child)
	 ,(when more
		`(nil ,@more))))

(defmaca m-accessor (obj accessor)
  `(glue ,obj period
	 ,(if (cdr accessor)
	      accessor
	      (car accessor))))

(defmaca m-exist-accessor (obj accessor)
  (let ((ref (gensym))
	(child (car accessor)))
    `(? (!= (paren (= ,ref (,obj > ,child)))
	    null)
	,(if (cdr accessor)
	     `(,ref . ,(cdr accessor))
	     ref)
	(void 0))))

(defmaca m-prototype-accessor (obj accessor)
  `(,obj > prototype ,@(when accessor `( > ,@accessor))))

;; -----------------------------
;; main compilation

;; todo:
;;   add an enviromental valiable
;;   add "must-return-value" option

(defpattern direct-specifier (child)
  `(or (list 'quote ,child)
       (type string ,child)
       (type number ,child)
       (type keyword ,child)))

(defun m-compile (s env lst)
  (macrolet ((rewrite (name &rest args)
	       `(values (,name s env ,@args) ',name)))
    (match lst
      ;; these operators are just meant to be used by the compiler
      ;; don't use it
      ((list* 'glue clauses)    (rewrite m-glue clauses))
      ((list 'paren (list 'paren clause)) (rewrite m-redundant-paren clause))
      ((list 'paren clause)     (rewrite m-paren clause))
      ((list 'bracket clause)     (rewrite m-bracket clause))
      ((list* 'comma clauses)   (rewrite m-comma clauses))
      ((list 'blk clause)     (rewrite m-block clause))
      ((list '// str) (rewrite m-comment str))
      ((list 'raw-string str) (rewrite m-raw-string str))

      ((when (char= #\@ (aref (symbol-name val) 0)) (type symbol val))
       (rewrite m-alias-this val))

      ((type atom val) (values (m-glue s env (list val)) (type-of val)))
      ((list 'var (type symbol v1)) (rewrite m-var v1))
      ((list 'var (type symbol v1) v2)  (rewrite m-var v1 v2))
      ((list* 'var _ rest)              (error "invalid variable name"))
      ((when (member op *assignments*) (list op v1 v2))  (rewrite m-assignments op v1 v2))
      ((when (member op *infixes*)     (list* op vars))  (rewrite m-infix op vars))
      ((when (member op *mono-ops*)    (list op))    (rewrite m-mono-ops op))
      ((when (member op *mono-ops*)    (list op var))    (rewrite m-mono-ops op var))
      ((when (member op *comparisons*) (list* op vars))  (rewrite m-comparison op vars))
      ((list '? thing)                 (rewrite m-exist-? thing))
      ((list '? cond then else)                 (rewrite m-? cond then else))
      ;;     ;; ((list 'set var val)                      (rewrite m-set var val))
      ((list* '-> (list* args) body)            (rewrite m-function args body))
      ((list* '=> (list* args) body)            (rewrite m-inherit-this-function args body))
      ((list* '-/> (list* args) body)           (rewrite m-procedure-function args body))
      ;; ((list* '-/ name (list* args) body)           (rewrite m-inline-function name args body))

      ((list* 'for val 'in array body)          (rewrite m-iter-array val array body))
      ((list* 'for val key 'in array body)      (rewrite m-iter-array val array body :key key))
      ((list* 'for val 'in array body)          (rewrite m-iter-array val array body))
      ((list* 'for val key 'in array body)      (rewrite m-iter-array val array body :key key))

      ((list* 'for val 'of array body)          (rewrite m-iter-obj val array body))
      ((list* 'for val key 'of array body)      (rewrite m-iter-obj val array body :key key))
      ((list* 'for 'own val 'of array body) (rewrite m-iter-obj val array body :own t))
      ((list* 'for 'own val key 'of array body) (rewrite m-iter-obj val array body :key key :own t))
      ((list* 'for (list begin condition next) body) (rewrite m-for begin condition next body))

      ((list 'if cond then)                     (rewrite m-if cond then))
      ((list 'if cond then else)                (rewrite m-if cond then else))
      ((list 'if? thing then)                     (rewrite m-if-? thing then))
      ((list 'if? thing then else)                (rewrite m-if-? thing then else))
      ((list '?= to from)                     (rewrite m-assign-if-exist to from))

      ((list 'while cond then) (rewrite m-while cond then))
      ((list* 'while _) (error "invalid while statement"))
      ((list 'do then 'while cond) (rewrite m-do-while cond then))
      ((list* 'do _)  (error "invalid do-while statement"))

      ((list* 'switch val conditions)	      (rewrite m-switch val conditions))
      ((list* 'case val body)	      (rewrite m-case val body))
      ((list* 'cases (list* vals) body)	      (rewrite m-cases vals body))
      ((list* 'cases _ body)	      (error "invalid cases"))
      ((list* 'default body)	      (rewrite m-default body))
      
      ((list 'try body 'catch (list error-var) error)              (rewrite m-try body error-var error))
      ((list 'try body 'catch (list error-var) error 'finally fin) (rewrite m-try body error-var error fin))
      ((list* 'try body _)                   (error "invalid try-catch statement"))

      ((list* obj '> (direct-specifier child) more)  (rewrite m-direct-accessor obj child more))
      ((list* obj '> more)                     (rewrite m-accessor obj more))
      ((list* obj '? more)                      (rewrite m-exist-accessor obj more))
      ((list* obj '>> more)                     (rewrite m-prototype-accessor obj more))

      ((list* (type keyword key) rest)          (rewrite m-obj lst))
      ((list 'quote (list* elems)) (rewrite m-array elems))
      ((list* 'global sentences) (rewrite m-global sentences))

      ((list* (type atom op) arguments)         (rewrite m-function-call op arguments))
      ((list) (values nil 'null))
      ((list* sentences)                        (rewrite m-sentences sentences))
      )))

(defmacro maca (&body body)
  `(progn
     (multiple-value-bind (value type)
	 ,(if (= (length body) 1)
	      `(m-compile t (list (make-closure)) ',@body)
	      `(m-compile t (list (make-closure)) '(global ,@body)))
       (declare (ignore value))
       (format t "~%")
       type)))
