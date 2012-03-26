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
	(indent . #\Tab)
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
;; core macros

(defmacro recompile ()
  `(defun m-compile (s env lst)
	 (macrolet ((rewrite (name &rest args)
				  `(values (,name s env ,@args) ',name)))
	   (match lst
		 ,@(copy-tree *customs*)
		 ,@(copy-tree *core-ops*)
		 ,@(copy-tree *functions*)
		 ,@(copy-tree *iteraters*)
		 ,@(copy-tree *conditions*)
		 ,@(copy-tree *objects*)
		 ,@(copy-tree *fundamentals*)
		 ,@(copy-tree *miscellaneous*)))))

(defun show-patterns ()
  (append *customs*
		  *core-ops*
		  *functions*
		  *iteraters*
		  *conditions*
		  *objects*
		  *fundamentals*
		  *miscellaneous*))

(defparameter *recompile-compiler* nil)

;; (compile-matcher lst)

(defstruct closure
  (indentation 0)
  (arguments nil)
  (variables nil)
  (initializations nil)
  (inline-lambda nil))

;; you can perform some bad behavior on "env". it is intentional
(defmacro defmaca (name args &body body)
  (with-gensyms (s)
    `(progn
	   (defun ,name (,s env ,@args)
		 (symbol-macrolet ((+cl+ (car env))
						   (+indentation+ (closure-indentation (car env)))
						   (+variables+ (closure-variables (car env)))
						   (+initializations+ (closure-initializations (car env)))
						   (+inline-lambda+ (closure-inline-lambda (car env)))
						   (+arguments+ (closure-arguments (car env))))
		   (m-compile ,s env (progn ,@body))))
	   ,@(if *recompile-compiler*
			 `((format t "recompiling m-compile ...")
			   (recompile)
			   (format t "done.~%"))))))
  
;; --------------------------------
;; fundamentals

;; these operators are just meant to be used by the compiler
;; don't use it
(defparameter *fundamentals*
  '(((list* 'glue clauses)              (rewrite m-glue clauses))
	((list 'paren (list 'paren clause)) (rewrite m-redundant-paren clause))
	((list 'paren clause)               (rewrite m-paren clause))
	((list 'bracket clause)             (rewrite m-bracket clause))
	((list* 'comma clauses)             (rewrite m-comma clauses))
	((list 'blk clause)                 (rewrite m-block clause))
	((list '// str)                     (rewrite m-comment str))
	((list 'raw-string str)             (rewrite m-raw-string str))))

(defun m-glue (s env args)
  (format s "~{~a~}"
		  (mapcar
		   #'(lambda (arg)
			   (let ((true-arg (or (cdr (assoc arg *aliases*)) arg)))
				 (typecase true-arg
				   (null "") 
				   (cons (m-compile nil env true-arg))
				   ;;						   (atom (m-compile nil env true-arg))
				   (string (format nil "\"~a\"" true-arg))
				   (symbol (let ((str (symbol-name true-arg)))
							 (cond
							   ((char= (aref str 0) #\@) (m-compile nil env true-arg))
							   ((every #'(lambda (c) (or (not (both-case-p c))
														 (upper-case-p c)))
									   str)
								 (string-downcase str))
							   (t str))))
				   (t (format nil "~a" true-arg)))))
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

(defun m-comment (s env string)
  (declare (ignore env))
  (format s "/* ~a */~%" string))
(defun m-raw-string (s env string)
  (declare (ignore env s))
  string)

;; -----------------------------
;; function and function calls

(defparameter *functions*
  '(((list* 'global sentences)          (rewrite m-global sentences))
	((list* '-> (list* args) body)      (rewrite m-function args body))
	((list* '=> (list* args) body)      (rewrite m-inherit-this-function args body))
	((list* '-/> (list* args) body)     (rewrite m-procedure-function args body))
	((list* '-/ name (list* args) body) (rewrite m-inline-function name args body))))

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
  (let ((raw (compile-in-advance env body)))
	;;(break "~a" +cl+)
    `(glue ,(aif +variables+ `((glue var space (comma ,@(uniquify it)))))
		   (,@(nreverse +initializations+))
		   ,raw)))


(defmaca m-function (args body)
  (check-args args
    `(glue function (paren (comma ,@args))
		   ,(let* ((cl (make-closure :arguments args))
				   (raw (compile-in-advance (cons cl env)
											`(,@(butlast body)
												(return ,(car (last body)))))))
				  `(blk (glue ,(aif +variables+
									`((glue var space (comma ,@(uniquify it)))))
							  (,@(nreverse +initializations+))
							  ,raw))))))

(defmaca m-procedure-function (args body)
  (check-args args
    `(glue function (paren (comma ,@args))
		   ,(let* ((cl (make-closure :arguments args))
				   (raw (compile-in-advance (cons cl env) body)))
				  `(blk (glue ,(aif +variables+ `((glue var space (comma ,@(uniquify it)))))
							  (,@(nreverse +initializations+))
							  ,raw))))))

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
     (car env)
     this 'this 
     fn `(-> ,args ,@(subst this 'this body)))
    fn))

(defmaca m-inline-function (name args body)
  (setf (getf (closure-inline-lambda (car env)) name) (cons args body))
  nil)

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

(defmaca m-function-call (op args)
  (aif (expand-inline op args env)
       it
       `(glue ,op (paren (comma ,@args)))))

(defparameter *non-sentence-ops*
  '(var for switch while do))

(defmaca m-sentences (sents)
  `(glue ,@(mapcar
			#'(lambda (sent) `(glue ,sent semicolon newline))
			sents)))
  
;; -----------------------------
;; iteration

(defparameter *iteraters*
  '(((list* 'for val 'in array body)          (rewrite m-iter-array val array body))
	((list* 'for val key 'in array body)      (rewrite m-iter-array val array body :key key))
	((list* 'for val 'in array body)          (rewrite m-iter-array val array body))
	((list* 'for val key 'in array body)      (rewrite m-iter-array val array body :key key))
	((list* 'for val 'of array body)          (rewrite m-iter-obj val array body))
	((list* 'for val key 'of array body)      (rewrite m-iter-obj val array body :key key))
	((list* 'for 'own val 'of array body) (rewrite m-iter-obj val array body :own t))
	((list* 'for 'own val key 'of array body) (rewrite m-iter-obj val array body :key key :own t))
	((list* 'for (list begin condition next) body) (rewrite m-for begin condition next body))))

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
		(= ,val (,ref > ',key))
		,@body))))

(defmaca m-iter-obj (val obj body &key (key (gensym)) own)
  (let ((ref (gensym "ref")))
    `((var ,key)
      (var ,val)
      (var ,ref ,obj)
      (glue for (paren (in ,key ,obj))
			(blk 
			 ((= ,val (,ref > ',key))
			  ,@(when own 
					  (let ((global (car (last env))))
						(pushnew '__hasprop (closure-variables global))
						(pushnew `(= __hasprop (|Object| >> |hasOwnProperty|))
								 (closure-initializations global))
						;;(break "~a" global)
						)
					  `((if (! (__hasprop > (call ,val ,key))) (continue))))
			  ,@body))))))

;; --------------------------------
;; conditional expression

(defparameter *conditions*
  '(((list '? cond then else)      (rewrite m-? cond then else))
	((list 'if cond then)          (rewrite m-if cond then))
	((list 'if cond then else)     (rewrite m-if cond then else))
	((list 'if? thing then)        (rewrite m-if-? thing then))
	((list 'if? thing then else)   (rewrite m-if-? thing then else))
	((list '?= to from)            (rewrite m-assign-if-exist to from))

	((list 'while cond then)     (rewrite m-while cond then))
	((list* 'while _)            (error "invalid while statement"))
	((list 'do then 'while cond) (rewrite m-do-while cond then))
	((list* 'do _)               (error "invalid do-while statement"))

	((list* 'with-label (list name) body) (rewrite m-label name body))
	((list* 'with-label _) (error "invalid label name"))

	((list* 'switch val conditions)	  (rewrite m-switch val conditions))
	((list* 'case val body)	          (rewrite m-case val body))
	((list* 'cases (list* vals) body) (rewrite m-cases vals body))
	((list* 'cases _ body)	          (error "invalid cases"))
	((list* 'default body)	          (rewrite m-default body))
      
	((list 'try body 'catch (list error-var) error)
	 (rewrite m-try body error-var error))
	((list 'try body 'catch (list error-var) error 'finally fin)
	 (rewrite m-try body error-var error fin))
	((list* 'try body _)
	 (error "invalid try-catch statement"))))

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
		 ,@(when else `(else (blk ,else)))))

(defmaca m-while (condition body)
  `(glue while (paren ,condition)
		 (blk ,body)))

(defmaca m-do-while (condition body)
  `(glue do (blk ,body)
		 while
		 (paren ,condition)))

(defmaca m-label (name body)
  `(glue ,name colon
		 (blk ,body)))

(defmaca m-switch (val conditions)
  `(glue switch (paren ,val)
		 (blk ,conditions)))
(defmaca m-cases (vals body)
  `(glue ,@(mapcan #'(lambda (val) (list 'newline 'case 'space val 'colon)) vals)
		 (,@body
		  (break))))
(defmaca m-case (val body)
  `(glue case space ,val colon 
		 (,@body
		  (break))))
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
;; math and core operators

(defparameter *core-ops*
  '(((list 'var (type symbol v1)) (rewrite m-var v1))
	((list 'var (type symbol v1) v2)  (rewrite m-var v1 v2))
	((list* 'var _ rest)              (error "invalid variable name"))
	((when (member op *assignments*) (list op v1 v2))  (rewrite m-assignments op v1 v2))
	((when (member op *infixes*)     (list* op vars))  (rewrite m-infix op vars))
	((when (member op *mono-ops*)    (list op))    (rewrite m-mono-ops op))
	((when (member op *mono-ops*)    (list op var))    (rewrite m-mono-ops op var))
	((when (member op *comparisons*) (list* op vars))  (rewrite m-comparison op vars))))

(defmaca m-var (var &optional val)
  (push var (closure-variables (car env)))
  (if val
      `(glue (= ,var ,val) semicolon newline)
      nil))

(defparameter *assignments*
  '(= += -= *= /= <<= >>= >>>= &= ^= |\|=| and= or= not=))

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
    void delete continue))
(defmaca m-mono-ops (op &optional val)
  `(paren (glue ,op space ,@(when val (list val)))))

;; -----------------------------
;; array and object literals

(defpattern direct-specifier (child)
  `(or (list 'quote ,child)
       (type string ,child)
       (type number ,child)
       (type keyword ,child)))

(defparameter *objects*
  '(((when (char= #\@ (aref (symbol-name val) 0)) (type symbol val))
	 (rewrite m-alias-this val))
	((list* obj '> (direct-specifier child) more)
	 (rewrite m-direct-accessor obj child more))
	((list* obj '> more)
	 (rewrite m-accessor obj more))
	((list* obj '? more)
	 (rewrite m-exist-accessor obj more))
	((list* obj '>> more)
	 (rewrite m-prototype-accessor obj more))
	((list* (type keyword key) rest)
	 (rewrite m-obj lst))
	((list 'quote (list* elems))
	 (rewrite m-array elems))))

(defun plist-to-alist (plist)
  (labels ((rec (p a)
			 (if p
				 (rec (cddr p) 
					  (cons (cons (car p) (cadr p)) a))
				 a)))
    (rec plist nil)))

(defmaca m-alias-this (val)
  (let ((property (make-symbol (subseq (symbol-name val) 1))))
	`(this > ,property)))

(defmaca m-array (args)
  `(bracket (comma ,@args)))

(defmaca m-obj (key-value-plist)
  (if (oddp (length key-value-plist))
      (error "invalid object literal")
      (let* ((alist (plist-to-alist key-value-plist))
			 (pairs (mapcar #'(lambda (cons)
								`(glue ,(car cons) colon ,(cdr cons)))
							alist)))
		`(blk (comma ,@pairs)))))

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
;;   add "must-return-value" option

(defparameter *customs* nil)
(defparameter *miscellaneous*
  '( ;;for the evaluation of single atom at the top level
	((type atom val) (values (m-glue s env (list val)) (type-of val))) 
	((list* (type atom op) arguments)         (rewrite m-function-call op arguments))
	((list) (values nil 'null))
	((list* sentences)                        (rewrite m-sentences sentences))))

(recompile)
(setf *recompile-compiler* t)

(defmacro maca (&body body)
  `(progn
     (multiple-value-bind (value type)
		 (m-compile t (list (make-closure)) '(global ,@body))
       (declare (ignore value))
       (format t "~%")
       type)))
