(in-package :maca)

;; -----------------------------
;; math and core operators

(defparameter *core-ops*
  '(((list 'var (type symbol v1))     (rewrite m-var v1))
	((list 'var (type symbol v1) v2)  (rewrite m-var v1 v2))
	((list* 'var _ rest)              (error "invalid variable name"))
	((when (member op *assignments*) (list op v1 v2))  (rewrite m-assignments op v1 v2))
	((when (member op *infixes*)     (list* op vars))  (rewrite m-infix op vars))
	((when (member op *mono-ops*)    (list op))        (rewrite m-mono-ops op))
	((when (member op *mono-ops*)    (list op var))    (rewrite m-mono-ops op var))
	((when (member op *comparisons*) (list* op vars))  (rewrite m-comparison op vars))))

(defmaca m-var (var &optional val)
  (push var +variables+)
  (if val
      `(= ,var ,val)
      nil))

(defparameter *assignments*
  '(= += -= *= /= <<= >>= >>>= &= ^= |\|=| and= or= not=))

(defmaca m-assignments (op to from)
  `(glue ,to space ,op space (value ,from)))

(defparameter *infixes* 
  '(+ - * / % << >> >>> in && |\|\|| and or))

(defmaca m-infix (op vars)
  `(paren (glue (value ,(car vars)) ,op 
				(value ,(if (third vars) 
							`(,op ,@(cdr vars))
							(cadr vars))))))

(defparameter *comparisons* 
  '(== != === !== > < >= <=))

(defmaca m-comparison (op vars)
  `(glue (paren (glue (value ,(first vars)) ,op (value ,(second vars))))
		 ,@(if (third vars)
			   `(&& (,op ,@(cdr vars))))))

(defparameter *mono-ops*
  '(++ -- ^ ~ ! not
    new set get typeof instanceof
    void delete continue))
(defmaca m-mono-ops (op &optional val)
  `(paren (glue ,op space ,@(when val (list 'value val)))))
