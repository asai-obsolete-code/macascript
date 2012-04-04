(in-package :maca)

;; -----------------------------
;; math and core operators

(defparameter *core-ops*
  '(((list 'var (type symbol v1))     (rewrite m-var t v1))
	((list 'var (type symbol v1) v2)  (rewrite m-var t v1 v2))
	((list* 'var _ rest)              (error "invalid variable name"))
	((when (member op *assignments*) (list op v1 v2))  (rewrite m-assignments t op v1 v2))
	((when (member op *infixes*)     (list* op vars))  (rewrite m-infix t op vars))
	((when (member op *mono-ops*)    (list op))        (rewrite m-mono-ops nil op))
	((when (member op *mono-ops*)    (list op var))    (rewrite m-mono-ops t op var))
	((when (member op *comparisons*) (list* op vars))  (rewrite m-comparison t op vars))))

(defmaca m-var (var &optional val)
  (push var +variables+)
  (if val
      `(= ,var ,val)
      nil))

(defparameter *assignments*
  '(= += -= *= /= <<= >>= >>>= &= ^= |\|=| and= or= not=))

(defmaca (m-assignments :environment env) (op to from)
  (with-set-temp env (from)
	`(glue ,to space ,op space ,from)))

(defparameter *infixes* 
  '(+ - * / % << >> >>> in && |\|\|| and or))

(defmaca (m-infix :environment env) (op vars)
  (let ((arg1 (car vars))
		(arg2 (cadr vars)))
	(if (third vars)
		(with-set-temp env (arg1)
		  `(paren (glue ,arg1 space ,op space (,op ,@(cdr vars)))))
		(with-set-temp env (arg1 arg2)
		  `(paren (glue ,arg1 space ,op space ,arg2))))))

(defparameter *comparisons* 
  '(== != === !== > < >= <=))

(defmaca (m-comparison :environment env) (op vars)
  (let ((arg1 (car vars))
		(arg2 (cadr vars)))
	(if (third vars)
		(with-set-temp env (arg1 arg2)
		  `(glue (paren (glue ,arg1 ,op ,arg2)) && (,op ,arg2 ,@(cddr vars))))
		(with-set-temp env (arg2)
		  `(paren (glue ,arg1 ,op ,arg2))))))

(defparameter *mono-ops*
  '(++ -- ^ ~ ! not
    new set get typeof instanceof
    void delete continue throw return))
(defmaca (m-mono-ops :environment env) (op &optional val)
  (if val
	  (with-set-temp env (val)
		`(paren (glue ,op space ,val)))
	  `(paren ,op)))
