(in-package :maca)

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
  `(glue (paren (value ,condition))
		 ?
		 (paren (value ,then))
		 colon
		 (paren (value ,else))))

(defmaca m-if-? (thing then &optional else)
  (if need-value
	  `(value (if (and (!== ,thing null)
					   (!== (typeof ,thing) "undefined"))
				  ,then
				  ,else))
	  `(if (and (!== ,thing null)
				(!== (typeof ,thing) "undefined"))
		   ,then
		   ,else)))

(defmaca m-assign-if-exist (to from)
  (value-if need-value
			`(if? ,to
				  (= ,to ,from))))

(defun return-last (sents temp-val)
  `(,@(butlast sents)
	(= ,temp-val ,@(last sents))))

(defmacro with-temp ((name) &body body)
  `(let ((,name (gensym "temp")))
	 `(paren (comma ((var ,,name) ,,@body) ,,name))))

(defun atom-or-op (arg)
  (or (atom arg) (atom (car arg))))

(defun 1-or-2-line (arg temp)
  (if (atom-or-op arg)
	  `((= ,temp ,arg))
	  (return-last arg temp)))

(defmaca m-if (condition then &optional else)
  (if need-value
	  (with-temp (temp)
		`(if ,condition
			 ,(1-or-2-line then temp)
			 ,(when else (1-or-2-line else temp))))
	  `(glue if (paren ,condition) (blk ,then)
 			 ,@(when else `(else (blk ,else))))))

(defmaca m-while (condition body)
  (if need-value
	  (with-temp (temp)
		`(while ,condition
		   ,(1-or-2-line body temp)))
	  `(glue while (paren ,condition)
			 (blk ,body))))

(defmaca m-do-while (condition body)
  (if need-value
	  (with-temp (temp)
		`(do ,(1-or-2-line body temp) while ,condition))
	  `(glue do (blk ,body)
			 while
			 (paren ,condition))))

(defmaca m-label (name body)
  `(glue ,name colon (blk ,body)))


;; todo: make it return a value
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
