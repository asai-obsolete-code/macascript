(in-package :maca)

;; --------------------------------
;; conditional expression

(defparameter *conditions*
  '(((list '? cond then else)      (rewrite m-? t cond then else))
	((list 'if cond then)          (rewrite m-if nil cond then))
	((list 'if cond then else)     (rewrite m-if nil cond then else))
	((list 'if? thing then)        (rewrite m-if-? t thing then))
	((list 'if? thing then else)   (rewrite m-if-? t thing then else))
	((list '?= to from)            (rewrite m-assign-if-exist t to from))

	((list* 'while (list cond) then)     (rewrite m-while nil cond then))
	((list* 'while _)            (error "invalid while statement"))
	((list 'do then 'while cond) (rewrite m-do-while nil cond then))
	((list* 'do _)               (error "invalid do-while statement"))

	((list* 'with-label (list name) body) (rewrite m-label nil name body))
	((list* 'with-label _) (error "invalid label name"))

	((list* 'switch val conditions)	  (rewrite m-switch nil val conditions))
	((list* 'case val body)	          (rewrite m-case nil val body))
	((list* 'cases (list* vals) body) (rewrite m-cases nil vals body))
	((list* 'cases _ body)	          (error "invalid cases"))
	((list* 'default body)	          (rewrite m-default nil body))
      
	((list 'try body 'catch (list error-var) error)
	 (rewrite m-try nil body error-var error))
	((list 'try body 'catch (list error-var) error 'finally fin)
	 (rewrite m-try nil body error-var error fin))
	((list* 'try body _)
	 (error "invalid try-catch statement"))))

(defmaca m-? (condition then &optional (else 'undefined))
  `(paren (glue (paren (value ,condition))
		?
		(paren (value ,then))
		colon
		(paren (value ,else)))))

(defmaca m-if-? (thing then &optional else)
  `(if (and (!== ,thing null)
			(!== (typeof ,thing) "undefined"))
	   ,then
	   ,else))

(defmaca m-assign-if-exist (to from)
  `(if? ,to (= ,to ,from)))

(defmaca (m-if :environment env
			   :return temp) (condition then &optional else)
  (if temp
	  `(if ,condition
		   ,(1-or-2-line-set-temp then temp)
		   ,(1-or-2-line-set-temp else temp))
	  (with-set-temp env (condition)
		`(glue if (paren ,condition) (blk ,then)
			   ,@(when else `(else (blk ,else)))))))

(defmaca (m-while :environment env
				  :return temp) (condition body)
  (with-set-temp env (condition)
	`(glue while (paren ,condition)
		   (blk ,(if temp
					 (1-or-2-line-set-temp body temp)
					 body)))))

(defmaca (m-do-while :environment env
					 :return temp) (condition body)
  (with-set-temp env (condition)
	`(glue do (blk ,(if temp
						(1-or-2-line-set-temp body temp)
						body))
		   while
		   (paren ,condition))))

(defmaca (m-label :environment env
				  :return temp) (name body)
  `(glue ,name colon (blk ,(if temp
							   (1-or-2-line-set-temp body temp)
							   body))))

;; todo: make it return a value
(defmaca (m-switch :environment env
				   :return temp) (val conditions)
  (if temp
	  `(switch (,val)
		 ,@(mapcar #'(lambda (condition)
					   (m-compile env condition :return temp))
				   conditions))
	  (with-set-temp env (val)
		`(glue switch (paren ,val)
			   (blk ,conditions)))))

(defmaca (m-cases :environment env
				  :return temp) (vals body)
  `(glue ,@(reduce #'append 
				   (mapcar #'(lambda (val)
							   `((newline-and-indent) case space ,val colon))
						   vals))
		 (,@(if temp
				(1-or-2-line-set-temp body temp)
				body)
			(break))))

(defmaca (m-case :environment env
				  :return temp) (val body)
  `(glue case space ,val colon 
		 (,@(if temp
				(1-or-2-line-set-temp body temp)
				body)
			(break))))

(defmaca (m-default :environment env
				  :return temp) (body)
  `(glue default colon ,(if temp
							(1-or-2-line-set-temp body temp)
							body)))


;; -----------------------------
;; try/catch expression

(defmaca (m-try :environment env
				:return temp) (body error-var error &optional finally)
  `(glue try
		 (blk ,(if temp (1-or-2-line-set-temp body temp) body))
		 catch (paren ,error-var)
		 (blk ,(if temp (1-or-2-line-set-temp error temp) error))
		 ,@(when finally
				 `(finally (blk ,(if temp (1-or-2-line-set-temp finally temp) finally))))))
