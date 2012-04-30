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

	((list* 'while (list cond) then)     (rewrite m-while cond then))
	((list* 'while _)            (error "invalid while,syntax: (while (cond) then*)"))
	((list 'do then 'while cond) (rewrite m-do-while cond then))
	((list* 'do _)               (error "invalid do-while,syntax:(do body* while cond)"))

	((list* 'with-label (list name) body) (rewrite m-label name body))
	((list* 'with-label _) (error "invalid label name"))

	((list* 'switch val conditions)	  (rewrite m-switch val conditions))
      
	((list 'try body 'catch (list error-var) error)
	 (rewrite m-try body error-var error))
	((list 'try body 'catch (list error-var) error 'finally fin)
	 (rewrite m-try body error-var error fin))
	((list* 'try body _)
	 (error "invalid try-catch statement"))))

(defmaca (m-? :is-value t) (condition then &optional (else 'undefined))
  `(paren (glue (paren ,condition)
				 ?
				 (paren ,then)
				 colon
				 (paren ,else))))

(defmaca (m-if-? :is-value t) (thing then &optional else)
  `(if (and (!== ,thing null)
			(!== (typeof ,thing) "undefined"))
	   ,then
	   ,else))

(defmaca (m-assign-if-exist :is-value t) (to from)
  `(if? ,to (= ,to ,from)))

(defmaca (m-if :environment env
			   :return temp) (condition then &optional else)
  (if temp
	  `(if ,condition
		   ,(1-or-2-line-set-temp then temp)
		   ,(1-or-2-line-set-temp else temp))
	  ;; (values `(? ,condition
	  ;; 			  (comma ,then)
	  ;; 			  (comma ,else)) t)
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
  `(glue ,name colon 
		 (blk ,(if temp
				   (1-or-2-line-set-temp body temp)
				   body))))

;; todo: make it return a value
(defmaca (m-switch :environment env
				   :return temp) (val conditions)
  (with-set-temp env (val)
	`(glue switch (paren ,val) (blk
	   (glue ,@(mapcar 
				#'(lambda (condition)
					(match condition
					  ((list* :case (list* vals) body) (m-case   env temp  vals body))
					  ((list* :default body)	       (m-default env temp  body))
					  (_ (error "bad switch condition declaration"))))
				conditions))))))

(defmaca (m-case :environment env
				  :return temp) (vals body)
  (let ((body2 (1-or-2-line body)))
	`(glue ,@(reduce #'append 
					 (mapcar #'(lambda (val)
								 `((newline-and-indent) case space ,val colon))
							 vals))
		   (blk (,@(if temp
					   (1-or-2-line-set-temp body2 temp)
					   body2)
				   break)))))

(defmaca (m-default :environment env
				  :return temp) (body)
  (let ((body2 (1-or-2-line body)))
	`(glue (newline-and-indent)
		   default colon
		   (blk (,@(if temp
					   (1-or-2-line-set-temp body2 temp)
					   body2)
				   break)))))

;; -----------------------------
;; try/catch expression

(defmaca (m-try :environment env
				:return temp)
	(body error-var error &optional finally)
  `(glue try
		 (blk ,(if temp (1-or-2-line-set-temp body temp) body))
		 catch (paren ,error-var)
		 (blk ,(if temp (1-or-2-line-set-temp error temp) error))
		 ,@(when finally
				 `(finally (blk 
							,(if temp
								 (1-or-2-line-set-temp finally temp)
								 finally))))))
