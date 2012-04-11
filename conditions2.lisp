
(in-package :maca)
(defparameter *conditions2*
  '(
	;; unless
	((list 'unless cond then)      (rewrite m-unless cond then))
	;; reversed if and unless
	((list then 'if cond)          (rewrite m-if cond then))
	((list then 'unless cond)      (rewrite m-unless cond then))
	))

(defmaca (m-unless :inherit-return-value t) (condition then)
  `(if (not ,condition) ,then))

