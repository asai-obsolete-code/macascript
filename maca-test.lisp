(defpackage maca-test
  (:use :common-lisp :cl-user :maca))
(in-package :maca-test)

;; (require 'macascript)
;; (in-package :maca)
;; (recompile)

(defun fundamentals ()
  ;; fundamentals
  (maca (glue a 2 space "string")
		(paren a)
		(paren (paren a))			;reduces redundancy automatically
		(paren (paren (paren a)))			;reduces redundancy automatically
		(comma (comma a b c) (comma a b c))
		(comma a b c)
		(stringify a)
		(blk ((a b c)))
		(blk 
		 ((blk 
		   ((a b c)))))
		(bracket a)
		;; constants and basic assignments
		undefined
		t
		"string"
		;; comments (if you want to include some comment into js)
		(// "haaa~~~~ this is comfortable")
		(// a)))

(defun assignments ()
  ;; assignments
  (maca (= number 3)
		(blk ((= number 3) (= number 3)))
		(with-label (label) 
		  (= number 3)
		  (= number 3))
		(comma (= number 3) (= number 3)))
  (maca	(var number 3))
  (maca (var number))
  ;;(maca (var (+ 2 1) 3)) ;throws error
  )


(defun ops ()
  ;; infix
  (maca (>>> number 3)
		(+ a 3)
		(+ a 3 4)
		(- a 3 4)
		(* a 3 4)
		(+ a 3 (>>> number 3))

		;; assignments
		(= a 3)
		(= a (a > b > v))
		(a > b > v)

		;; comparison
		(< number 50)
		(== 5 3)
		(== 5 3 4)
		(>= 5 3 4)

		;; mono-ops
		(new -number)
		(typeof 5)

		;; in
		(in 5 array)))

(defun functions ()
  ;; global scope(bare)
  (maca (a b c)
		(= d (- a b c))
		(var e 2)
		(+ a b c d e))

  ;; function definition
  ;; normal function: implicit return on
  ;; procedure function: returns undefined
  (maca (-/> (a)
			 (alert a)
			 (-> (b)
				 (if (< b 3)
					 ((var a (- 3 b))
					  (= b (expr a 5)))
					 ((var a (- b 3))))
				 (var c (+ a 3))
				 (alert a b)
				 a)))

  ;; inherit-this function: saves 'this' of the outer environment
  (maca (a > (click (=/> (e) 
						 (alert (this > 2))))))

  ;; inline function: they are directly expanded into the environment

  (maca (foo)
		(bar foo)
		(-/ baz (a b) (+ (some-operation a)
						 (other-operation b)))
		(-/ bud (a b) (+ (sqrt a)
						 (expr b)))

		(= foo (baz foo bar))
		(alert (baz 1 (bud foo bar)))))

;; conditional expression

(maca (-/> ()
		   ((if (> a b) a b)
			> '(if (> c d) c d)
			> content)))

;invalid accessor : you must quote the accessor key
(maca (-> ()
		  (content > (if (> a b) a b)))) 

(maca (if (a b c)
		  ((= d (- a b c)))))		;one-line
(maca (if (a b c)
		  (= d (- a b c))
		  (= d (- a b c))))		;one-line else
(maca (if (a b c)
		  ((= d (- a b c))
		   (= d (- a b c)))
		  (= e (+ a b c d))))		;multi-line then

(maca (value (if (a b c)			;multi-line then/else
				 ((= d (- a b c))
				  (= e (+ a b c d)))
				 ((= e (+ a b c d))
				  (= d (- a b c))))))

(maca (? a b c))

;; true if it's not undefined
(maca (if? a b c))

(maca (a > (if? a b c))) 				;if? should be 

;; Literals

;; array literal
(maca '(a (b c) (+ 1 2 3)))
(maca '(a (if (b c) d e) (+ 1 2 3)))

;; object literal
(maca (:a a :b (b c) :c (+ 1 2 3) :d (if (b c) d e)))

;; object accessor
(maca (obj > attibute))
(maca (obj > child > grandchild))
(maca (granpa > parent > obj > child > grandchild))
(maca (obj > (child 2) > (grandchild 3 4 5)))

(maca (obj > child 2 2 4))				;is an alias for 
(maca (obj > (child 2 2 4)))

;; however there's a difference.
;;for example, this is valid
(maca (obj > (child 2 2) > 4))			;obj.child(2,2)[4];
;; and this is not valid
(maca (obj > child 2 2 > 4))			;obj.child(2,2,>,4);




(maca (obj > '(if (even n)
			   (/ n 2)
			   (+ (/ n 2) 1))))


(maca (obj > 2 > "key"))
(maca (obj > 'key))
(maca (obj > :key))

(maca @a) ;is identical to (maca (this > a))

;; prototype accessor
(maca (obj >> child > grandchild))
(maca (obj >> (method a)))
(maca (obj >>))

;; existantial accessor
(maca (a ? b))
(maca (a ? b ? c))
(maca (a > b ? c ? d > (e) > (f) ?  g >> h))

;; try-catch-finally
(maca (try ((drink 3 "beer")
			(eat 5 pizza))
		   catch (x)
		   ((if (== x "drunkTooMuch")
				(puke)
				(take digestive)))
		   finally
		   ((pay money)
			(go home))))

;; iteration

(maca (value (for elem in ary
				  (alert elem)
				  (alert elem))))
(maca (for elem in ary
		   (alert elem)))
(maca (for elem i in ary
		   (alert elem i)))

(maca (for elem of obj
		   (alert elem)))
(maca (for elem key of ary
		   (alert elem i)))

(maca (for own elem key of ary
		   (alert elem i)))

(maca (for own elem of ary
		   (alert elem i)))

(maca (for ((var i 0)
			(< i 10)
			(++ i))
		(alert "hello!")
		(alert i)))

;; while and do-while

;; bad methodology
(maca (while (true)
		((alert "Let's do it tomorrow")
		 (alert "Let's do it tomorrow"))))

;; good methodology
(maca (do 
	   ((alert "Let's do it now")
		(alert "hush hush")
		(alert "hush hush"))
	   while (not tired)))

;; switch
(defun switch-test ()
  (maca (switch x
		  (:case (1)
			(alert x))			;1 case 1 statement
		  (:case (2)				;1 case 2 statements
			(alert 22)
			(alert 22))
		  (:case (3 4)			;2 cases 1 statements
			(alert x))
		  (:case ((sqrt 2)
				  (sqrt 3))	        ;2 cases 2 statements
			(alert x) 
			(alert x))
		  (:default
			  (alert "default")
			  (alert "default")
			  (alert "default")))
		(// "try to set a value")
		(= a (switch x
		  (:case (1)
			(alert x))			;1 case 1 statement
		  (:case (2)				;1 case 2 statements
			(alert 22)
			(alert 22))
		  (:case (3 4)			;2 cases 1 statements
			(alert x))
		  (:case ((sqrt 2)
				  (sqrt 3))	        ;2 cases 2 statements
			(alert x) 
			(alert x))
		  (:default
			  (alert "default")
			  (alert "default")
			  (alert "default"))))))