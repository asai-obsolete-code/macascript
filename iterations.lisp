(in-package :maca)
  
;; -----------------------------
;; iteration

(defparameter *iteraters*
  '(((list* 'for val 'in array body)               (rewrite m-iter-array val array body))
	((list* 'for val key 'in array body)           (rewrite m-iter-array val array body :key key))
	((list* 'for val 'in array body)               (rewrite m-iter-array val array body))
	((list* 'for val key 'in array body)           (rewrite m-iter-array val array body :key key))
	((list* 'for val 'of array body)               (rewrite m-iter-obj val array body))
	((list* 'for val key 'of array body)           (rewrite m-iter-obj val array body :key key))
	((list* 'for 'own val 'of array body)          (rewrite m-iter-obj val array body :own t))
	((list* 'for 'own val key 'of array body)      (rewrite m-iter-obj val array body :key key :own t))
	((list* 'for (list begin condition next) body) (rewrite m-for begin condition next body))))

(defmaca (m-for :environment env :return result) (begin condition next body)
  (if result
	  `((var ,result '())
		(for (,begin ,condition ,next)
		  ,@(butlast body)
		  (,result > (push ,@(last body)))))
	  `(glue for (paren (glue ,begin semicolon ,condition semicolon ,next))
			 (blk ,body))))

(defmaca (m-iter-array :environment env :return result)
	(val ary body &key (key (gensym)))
  (if result
	  `((= ,result '())
		(for ,val ,key in ,ary
			 ,(if (atom-or-op body)
				  `(,result > push ,body)
				  `(,@(butlast body)
					  (,result > push ,@(last body))))))
	  (with-set-temp env (ary)
		(with-gensyms (len ref)
		  `((var ,key)
			(var ,val)
			(var ,ref ,ary)
			(var ,len (,ref > length))
			(for ((= ,key 0)
				  (< ,key ,len)
				  (++ ,key))
			  (= ,val (,ref > ',key))
			  ,@body))))))

(defmaca (m-iter-obj :environment env :return result)
	(val obj body &key (key (gensym)) own)
  (if result
	  `((= ,result (new (|Object|)))
		(for ,val ,key of ,obj
			 ,(if (atom-or-op body)
				  `(= (,result > ',key) ,body)
				  `(,@(butlast body)
					  (= (,result > ',key) ,@(last body))))))
	  (with-set-temp env (obj)
		(with-gensyms (ref)
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
									   (closure-initializations global)))
							`((if (! (__hasprop > (call ,val ,key))) (continue))))
					,@body))))))))
