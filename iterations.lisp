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

(defmaca m-for (begin condition next body)
  (if need-value
	  (let ((result (gensym "result")))
		`((var ,result '())
		  (for (,begin ,condition ,next)
			,@(butlast body)
			(,result > (push (need-value ,@(last body)))))))
	  `(glue for (paren (glue ,begin semicolon ,condition semicolon ,next))
			 (blk ,body))))

(defmaca m-iter-array (val array body &key (key (gensym)))
  (if need-value
	  (with-temp (result)
		`((= ,result '())
		  (for ,val ,key in ,array
			   ,(if (atom-or-op body)
					`(,result > (push (value ,body)))
					`(,@(butlast body)
						(,result > (push (value ,@(last body)))))))))
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
			,@body)))))

(defmaca (m-iter-obj :environment env
					 :temporary-return temp
					 :stream s)
	(val obj body &key (key (gensym)) own)
  (if need-value
	  `((= ,temp (new (|Object|)))
		(for ,val ,key of ,obj
			 ,(if (atom-or-op body)
				  `(= (,result > ',key) (value ,body))
				  `(,@(butlast body)
					  (= (,result > ',key) (value ,@(last body))))))))
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
				  ,@body)))))))
