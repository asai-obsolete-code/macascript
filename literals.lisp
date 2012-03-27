(in-package :maca)

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

(defmaca m-alias-this (val)
  (let ((property (make-symbol (subseq (symbol-name val) 1))))
	`(this > ,property)))

(defmaca m-array (args)
  `(bracket (comma ,@(mapcar #'value args))))

(defmaca m-obj (key-value-plist)
  (if (oddp (length key-value-plist))
      (error "invalid object literal")
	  `(blk (comma ,@(mapcar #'(lambda (cons)
								 `(glue ,(car cons) colon (value ,(cdr cons))))
							 (plist-alist key-value-plist))))))

(defmaca m-direct-accessor (obj child more)
  `(glue (value ,obj) (bracket (value ,child))
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
