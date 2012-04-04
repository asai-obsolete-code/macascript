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
	 (rewrite m-alias-this t val))
	((list* obj '> (direct-specifier child) more)
	 (rewrite m-direct-accessor nil obj child more))
	((list* obj '> more)
	 (rewrite m-accessor t obj more))
	((list* obj '? more)
	 (rewrite m-exist-accessor nil obj more))
	((list* obj '>> more)
	 (rewrite m-prototype-accessor nil obj more))
	((list* (type keyword key) rest)
	 (rewrite m-obj nil lst))
	((list 'quote (list* elems))
	 (rewrite m-array nil elems))))

(defmaca m-alias-this (val)
  (let ((property (make-symbol (subseq (symbol-name val) 1))))
	`(this > ,property)))

(defmaca (m-array :environment env) (args)
  (with-set-temps-in-list (env args contents)
	`(bracket (comma ,@contents))))

(defmaca (m-obj :environment env) (key-value-plist)
  (if (oddp (length key-value-plist))
      (error "invalid object literal")
	  (let ((alist (plist-alist key-value-plist)))
		(with-set-temps-in-list (env (mapcar #'cdr alist) contents)
		  `(blk (comma ,@(mapcar #'(lambda (key content)
									 `(glue ,key colon ,content))
								 (mapcar #'car alist) contents)))))))

(defmaca (m-direct-accessor :environment env
							:return tmp) (obj child more)
  (if tmp
	  `(var ,tmp (,obj > ',child ,@more))
	  (with-set-temp env (obj child)
		`(glue ,obj (bracket ,child)
			   ,(when more
					  `(nil ,@more))))))

(defmaca (m-accessor :return tmp :environment env) (obj accessor)
  ;; (if tmp
  ;; 	  ;;(break "~a" tmp)
  ;; 	  (with-set-temp env (obj)
  ;; 		`((= ,tmp (glue ,obj period
  ;; 						,(if (cdr accessor)
  ;; 							 accessor
  ;; 							 (car accessor))))))
  (with-set-temp env (obj)
	`(glue ,obj period
		   ,(if (cdr accessor)
				accessor
				(car accessor)))));)
  
(defmaca (m-exist-accessor :return tmp
						   :environment env) (obj accessor)
  (let ((ref (gensym))
		(child (car accessor)))
	(if tmp
		`(var ,tmp (,obj ? ,@accessor))
		(with-set-temp env (obj child)
		  `(? (!= (paren (= ,ref (,obj > ,child))) null)
			  ,(if (cdr accessor)
				   `(,ref . ,(cdr accessor))
				   ref)
			  (void 0))))))

(defmaca m-prototype-accessor (obj accessor)
  `(,obj > prototype ,@(when accessor `( > ,@accessor))))
