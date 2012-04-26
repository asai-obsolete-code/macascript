(in-package :maca)

;; -----------------------------
;; array and object literals

(defpattern direct-specifier (child)
  `(or (list 'quote ,child)
       (type string ,child)
       (type number ,child)
       (type keyword ,child)))

(defparameter *objects*
  '(((when (char= #\@ (aref (symbol-name val) 0))
	   (type symbol val))
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

(defmaca (m-alias-this :is-value t) (val)
  (let ((property (make-symbol (subseq (symbol-name val) 1))))
	`(this > ,property)))

(defmaca (m-array :environment env :is-value t) (args)
  (with-set-temps-in-list (env args contents)
	`(bracket (comma ,@contents))))

(defmaca (m-obj :environment env :is-value t) (key-value-plist)
  (if (oddp (length key-value-plist))
      (error "invalid object literal")
	  (let ((alist (plist-alist key-value-plist)))
		(with-set-temps-in-list (env (mapcar #'cdr alist) contents)
		  `(blk (comma ,@(mapcar #'(lambda (key content)
									 `(glue ,key colon ,content))
								 (mapcar #'car alist) contents)))))))

(defmaca (m-direct-accessor :environment env
							:return tmp
							:is-value t) (obj child more)
   (with-set-temp env (obj child)
	 `(glue ,obj (bracket ,child)
			,(when more `(nil ,@more)))))

(defmaca (m-accessor :return tmp
					 :environment env
					 :is-value t) (obj accessor)
  (multiple-value-bind (body is-value type)
  	  (m-compile (list (make-closure)) (car accessor))
  	(declare (ignore body is-value))
  	(unless (member type (list 'm-function-call 'compiled 'atom))
  	  (error "invalid accessor: ~a" (car accessor))))
  (with-set-temp env (obj)
	`(glue ,obj period
		   ,(if (cdr accessor)
				accessor
				(car accessor)))))

(defmaca (m-exist-accessor :return tmp
						   :environment env) (obj accessor)
  (with-gensyms (exist-ref)
	(let ((child (car accessor)))
	  (if tmp
		  `(var ,tmp (,obj ? ,@accessor))
		  (with-set-temp env (obj child)
			`(? (!= (paren (= ,exist-ref (,obj > ,child))) null)
				,(if (cdr accessor)
					 `(,exist-ref . ,(cdr accessor))
					 exist-ref)
				(void 0)))))))

(defmaca m-prototype-accessor (obj accessor)
  `(,obj > prototype ,@(when accessor `( > ,@accessor))))
