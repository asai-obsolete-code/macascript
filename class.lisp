



(in-package :maca)


(ifmatch (list 'a 'b c) '(a b 3) c)



(defparameter *class*
  '(((list* 'class class-name
	  (list* superclasses)
	  (list* options)
	  slot-descriptions)
	 (rewrite m-class class-name superclasses slot-descriptions))))

(defun keywordize (symbol)
  (make-keyword (symbol-name symbol)))

(defmaca (m-class :environment env
				  :return temp)
	(class-name superclasses slot-descriptions options)
  (declare (type symbol class-name))
  (declare (type (list symbol) slot-descriptions))
  ;; (when (notevery #'symbolp superclasses)
  ;; 	(error "invalid superclass specifier: ~a" superclasses))
  (destructuring-bind (&key static) options
	(declare (ignore static))
	`(var ,class-name
		  ,(loop for slot in slot-descriptions
			  if (symbolp slot)
			  append (list (keywordize slot) 'undefined)
			  else
			  append 
				(destructuring-bind (name definition &key setter getter (readable t) (writable t)) slot
				  (let ((script (list (keywordize name) (or definition 'undefined))))
					(appendf script
							 (if setter
								 (if writable
									 `((set ,(keywordize name)) (-> (,name) ,@setter))
									 `(-> ()
										  (// ,(format nil "the slot ~a is not writable, setter ignored." name))
										  (throw (new (-error ,(format nil "~a is not writable" name))))))
								 (if (not writable)
									 `((set ,(keywordize name))
									   (-> () (throw (new (-error ,(format nil "~a is not writable" name))))))
									 nil))
							 (if getter
								 (if readable
									 `((get ,(keywordize name)) (-> () ,@getter))
									 `((get ,(keywordize name))
									   (-> () 
										   (// ,(format nil "the slot ~a is not readable, getter ignored." name))
										   (throw (new (-error ,(format nil "~a is not readable" name)))))))
								 (if readable
									 nil
									 `((get ,(keywordize name))
									   (-> () (throw (new (-error ,(format nil "~a is not readable" name)))))))))
					script))))))