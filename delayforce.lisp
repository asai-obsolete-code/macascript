

(defvar *delay*
  '(((list 'delay delayed) (rewrite m-delay delayed))
	((list 'force delayed) (rewrite m-force delayed))))

(defmaca m-delay (delayed)
  `(:forced undefined 
	:closure (-> () (= @forced ,delayed))))

(defmaca m-force (delayed)
  `(or (,delayed ? closure > (call)) ,delayed))