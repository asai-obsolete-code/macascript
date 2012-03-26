(in-package :maca)

(defparameter *customs* nil)
(defparameter *miscellaneous*
  '( ;;for the evaluation of single atom at the top level
	((type atom val) (values (m-glue s env nil (list val)) (type-of val))) 
	((list* (type atom op) arguments)         (rewrite m-function-call op arguments))
	((list) (values nil 'null))
	((list* sentences)                        (rewrite m-sentences sentences))))

(defparameter *non-sentence-ops*
  '(var if for switch while do))

(defmaca m-sentences (sents)
  `(glue ,@(mapcar
			#'(lambda (sent) 
				(ifmatch (when (member keyword *non-sentence-ops*)
						   (list* keyword _)) sent
					sent
					`(glue ,sent semicolon newline)))
			sents)))

(recompile)
(setf *recompile-compiler* t)