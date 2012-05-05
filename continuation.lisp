
(in-package :maca)
(defparameter *cont*
  '(((list* 'with-cc (list cc) body) (rewrite m-with-cc cc body))))

;; (defmaca (m-with-cc :is-value t) (cc body)
;;   ;;(break "cc-name:~a~%body:~a" cc body)
;;   (with-gensyms (passing)
;; 	`((paren (var ,passing
;; 				  (-/> ()
;; 					   (try
;; 						((var ,cc (-/> (value) (throw value))))
;; 						catch (x)
;; 						((return x)))
;; 					   ,@body
;; 					   ))) > (call this))))


(defmaca (m-with-cc :environment env :is-value t) (cc body)
  `(-> (,cc) ,@body))
