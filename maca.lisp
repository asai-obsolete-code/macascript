(ql:quickload :alexandria)
(ql:quickload :cl-match)

(defpackage maca
  (:use :common-lisp :cl-user :alexandria :cl-match :anaphora))

(proclaim '(optimize (debug 3)))

(in-package :maca)

(defparameter *reserved*
  '(break case catch continue default delete
    do else finally for function if in instanceof
    new return switch this throw try typeof var
    void while with 
    ;; future reserved
    abstract boolean byte char class const debugger
    double enum export extends final float goto implements
    import int interface long native package private protected
    public short static super synchronized throws transient volatile))

(defparameter 

;; utility

(defun not-uniquep (lst &key (test #'eq))
  (if lst
      (or (member (car lst) (cdr lst) :test test)
	  (not-uniquep (cdr lst) :test test))
      nil))

(defun not-unique (lst &key (test #'eq))
  (aif (not-uniquep lst :test test)
       (cons (car it) (not-unique (cdr lst) :test test))))
      
(defun uniquep (lst &key (test #'eq))
  (not (not-uniquep lst :test test)))

;; writer

(defun prints (&rest args)
  (format nil "~{~a~}" args))

(defmacro defw (name args &rest contents)
  `(defun ,name ,args
       (prints ,@contents)))

(defw m-var (var val)
  "var " var "=" val)

(defun m-args (&rest args)
  (let ((args (flatten args)))
    (cond ((not-uniquep args) (error (prints "duplicated argument in the list:" (not-unique args))))
	  ((some #'(lambda (a) (not (symbolp a))) args) (error (prints "invalid argument in the list")))
	  (t (format nil "~{~a~^,~}" args)))))

(defw m-fn (args body)
  "function(" (m-args args) "){\n"
  (mapcar #'m-sentences body)
  "}\n")

(defw m-sentence (sent)
  (m-compile sent) ";\n")

(defw m-sentences (sents)
  (reduce #'(lambda (prev now) (concatenate 'string prev now))
	  (mapcar #'m-sentence sents)))

(defun m-obj (key-value-lst)
  (format nil "{~%~{~a:~a~^,~%~}~%}"
	  (mapcar #'(lambda (val) (if (keywordp val) val (m-compile val)))
		  key-value-lst)))

(defmacro maca (&body body)
  `(m-compile ',body))

(maca
  (= number 42)
  (= opposite true))

(defun m-compile (lst)
  (match lst
    ((type atom val) (values val 'atom))
    ((list (or '= 'set) var val) (values (m-var (m-compile var) (m-compile val)) 'var))
    ;; ((list* '-> (list* args) body) (values (m-fn args body) 'fn))
    ;; ((list* 'for (type atom val) 'in array)
    ;;  (values (iter-array val key array) 'iter-array))
    ;; ((list* 'for (type atom val) (type atom key) (when (not eq key 'in) 'in array))
    ;;  (values (iter-array val key array) 'iter-array-key))
    ;; ((list* (type atom obj) '> accessor) (values (prints obj "." (m-compile accessor)) 'accessor))
    ;; ((list* (type atom obj) '? accessor) (values (prints obj "?." (m-compile accessor)) 'exist-accessor))
    ;; ((list* (type atom obj) '-> accessor) (values (prints obj ".prototype." (m-compile accessor)) 'prototype))
    ;; ((list* (type keyword key) rest) (values (m-obj lst) 'obj-literal))
    ((list* sentences) (values (m-sentences sentences) 'sentences))
    ))