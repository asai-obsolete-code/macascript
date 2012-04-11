

(defsystem macascript
    :description "Yet another Javascript compiler : Macascript"
    :version "0.2"
    :author "Masataro Asai <guicho2.71828@gmail.com>"
    :licence "Public Domain"
	:depends-on (:cl-match :alexandria :anaphora)
	:components ((:file "core")
				 (:file "utilities")
				 (:file "fundamentals")
				 (:file "functions")
				 (:file "iterations")
				 (:file "conditions")
				 (:file "conditions2")
				 (:file "ops")
				 (:file "literals")
				 (:file "continuation")
				 (:file "misc"))
	:serial t)


;; (recompile)
;; (setf *recompile-compiler* t)