

(defsystem macascript
    :description "Yet another Javascript compiler : Macascript"
    :version "0.2"
    :author "Masataro Asai <guicho2.71828@gmail.com>"
    :licence "Public Domain"
	:depends-on (:cl-match :alexandria :anaphora)
	:components ((:file "maca-core")
				 (:file "fundamentals")
				 (:file "functions")
				 (:file "iterations")
				 (:file "conditions")
				 (:file "ops")
				 (:file "literals")
				 (:file "misc"))
	:serial t)


;; (recompile)
;; (setf *recompile-compiler* t)