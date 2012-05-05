
(defsystem maca
    :description "Yet another Javascript compiler : Macascript"
    :version "0.2"
    :author "Masataro Asai <guicho2.71828@gmail.com>"
    :licence "Public Domain"
	:depends-on (:cl-match :alexandria :anaphora :cl-ppcre)
	:perform (compile-op :after (op c)
						 ;;(maca:recompile)
						 ;;(setf maca:*recompile-compiler* t)
						 )
	:components ((:file "core")
				 (:file "utilities")
				 (:file "fundamentals" :depends-on ("core" "utilities"))
				 (:file "functions" :depends-on ("core" "utilities"))
				 (:file "iterations" :depends-on ("core" "utilities"))
				 (:file "conditions" :depends-on ("core" "utilities"))
				 (:file "conditions2" :depends-on ("core" "utilities"))
				 (:file "ops" :depends-on ("core" "utilities"))
				 (:file "literals" :depends-on ("core" "utilities"))
				 (:file "continuation" :depends-on ("core" "utilities"))
				 (:file "delayforce" :depends-on ("core" "utilities"))
				 (:file "class" :depends-on ("core" "utilities"))
				 (:file "misc" :depends-on ("core" "utilities"))))


;; (recompile)
;; (setf *recompile-compiler* t)