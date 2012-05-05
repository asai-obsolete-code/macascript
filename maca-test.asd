

(defsystem maca-test
    :description "the test package of macascript"
    :version "0.2"
    :author "Masataro Asai <guicho2.71828@gmail.com>"
    :licence "Public Domain"
	:depends-on (:cl-match :alexandria :anaphora :cl-ppcre :maca)
	:perform (compile-op :after (op c)
						 ;;(maca:recompile)
						 ;;(setf maca:*recompile-compiler* t)
						 )
	:components ((:file "maca-test")))