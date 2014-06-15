

(in-package :cl-user)
(defpackage macroexpand-dammit-test-asd
  (:use :cl :asdf))
(in-package :macroexpand-dammit-test-asd)

(defsystem macroexpand-dammit-test
  :depends-on (:macroexpand-dammit :fiveam)
  :serial t
  :components
  ((:file :test)
   (:file :issue-1-again))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :macroexpand-dammit-test)"))
		    (asdf:clear-system c)))

