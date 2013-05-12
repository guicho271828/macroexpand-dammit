

(in-package :cl-user)
(defpackage macroexpand-dammit-test-asd
  (:use :cl :asdf))
(in-package :macroexpand-dammit-test-asd)

(defsystem macroexpand-dammit-test
  :depends-on (:macroexpand-dammit :fiveam)
  :components
  ((:file :test)))
