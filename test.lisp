


(defpackage macroexpand-dammit-test
  (:use :cl :macroexpand-dammit :fiveam))

(in-package :macroexpand-dammit-test)

(def-suite macroexpand-dammit)
(in-suite macroexpand-dammit)
(test nested
  "testing highly nested macrolets"
  (macrolet ((a () :b))

       (macrolet ((c (&body body &environment env)
		    (let ((expansion (macroexpand-dammit body env)))
		      (is (equalp expansion
				  '((PRINT :b) (PRINT :b))))
		      `(progn ,@expansion))))

	 (macrolet ((d () `(a)))
	   (c (print (d))
	      (print (d)))))))

(test nested2
  "testing highly nested macrolets 2nd. error case in 20100701"
  (macrolet ((a () :b))

       (macrolet ((c (&body body &environment env)
		    (let ((expansion (macroexpand-dammit body env)))
		      (is (equalp expansion
				  '((PRINT :b) (PRINT :b))))
		      `(progn ,@expansion))))

	 (macrolet ((d () `(a)))
	   (c 
	    (macrolet ((e () `(a)))
	      (print (d))
	      (print (d))))))))

