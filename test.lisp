
(defpackage macroexpand-dammit-test
  (:use :cl :macroexpand-dammit :fiveam))

(in-package :macroexpand-dammit-test)

(def-suite :macroexpand-dammit-test)
(in-suite :macroexpand-dammit-test)
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
    
    (macrolet
	((c (&body body &environment env)
	   (let ((expansion (macroexpand-dammit body env))) ; <-+
	     (is (equalp expansion			    ;   |
			 '((progn (PRINT :b) (PRINT :b))))) ;   |
	     `(progn ,@expansion))))			    ;   |
      (macrolet ((d () `(a)))	     ;			        |
	(c			     ;			        |
	 (macrolet ((e () `(a)))     ; <------------------------+
	   (print (d))		     ; the older version of macroexpand-dammit
	   (print (e))))))))	     ; stops expanding the form after this point.


;; The test results were
;;  The following check failed: (EQUALP EXPANSION '((PROGN (PRINT :B) (PRINT :B))))
;; '((PROGN (PRINT :B) (PRINT :B)))
;;  evaluated to 
;; ((PROGN (PRINT :B) (PRINT :B)))
;;  which is not 
;; EQUALP
;;  to 
;; ((PROGN (PRINT (D)) (PRINT (A))))

;; The original version of macroexpand-dammit stops expanding the form
;; 
;; (macrolet ((e () `(a)))
;;   (print (d))
;;   (print (e)))
;; 
;; mainly because it uses EVAL in order to expand macrolet forms.
;; 
;; When macroexpand-dammit finds a macrolet form it passes the form to
;; EVAL.  ANSI Common Lisp doesn't have EVAL-IN-LEXENV (which some
;; implementation has) and EVAL doesn't take &environment argument.
;; Obviously EVAL doesn't recognize the lexical environment containing
;; the macro-function of D, therefore D remains without expansion. 

;; Since the macro definition of E exists in the form
;; E is always expanded. However again, the definition of A is ignored
;; and the test fails.
;; 
;; The fixed version expands both D and E.

(run! :macroexpand-dammit-test)


