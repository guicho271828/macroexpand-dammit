
(in-package :macroexpand-dammit-test)
(in-suite :macroexpand-dammit-test)

#-allegro
(declaim (type integer *declared-integer*))
#+allegro
(eval-when (:load-toplevel :execute)
  (proclaim '(type integer *declared-integer*)))

(defvar *declared-integer* 0)
(defvar *compile-time-counter* 0)

(defmacro count-up ()
  (incf *compile-time-counter*)
  `(progn))


(test issue1-control
  (let* ((expanded
          (macroexpand-dammit
           '(symbol-macrolet ((a (error "the binding of symbol a is not shadowed!")))
             (let* ((b (progn (count-up) (+ a 2))))
               (print b)))))
         (compiled-fn (compile nil `(lambda () ,expanded))))
    (signals error
      (funcall compiled-fn))))

(test issue1-again
  (let ((*compile-time-counter* 0))
    (let* ((expanded
            (macroexpand-dammit '(let ((a (progn (count-up) 1))
                                       (b (progn (count-up) 2)))
                                  (+ a b))))
           (compiled-fn (compile nil `(lambda () ,expanded))))
      (is (= 3 (funcall compiled-fn)))
      (is (= 2 *compile-time-counter*))))

  ;; symbol-macrolet shadowed by let
  (is (=
       (symbol-macrolet ((a 100))
         (let* ((a 1) (b (+ a 2)))
           (+ a b)))
       (eval
        (macroexpand-dammit
         '(symbol-macrolet ((a 100))
           (let* ((a 1)
                  (b (+ a 2)))
             (+ a b)))))))

  ;; let shadowed by symbol-macrolet
  (is (=
       (let ((a 1))
         (symbol-macrolet ((a 100))
           (let ((b (+ a 2)))
             (+ a b))))
       (eval
        (macroexpand-dammit
         '(let ((a 1))
           (symbol-macrolet ((a 100))
             (let ((b (+ a 2)))
               (+ a b))))))))

  (let ((*compile-time-counter* 0))
    (let* ((expanded
            (macroexpand-dammit
             '(symbol-macrolet ((a (error "the binding of symbol a is not shadowed!")))
               (let* ((a (progn (count-up) 1))
                      (b (progn (count-up) (+ a 2))))
                 (+ a b)))))
           (compiled-fn (compile nil `(lambda () ,expanded))))
      (is (= 4 (funcall compiled-fn)))
      (is (= 2 *compile-time-counter*)))))

(test issue1-type-declaration
  ;; Ensure that in the absence of a symbol-macro binding, no dummy binding is
  ;; imposed that would conflict with the type declaration for a global
  ;; variable.
  (is (=
       5
       (eval
        (macroexpand-dammit
         '(symbol-macrolet ((not-a-special-variable 1))
           (let ((*declared-integer* 2)
                 (not-a-special-variable 3))
             (+ *declared-integer* not-a-special-variable)))))))
  
  ;; In ANSI CL it is an error to establish a symbol-macro binding for an
  ;; existing global variable name, so no let binding for such a variable can
  ;; ever shadow a symbol-macro anyway.
  (signals error
    (macroexpand-dammit
     '(symbol-macrolet ((*declared-integer* 'foo))))))
