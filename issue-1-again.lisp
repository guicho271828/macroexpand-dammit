
(in-package :macroexpand-dammit-test)
(in-suite :macroexpand-dammit-test)



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
       (macroexpand-dammit
        '(let ((a 1))
          (symbol-macrolet ((a 100))
            (let ((b (+ a 2)))
              (+ a b)))))))

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
