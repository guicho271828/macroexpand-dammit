
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


(defmacro expansion-is-correct (&body body)
  (format *error-output* "~&expanding ~a" body)
  `(= (progn ,@body)
      (eval
       (macroexpand-dammit
        '(progn ,@body)))))

(test issue1-again
  (let ((*compile-time-counter* 0))
    (let* ((expanded
            (macroexpand-dammit '(let ((a (progn (count-up) 1))
                                       (b (progn (count-up) 2)))
                                  (+ a b))))
           (compiled-fn (compile nil `(lambda () ,expanded))))
      (is (= 3 (funcall compiled-fn)))
      (is (= 2 *compile-time-counter*))))

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

(test symbol-macrolet-shadowed-by-let
  (is (expansion-is-correct 
        (symbol-macrolet ((a 100))
          (let* ((a 1) (b (+ a 2)))
            (+ a b)))))

  (is (expansion-is-correct 
        (symbol-macrolet ((a 100))
          (let* ((b (+ a 2)))
            (+ a b)))))

  (is (expansion-is-correct 
        (symbol-macrolet ((a 100))
          (let ((b (+ a 2)))
            (+ a b)))))

  ;; let shadowed by symbol-macrolet
  (is (expansion-is-correct 
        (let ((a 1))
          (symbol-macrolet ((a 100))
            (let ((b (+ a 2)))
              (+ a b))))))

  (is (expansion-is-correct 
        (let ((a 1))
          (let ((b (+ a 2)))
            (symbol-macrolet ((a 100))
              (+ a b))))))

  (is (expansion-is-correct 
        (let ((a 1))
          (symbol-macrolet ((a 100))
            (let ((b (+ a 2)))
              (+ a b))))))

  (is (expansion-is-correct 
        (symbol-macrolet ((a 100))
          (let* ((a 1)
                 (b (+ a 2)))
            (+ a b)))))

  (is (expansion-is-correct 
        (let* ((a 1)
               (b (+ a 2)))
          (symbol-macrolet ((a 100))
            (+ a b))))))
