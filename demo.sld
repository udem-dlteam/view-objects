(define-library (demo)

  (import (..view-objects)
          (gambit))

  (begin

    (define (call-in-fresh-thread thunk)
      (thread-join! (thread-start! (make-thread thunk))))

    (define (demo)

      (define-type point id: ABCDEFG x y)

      (define (get-cont)

        ;; get a first-class continuation inside a thread executing a
        ;; call to map (so the continuation is more predictable)

        (call-in-fresh-thread
         (lambda ()
           (call-with-current-continuation
            (lambda (return)
              (map (lambda (x)
                     (call-with-current-continuation
                      (lambda (cont)
                        (if (= x 6)
                            (return cont)
                            (+ 1000 x)))))
                   '(0 1 2 3 4 5 6 7 8 9)))))))

      (view-objects
       (list (cons "simple: "
                   (list 1 2 (list 3 4) (list 5) 6))
             (cons "cycle: "
                   (vector (circular-list 1 2 3) current-exception-handler))
             (cons "struct: "
                   (vector (make-point 1 2) (make-point 3 4)))
             (cons "cont: "
                   (get-cont)))
       '((detail-level . 1))))

    (demo)))
