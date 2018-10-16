(define list-in (list 1 2 3 4 5 6 7 8 9))
(define list-out (list 6 7 8 9))

(display "Result is: ")
(display (filter (lambda (x) (> x 5)) list-in))
(newline)

(run-test "Basic unit test(p)" #f
          (lambda () (equal?
                      (filter (lambda (x) (> x 5)) list-in)
                      list-out)))


