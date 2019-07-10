(load "Chapter 1.scm")

(define (make-rat n d)
  (let ((n2 (abs n))
        (d2 (abs d)))
    (let ((g (gcd n2 d2)))
      (if (or (and (< n 0) (< d 0)) (and (> n 0) (> d 0)))
        (cons (/ n2 g) (/ d2 g))
        (cons (* -1 (/ n2 g)) (/ d2 g))))))