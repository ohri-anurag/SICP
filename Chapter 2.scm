(load "Chapter 1.scm")

; === 2.1 ===
(define (make-rat n d)
  (let ((n2 (abs n))
        (d2 (abs d)))
    (let ((g (gcd n2 d2)))
      (if (or (and (< n 0) (< d 0)) (and (> n 0) (> d 0)))
        (cons (/ n2 g) (/ d2 g))
        (cons (* -1 (/ n2 g)) (/ d2 g))))))

; === 2.2 ===
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
	      (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

; === 2.3 ===
;(define make-rectangle cons)
;(define width car)
;(define height cdr)

(define make-rectangle cons)
(define (width rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))
(define (height rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))

(define (area rectangle)
  (* (width rectangle) (height rectangle)))

(define (parameter rectangle)
  (* 2 (+ (width rectangle) (height rectangle))))
