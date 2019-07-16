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

; === 2.4 ===
;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

; === 2.5 ===
;(define (cons a b)
;  (* (fast-exp 2 a) (fast-exp 3 b)))

;(define (car z)
;  (if (even? z)
;      (+ 1 (car (/ z 2)))
;      0))
;(define (cdr z)
;  (if (= (remainder z 3) 0)
;      (+ 1 (cdr (/ z 3)))
;      0))

; === 2.6 ===
; (define zero (lambda (f) (lambda (x) x)))

; (define (add-1 n)
;   (lambda (f) (lambda (x) (f ((n f) x)))))

; (define one (lambda (f) (lambda (x) (f x))))

; (define two (lambda (f) (lambda (x) (f (f x)))))

; (define (+ m n)
;   (lambda (f)
;     (lambda (x)
;       ((m f) ((n f) x)))))

; (define three (+ one two))

; (define (test x)
;   (display x)
;   x)

; === 2.7 ===
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

; (define (mul-interval x y)
;   (let ((p1 (* (lower-bound x) (lower-bound y)))
; 	(p2 (* (lower-bound x) (upper-bound y)))
; 	(p3 (* (upper-bound x) (lower-bound y)))
; 	(p4 (* (upper-bound x) (upper-bound y))))
;     (make-interval (min p1 p2 p3 p4)
; 		   (max p1 p2 p3 p4))))

(define make-interval cons)
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

; === 2.8 ===
(define (sub-interval x y)
  (add-interval (make-interval (upper-bound x) (- (lower-bound y)))
		(make-interval (lower-bound x) (- (upper-bound y)))))

; === 2.9 ===
; in case of addition
; width of first interval = w1
; width of second interval = w2
; width of sum interval = [(u1+u2) - (l1+l2)] / 2
; = [ (u1-l1) + (u2-l2)] / 2
; = (u1-l1)/2 + (u2-l2)/2
; = w1 + w2
; In case of multiplication, however, this is not the case
; Consider [0 10] * [0 2] = [0 20]
; w1 = 5, w2 = 1. However, width of product is 10.

; === 2.10 ===
(define (div-interval x y)
  (mul-interval x
    (let ((l (lower-bound y))
          (u (upper-bound y)))
      (if (or (= l 0) (= u 0) (and (< l 0) (> u 0)))
        (error "Divisor interval contains zero!!")
        (make-interval (/ 1.0 u) (/ 1.0 l))))))

; === 2.11 ===
(define (mul-interval x y)
  (let ((u1 (upper-bound x))
        (l1 (lower-bound x))
        (u2 (upper-bound y))
        (l2 (lower-bound y)))
    (cond 
           ; One negative
           ((and (> u1 0) (< l1 0) (< u2 0) (< l2 0)) (make-interval (* l1 l2) (* u1 l2)))
           ((and (< u1 0) (< l1 0) (> u2 0) (< l2 0)) (make-interval (* l1 l2) (* l1 u2)))
           ; One positive
           ((and (> u1 0) (< l1 0) (> u2 0) (> l2 0)) (make-interval (* u1 u2) (* l1 u2)))
           ((and (> u1 0) (> l1 0) (> u2 0) (< l2 0)) (make-interval (* u1 u2) (* u1 l2)))
           ; 2 +, 2 -
           ((and (> u1 0) (> l1 0) (< u2 0) (< l2 0)) (make-interval (* l1 u2) (* u1 l2)))
           ((and (< u1 0) (< l1 0) (> u2 0) (> l2 0)) (make-interval (* l2 u1) (* u2 l1)))
           ; SPECIAL
           ((and (> u1 0) (< l1 0) (> u2 0) (< l2 0)) (make-interval (max (* u1 u2) (* l1 l2)) (min (* u1 l2) (* u2 l1))))
           ; All Positive
           ((and (> u1 0) (> l1 0) (> u2 0) (> l2 0)) (make-interval (* u1 u2) (* l1 l2)))
           ; All Negative
           ((and (< u1 0) (< l1 0) (< u2 0) (< l2 0)) (make-interval (* l1 l2) (* u1 u2))))))

; === 2.12 ===
(define (make-center-percent c p)
  (let ((w (/ (* p c) 100.0)))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent x)
  (* 100.0 (/ (width x) (center x))))

; === 2.13 ===
; t1 = w1/c1
; t2 = w2/c2
; For positive intervals, we have
; [c1-w1, c1+w1] * [c2-w2, c2+w2]
; = [(c1-w1)*(c2-w2), (c1+w1)*(c2+w2)]
; = [c1c2 - c1w2 - c2w1 + w1w2, c1c2 + c1w2 + c2w1 + w1w2]
; Since, w1, w2 are both small, w1w2 ~ 0
; = [c1c2 - (c1w2+c2w1), c1c2 + (c1w2+c2w1)]
; Tolerance = (c1w2 + c2w1)/c1c2 = c1w2/c1c2 + c2w1/c1c2 = w2/c2 + w1/c1 = t1 + t2

; === 2.14 ===
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; (par1 (make-center-percent 6 0.1) (make-center-percent 5 0.1)) ; (2.719101807283626 . 2.7354654654654653)
; (par2 (make-center-percent 6 0.1) (make-center-percent 5 0.1)) ; (2.7245454545454546 . 2.73)

; (div-interval (make-center-percent 100 1) (make-center-percent 100 1)) ; (.9801980198019802 . 1.0202020202020203)

; (define r1 (make-center-percent 2 1))
; (par1 r1 r1) = (1.0304040404040404 . .9703960396039604)
; (par2 r1 r1) = (1.01 . .99)

; === 2.15 ===
; Eva Lu Ator is right. When uncertain variables are repeated in a program, the program might end up using different values within
; the same interval for the same calculation.

; In case of r1*r2/(r1 + r2)
; to get the upper bound of total resistance, we need to maximise numerator and minimize denomerator.
; To maximise numerator, max value of r1, r2 is chosen.
; To minimise denomerator, min value of r1, r2 is chosen.
; Here we see that an inconsistency has arisen, which causes looser bounds.

; === 2.16 ===
; We have already seen in 2.15 why we get different answers in computing the same algebraic expression in
; two different ways. 
; It seems that it would be quite difficult to create such a library that implements correct interval algebra, since we would need
; to relate two uses of the same interval somehow. We might have to create an interpreter that understands operations, and
; relates the different uses of the same interval. Quite difficult indeed.

; === 2.17 ===
(define (last-pair items)
  (let ((tail (cdr items)))
    (if (null? (cdr items))
      items
      (last-pair (cdr items)))))

; === 2.18 ===
(define (reverse items)
  (define (reverse-iter acc items)
    (if (null? items)
      acc
      (reverse-iter (cons (car items) acc) (cdr items))))
  (reverse-iter (list) items))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

; The order of coin-values has no effect on the result. The function cc makes no such assumptions about coin-values,
; and hence the result is independent of the order of coin-values.

; === 2.20 ===
(define (same-parity x . items)
  (define (iter acc items)
    (cond ((null? items) (reverse acc))
          ((= (remainder x 2) (remainder (car items) 2)) (iter (cons (car items) acc) (cdr items)))
          (else (iter acc (cdr items)))))
  (iter (list x) items))