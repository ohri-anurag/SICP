(load "Chapter 1.scm")
(load "pgm.scm")

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
        (error #f "Divisor interval contains zero!!")
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

; === 2.21 ===
; (define (square-list items)
;   (if (null? items)
;     ()
;     (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

; === 2.22 ===
; Since cons prepends an item to the rest of the list, the last item to be processed is
; added last, and it becomes the first element.

; In the second case, the traditional list structure is not followed. Instead of each cdr providing the rest of the list,
; we have the cons providing it.

; === 2.23 ===
(define (for-each f items)
  (cond ((not (null? items))
          (f (car items))
          (for-each f (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; === 2.24 ===
(list 1 (list 2 (list 3 4))) ; (1 (2 (3 4)))
;  ----     ----
; |.|.| -> |.|\|
; ----     ----
; |        |
; 1       ----     ----
;        |.|.| -> |.|\|
;        ----     ----
;        |        |
;        2       ----     ----
;               |.|.| -> |.|\|
;               ----     ----
;               |        |
;               3        4

; === 2.25 ===
; ( 1 3 (5 7) 9) - cadaddr
; ((7)) - caar
; (1 (2 (3 (4 (5 (6 7)))))) - cadadadadadadr
;   ((repeated (compose car cdr) 6) (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; === 2.26 ===
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

; === 2.27 ===
(define (deep-reverse items)
  (define (iter acc items)
    (if (null? items)
      acc
      (iter (cons (deep-reverse (car items)) acc) (cdr items))))
  (if (pair? items)
    (iter '() items)
    items))

; === 2.28 ===
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

; (define x (list (list 1 2) (list 3 4)))

; === 2.29 ===
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; === a ===
(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

; === b ===
(define (total-weight mobile)
  (if (pair? mobile)
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))
    mobile))

; === c ===
(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (if (pair? mobile)
    (let ((l (left-branch mobile))
          (r (right-branch mobile)))
      (and (balanced? (branch-structure l))
            (balanced? (branch-structure r))
            (= (torque l) (torque r))))
    #t))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(define d (make-mobile (make-branch 10 a) (make-branch 12 5))) 

; === d ===
; Only need to change the selectors

; === 2.30 ===
; (define (square-tree tree)
;   (cond ((null? tree) ())
;         ((not (pair? tree)) (square tree))
;         (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

; (define (square-tree tree)
;   (map (lambda (sub-tree)
;          (if (pair? sub-tree)
;              (square-tree sub-tree)
;              (square sub-tree)))
;        tree))

; === 2.31 ===
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
; (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; === 2.32 ===
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset)) rest)))))

; This works since for each subset, if we consider one element of s,
; it can either be a member or not be a member.
; We calculate the subsets without taking the first element into account,
; and then add it to each of the calculated subsets. In this way, we have
; both the sets that contain and miss the first element. This works recursively
; to calculate all the elements.

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; === 2.33 ===
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; === 2.34 ===
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; === 2.35 ===
(define (count-leaves t)
  (accumulate + 0 
    (cond ((null? x) (list 0))
          ((not (pair? t)) (list 1))
          (else (map count-leaves t)))))
; (define (count-leaves x)
;   (cond ((null? x) 0)
;         ((not (pair? x)) 1)
;         (else (+ (count-leaves (car x))
;                  (count-leaves (cdr x))))))

; === 2.36 ===
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) - (22 26 30)

; === 2.37 ===
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

; === 2.38 ===
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

; (fold-right / 1 (list 1 2 3)) = 3/2
; (fold-left / 1 (list 1 2 3)) = 1/6
; (fold-right list () (list 1 2 3)) = (1 (2 (3 ())))
; (fold-left list () (list 1 2 3)) = (((() 1) 2) 3)

; op must be commutative, ie (op a b) = (op b a)
; and associative, ie (op (op a b) c) = (op a (op b c))

; === 2.39 ===
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


; === 2.40 ===
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum? (unique-pairs n))))

; === 2.41 ===
(define (ordered-triples n s)
  (filter (lambda (triple) (= (accumulate + 0 triple) s))
    (flatmap
      (lambda (i)
        (map (lambda (p) (cons i p)) (unique-pairs (- i 1))))
      (enumerate-interval 1 n))))

; === 2.42 ===
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-boards)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-boards '())
(define (safe? col positions)
  (define (safe-pair? a b)
    (not
      (or 
        (= (car a) (car b))
        (= 
          (abs (- (car a) (car b)))
          (abs (- (cdr a) (cdr b)))))))
  (define (safe-helper? pos positions)
    (if (null? positions)
      #t
      (and
        (safe-pair? pos (car positions))
        (safe-helper? pos (cdr positions)))))
  (safe-helper? (car positions) (cdr positions)))
(define (adjoin-position row col positions)
  (cons (cons row col) positions))

; === 2.43 ===
; Since the queen-cols function is being called for every possible row, this means that for 1 column
; there will be 8 queen-cols calls. And this happen for 8 columns recursively. That means 8^8 calls.
; I think that the running time would be 8^8T.

; === 2.44 ===
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter ( - n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; === 2.53 ===
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

; === 2.54 ===
(define (equal? a b)
  (if (and (pair? a) (pair? b))
    (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
    (eq? a b)))

; === 2.55 ===
; ''abracadabra
; = '(quote abracadabra)
; which scheme interprets as a list with first element quote and second element abracadabra, both of them symbols.
; So, car returns the first element, ie quote.