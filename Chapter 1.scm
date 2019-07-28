; #lang sicp
; === Helpers ===
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; === 1.1 ===
10                     ; Prints 10
(+ 5 3 4)              ; Prints 12
(- 9 1)                ; Prints 8
(/ 6 2)                ; Prints 3
(+ (* 2 4) (- 4 6))    ; Prints 6
(define a 3)           ; Prints nothing
(define b (+ a 1))     ; Prints nothing
(= a b)                ; Prints #f
(if (and (> b a) (< b (* a b)))
    b
    a)                 ; Prints 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))       ; Prints 16
(+ 2 (if (> b a) b a)) ; Prints 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))            ; Prints 16

; === 1.2 ===
; (/ ( + 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5)))))) (* 3 (- 6 2) (- 2 7)))

; === 1.3 ===
(define (sum-of-larger-squares a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        (else                  (sum-of-squares a b))))
;(sum-of-larger-squares 1 2 3)
;(sum-of-larger-squares 3 2 1)
;(sum-of-larger-squares 2 1 3)

; === 1.4 ===
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; Both the following statements print 6. The absolute value of b is added to a.
;(a-plus-abs-b 5 1)
;(a-plus-abs-b 5 -1)

; === 1.5 ===
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;(test 0 (p))
; In case of an applicative-order interpreter, this creates an infinite loop.
; In case of a normal-order interpreter, however, this will terminate with the result 0.

; === Helpers ===
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; === 1.6 ===
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
; Doesn't work, applicative order forces the recursion to happen every time, creating an infinite loop.
(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x)
                     x)))
; === 1.7 ===
(sqrt 0.0001) ; Should produce 0.01, instead produces 0.0323. Fails since the radicand is smaller than the delta that we're using in good-enough?
(sqrt 1234567890098765432112345678900987654321) ; Produces 3.513641828785008e+019, loses out on the Least significant bits, since operations have limited precision

(define (good-enough-new? guess x)
  (< (abs (- guess (improve guess x))) 0.001))
(define (sqrt-iter-newer guess x)
  (if (good-enough-new? guess x)
          guess
          (sqrt-iter-newer (improve guess x)
                     x)))
(define (sqrt-new x)
  (sqrt-iter-newer 1.0 x))
(sqrt-new 0.0001) ; Produces 0.010120218365353947, an improvement over the previous solution
(sqrt-new 1234567890098765432112345678900987654321) ; Produces 3.513641828785008e+019, no change from the previous one

; === 1.8 ===
(define (cube-root x)
  (cube-root-iter 1.0 x))
(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x)
                      x)))
(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cube x) (* x x x))
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(cube-root 8)
(cube-root 729)

; === 1.9 ===
; The first definition is a recursive process. The second one is iterative though, the interpreter needs to keep track of just a and b.
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

; === 1.10 ===
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ; Produces 1024
(A 2 4)  ; Produces 65536
(A 3 3)  ; Produces 65536

(define (f n) (A 0 n))   ; 2*n
(define (g n) (A 1 n))   ; 2^n
(define (h n) (A 2 n))   ; 2^(h (- n 1))
(define (k n) (* 5 n n)) ; 5n^2

; === 1.11 ===
; Recursive process
(define (recursive-f n)
  (if (< n 3)
      n
      (+ (recursive-f (- n 1)) (* 2 (recursive-f (- n 2))) (* 3 (recursive-f (- n 3))))))
; Iterative process
(define (iterative-f n)
  (define (helper times a b c)
    (cond ((= times 0) c)
          ((= times 1) b)
          ((= times 2) a)
          (else (helper (- times 1) (+ a (* 2 b) (* 3 c)) a b))))
  (helper n 2 1 0))

; === 1.12 ===
(define (pascal row col)
  (if (or (= col 1) (= row col))
      1
      (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))

; === 1.13 ===
; Here Phi(n) = (Golden Ratio)^n
;      phi(n) = (1 - Golden Ratio)^n
; To Prove : Fib(n) = (Phi(n) - phi(n))/sqrt(5)
; For n = 1
; Fib(1) = 1
; (Phi(1) - phi(1))/sqrt(5) = ( 1 + sqrt(5) - 1 + sqrt(5) ) / (2 * sqrt(5) = (2 * sqrt(5)) / (2 * sqrt(5)) = 1
;
;
; For k, assume, Fib(k) = (Phi(k) - phi(k)) / sqrt(5)
; For k+1, RHS
; (Phi(k+1) - phi(k+1))/sqrt(5) = (Phi(k).Phi(1) - phi(k).phi(1))/sqrt(5)
;                               = (Phi(k).(1 - phi(1)) - phi(k).(1 - Phi(1)))/sqrt(5)
;                               = [ Phi(k) - Phi(k).phi(1) - phi(k) + phi(k).Phi(1) ] / sqrt(5)
;                               = [ (Phi(k) - phi(k)) - phi(1).Phi(1).{Phi(k-1) - phi(k-1)} ] / sqrt(5)
;                               = [ (sqrt(5)*Fib(k) - phi(1).Phi(1).sqrt(5).Fib(k-1) ] / sqrt(5)
;                               = Fib(k) - (1 + sqrt(5)).(1 - sqrt(5)).Fib(k-1)/4
;                               = Fib(k) - (1-5).Fib(k-1)/4
;                               = Fib(k) + 4.Fib(k-1)/4
;                               = Fib(k) + Fib(k-1)
;                               = Fib(k+1)
; Hence Proved
; Since phi(1) = (1 - sqrt(5))/2 ~ -0.6
; phi(1) < 1
; phi(n) << 1
; Which means that the Fib(n) is indeed the closest integer to Phi(n)

; === 1.14 ===
; count-change 11
; cc 11 5
; cc 11 4 + cc -39 5
; cc 11 3 + cc -14 4
; cc 11 2 + cc 1 3
; cc 11 1 + cc 6 2 + cc 1 2 + cc -9 3
; cc 11 0 + cc 10 1 + cc 6 1 + cc 1 2 + cc 1 1 + cc -4 2
; cc 10 0 + cc 9 1 + cc 6 0 + cc 5 1 + cc 1 1 + cc -4 2 + cc 1 0 + cc 0 1
; cc 9 0 + cc 8 1 + cc 5 0 + cc 4 1 and so on

; The order of growth seems to be O(a*n)

; === 1.15 ===
; sine 12.15
; sine 4.05  - call 1
; sine 1.016 - call 2
; sine 0.33  - call 3
; sine 0.11  - call 4
; sine 0.033 - call 5

; It seems that the order of growth in both space and time is log3 a

; === 1.16 ===
(define (fast-exp b n)
  (define (helper a b n)
    (cond ((= n 0) a)
          ((even? n) (helper a (square b) (/ n 2)))
          (else (helper (* a b) b (- n 1)))))
  (helper 1 b n))

; === 1.17 ===
; === Helpers ===
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

; === 1.18 ===
(define (fast-mult-iter a b)
  (define (helper temp sum times)
    (cond ((= times 0) temp)
          ((even? times) (helper temp (double sum) (halve times)))
          (else (helper (+ temp sum) sum (- times 1)))))
  (helper 0 a b))

; === 1.19 ===
; Tpq ==> a' <- bq + aq + ap, b' <- bp + aq
; Tpq ==> a'' <- b'q + a'q + a'p, b'' <- b'p + a'q
;     ==> a'' <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, b'' <- (bp + aq)p + (bq + aq + ap)q
;     ==> a'' <- bpq + aqq + bqq + aqq + apq + bpq + apq + app, b'' <- bpp + apq + bqq + aqq + apq
;     ==> a'' <- app + 2aqq + 2apq + 2bpq + bqq, b'' <- 2apq + aqq + bpp + bqq
;     ==> a'' <- b(2pq + qq) + a(2pq + qq) + a(pp + qq), b'' <- b(pp + qq) + a(2pq + qq)
; Hence, p' = pp + qq, q' = 2pq + qq
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; === 1.20 ===
; Normal Order Evaluation
;   (gcd 206 40)
;   Since 40 != 0,
; = (gcd 40 (remainder 206 40))
;   Since (remainder 206 40) = 6 != 0, <<==>> 1 calculation total
; = (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;   Since (remainder 40 (remainder 206 40)) = 4 != 0, <<==>> 3 calculations total
; = (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;   Since (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) = 2 != 0 <<==>> 7 calculations total
; = (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;   Since (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) = 0 - 14 calculations total
; = (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) <<==>> 18 calculations total

; Applicative order evalutation
; (gcd 206 40)
; Since 40 != 0
; (gcd 40 6)    -- 1 calculation
; Since 6 != 0
; (gcd 6 4)     -- 2 calculations
; Since 4 != 0
; (gcd 4 2)     -- 3 calculations
; Since 2 != 0
; (gcd 2 0)     -- 4 calculations
; 2

; === Helper Functions ===
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
; === 1.21 ===
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; === 1.22 ===
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (cpu-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (cpu-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes begin end)
  (cond ((> begin end)
         (newline)
         (display "Search Finished"))
        ((timed-prime-test begin)
         (search-for-primes (+ begin 1) end))
        (else (search-for-primes (+ begin 1) end))))

;(search-for-primes 1000 1019)
;(newline)
;(search-for-primes 10000 10037)
;(newline)
;(search-for-primes 100000 100043)
;(newline)
;(search-for-primes 1000000 1000037)

; As of 2019, computers have become too fast to appreciate the time 
; required to test the primality of such small numbers. 
; To get meaningful results, we should perform the test with numbers 
; greater by, say, a factor 1e8. 
;(search-for-primes 100000000000 100000000057)       ; 1e11, 100000000003, 100000000019, 100000000057
;(newline)
;(search-for-primes 1000000000000 1000000000063)     ; 1e12, 1000000000039, 1000000000061, 1000000000063
;(newline)
;(search-for-primes 10000000000000 10000000000099)   ; 1e13, 10000000000037, 10000000000051, 10000000000099
;(newline)
;(search-for-primes 100000000000000 100000000000097) ; 1e14, 100000000000031, 100000000000067, 100000000000097

; Average time for 1e11 = (/ (+ 15599 46800 15600) 3) = 25999.67 ~ 26000
; Average time for 1e12 = (/ (+ 62400 62400 62400) 3) = 62400        ~ 2.2  x 1e11
; Average time for 1e13 = (/ (+ 202800 202801 218400) 3) = 208000.33 ~ 8    x 1e11
;                                                                    ~ 3.33 x 1e12
; Average time for 1e14 = (/ (+ 655201 624001 624001) 3) = 634401    ~ 24   x 1e11
;                                                                    ~ 10   x 1e12
;                                                                    ~ 3    x 1e13

; We can clearly see that as the data size increases, the factor gets closer and closer to 10 and sqrt(10) ~ 3.3

; === 1.23 ===
(define (next n)
  (if (= n 2) 3 (+ n 2)))
(define (smallest-divisor-new n)
  (find-divisor-new n 2))
(define (find-divisor-new n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-new n (next test-divisor)))))
(define (prime-new? n)
  (= n (smallest-divisor-new n)))

(define (start-prime-test-new n start-time)
  (if (prime-new? n)
      (report-prime (- (cpu-time) start-time))))
(define (timed-prime-test-new n)
  (newline)
  (display n)
  (start-prime-test-new n (cpu-time)))

; (timed-prime-test-new 100000000003)
; (timed-prime-test-new 100000000019)
; (timed-prime-test-new 100000000057)
; (newline)
; (timed-prime-test-new 1000000000039)
; (timed-prime-test-new 1000000000061)
; (timed-prime-test-new 1000000000063)
; (newline)
; (timed-prime-test-new 10000000000037)
; (timed-prime-test-new 10000000000051)
; (timed-prime-test-new 10000000000099)
; (newline)
; (timed-prime-test-new 100000000000031)
; (timed-prime-test-new 100000000000067)
; (timed-prime-test-new 100000000000097)
; New running Times
;100000000003 *** 15600
;100000000019 *** 0
;100000000057 *** 15600

;1000000000039 *** 46800
;1000000000061 *** 31200
;1000000000063 *** 31201

;10000000000037 *** 124800
;10000000000051 *** 124800
;10000000000099 *** 109200

;100000000000031 *** 405601
;100000000000067 *** 374401
;100000000000097 *** 390000

; Once again, it is clearly evident that the cpu-times have decreased by a factor 2, which is what we expected.

; === 1.24 ===

; === Helper Methods ===
;(#%require (lib "27.ss" "srfi"))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (start-prime-test-fast n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (cpu-time) start-time))))
(define (timed-prime-test-fast n)
  (newline)
  (display n)
  (start-prime-test-fast n (cpu-time)))

(newline)
(display "Fast Prime Test")
(timed-prime-test-fast 100000000003)
(timed-prime-test-fast 100000000019)
(timed-prime-test-fast 100000000057)
(newline)
(timed-prime-test-fast 1000000000039)
(timed-prime-test-fast 1000000000061)
(timed-prime-test-fast 1000000000063)
(newline)
(timed-prime-test-fast 10000000000037)
(timed-prime-test-fast 10000000000051)
(timed-prime-test-fast 10000000000099)
(newline)
(timed-prime-test-fast 100000000000031)
(timed-prime-test-fast 100000000000067)
(timed-prime-test-fast 100000000000097)

; Computers have become too fast, all above tests return 0

; === 1.25 ===
; (define (expmod base exp m)
;   (remainder (fast-exp base exp) m))

; This procedure will be slower than the one we defined, since we only multiply numbers under n.
; This procedure has no limit on the exponentiation part, so the numbers can grow to huge digits, which will slow down the multiplication.

; === 1.26 ===
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (* (expmod base (/ exp 2) m)
;                       (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

; This procedure calls expmod twice whenever the exponent is even.
; For instance, take exp=2^n. The previous definition of expmod would take log(2^n) ~ n steps.
; This definition however forms a Geometric progression as follows
; expmod b 2^n m     -- 1
; expmod b 2^(n-1) m  *  expmod b 2^(n-1) m   -- 2
; expmod b 2^(n-2) m  *  expmod b 2^(n-2) m  *  expmod b 2^(n-2) m  *  expmod b 2^(n-2) m   -- 4

; This recursion stops when exp becomes 1, after n steps
; Total calls become = 1 + 2 + 4 + 2^n
;                    = 1.(2^(n+1) - 1)/(2-1)
;                    = 2^(n+1) - 1
; Which is the same (*2) as the input (2^n). Hence this is a O(n) process.

; === 1.27 ===
(newline)
(newline)
(display "Carmichael Test")
(define (carmichael? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test m)
    (cond ((= m 0) #t)
          ((try-it m) (test (- m 1)))
          (else #f)))
  (test (- n 1)))
(newline)
(carmichael? 560)  ; This one should fail
(carmichael? 561)
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)

; === 1.28 ===
(define (expmod-signal base exp m)
  (define (non-trivial-root t)
    (if (and (> t 1) (< t (- m 1)) (= (remainder (square t) m) 1))
        0
        (remainder (square t) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (non-trivial-root (expmod-signal base (/ exp 2) m)))
        (else
         (remainder (* base (expmod-signal base (- exp 1) m))
                    m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-signal a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-new? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime-new? n (- times 1)))
        (else #f)))
(define (print string)
  (newline)
  (display string)
  (newline))
(print "Miller Rabin")
(print "Carmichael numbers")
(fast-prime-new? 561  5)
(fast-prime-new? 1105 5)
(fast-prime-new? 1729 5)
(fast-prime-new? 2465 5)
(fast-prime-new? 2821 5)
(fast-prime-new? 6601 5)
(print "Prime Numbers")
(fast-prime-new? 100000000003 5)
(fast-prime-new? 100000000019 5)
(fast-prime-new? 100000000057 5)
(newline)
(fast-prime-new? 1000000000039 5)
(fast-prime-new? 1000000000061 5)
(fast-prime-new? 1000000000063 5)
(newline)
(fast-prime-new? 10000000000037 5)
(fast-prime-new? 10000000000051 5)
(fast-prime-new? 10000000000099 5)
(newline)
(fast-prime-new? 100000000000031 5)
(fast-prime-new? 100000000000067 5)
(fast-prime-new? 100000000000097 5)

; === 1.29 ===
; === Helper ===
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (simpson-constant k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (simpson-term k)
    (* (simpson-constant k) (f (+ a (* k h)))))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

; Either this rule is too accurate or the scheme implementation is rounding off, I get 0.25 exactly.

; === 1.30 ===
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; === 1.31 (A) ===
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (factorial n)
  (product identity 1 inc n))
(define (pi n)
  (define (term k)
    (if (even? k)
        (/ (+ k 2) (+ k 3))
        (/ (+ k 3) (+ k 2))))
  (* 4.0 (product-iter term 0 inc n)))
; === 1.31 (B) ===
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; === 1.32 (A) ===
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))
(define (product-acc term a next b)
  (accumulate * 1 term a next b))

; === 1.32 (B) ===
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; === 1.33 ===
(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter? (term a)) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))
; === 1.33 (A) ===
(define (sum-prime a b)
  (filtered-accumulate prime? + 0 identity a inc b))
(define (sum-relatively-prime n)
  (define (relatively-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate relatively-prime? + 0 identity 1 inc (- n 1)))

(define (f g)
  (g 2))

; (display (f f)) - This evaluates to (2 2), which fails, since 2 is not a function.

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f a b))
          (else
           (error "Values are not of opposite sign" a b)))))

; === 1.35 ===
; x -> 1 + 1/x
; x^2 -> x + 1
; x^2 - x - 1 = 0
; Here, a = 1, b = -1, c = -1
; x = (-b +- sqrt(b^2 - 4ac))/2a
; x = (1 +- sqrt(1 + 4))/2
; x = (1 + sqrt(5))/2 or x = (1 - sqrt(5))/2
; x = Phi or x = phi

(define tolerance 0.00001)
; === 1.36 ===
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1) - Returns 1.6180327868852458

; x^x = 1000
; x * log x = log 1000
; x = log 1000 / log x
; f(x) = log 1000 / log x

; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0) - Without damping takes 34 steps, produces 4.555532270803653
; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0) - With damping 9 steps, produces 4.555537551999825

; === 1.37 (A) ===
(define (cont-frac n d k)
  (define (iter t result)
    (if (= t 0)
      result
      (iter (- t 1) (/ (n k) (+ (d k) result)))))
  (iter k 0.0))

;  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) - Produces correct value upto 4 decimal places in 11 turns
; === 1.37 (B) ===
(define (cont-frac-rec n d k)
  (define (recur i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))
; (cont-frac-rec (lambda (i) 1.0) (lambda (i) 1.0) 11)

; === 1.38 ===
(define (euler k)
  (+ 2 (cont-frac
    (lambda (i) 1)
    (lambda (i)
      (if (= (remainder i 3) 2)
        (- i (quotient i 3))
        1))
    k)))

; === 1.39 ===
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
      x
      (* -1 (square x))))
  (define (d i) (- (* 2 i) 1))
  (cont-frac n d k))

; === 1.40 ===
; === Helpers ===
(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; === 1.41 ===
(define (double g)
  (lambda (x)
    (g (g x))))

(define (inc x) (+ 1 x))

; (((double (double double)) inc) 5) - Returns 21
; ((double (double double)) inc)
; ((double double) ((double double) inc))
; ((double double) (double (double inc)))
; (double (double (double (double inc)))) - inc called 16 times

; === 1.42 ===
(define (compose f g)
  (lambda (x)
    (f (g x))))

; ((compose square inc) 6)

; === 1.43 ===
(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

; === 1.44 ===
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

; (fixed-point-of-transform (lambda (y) (/ 256 (fast-exp y 7))) (repeated average-damp 3) 1.0)
; === 1.45 ===
(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (fast-exp y (- n 1)))) (repeated average-damp (/ (log n) (log 2))) 1.0))

; === 1.46 ===
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

(define (sqrt-final x)
  ((iterative-improve
    (lambda (guess)
      (good-enough? guess x))
    (lambda (guess)
      (improve guess x)))
   1.0))

(define (fixed-point-final f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve
    (lambda (guess)
      (close-enough? guess (f guess)))
    f)
   first-guess))