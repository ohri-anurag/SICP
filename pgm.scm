(import (rnrs)
        (rnrs io ports))

(define (read-binary-pgm-file file)
  (let ((port (open-file-input-port file)))
    (if (binary-pgm? port)
      (parse-parameters port)
      "Invalid input file.")))
  
(define (binary-pgm? binary-port)
  (let ((bv (get-bytevector-n binary-port 3))) ; 3 characters including newline
    (and
      (= (bytevector-u8-ref bv 0) 80)       ; For P
      (= (bytevector-u8-ref bv 1) 53))))    ; For 5

(define (read-line binary-port)
  (if (= (get-u8 binary-port) 10)
    #t
    (read-line binary-port)))

(define (whitespace? x)
  (or (= x 9) (= x 10) (= x 13) (= x 32)))

(define (read-ascii-integer port)
  (define (helper acc)
    (let ((b (get-u8 port)))
      ; (display b)
      ; (newline)
      (if (whitespace? b)
        acc
        (helper (+ (* acc 10) (- b 48))))))
  (helper 0))


(define (parse-parameters port)
  (read-line port)
  (let ((maxval (read-ascii-integer port))
        (h (read-ascii-integer port))
        (w (read-ascii-integer port)))
    (parse-matrix w h maxval port)))

(define (parse-matrix w h maxval port)
  (cons (list w h maxval) (bytevector->u8-list (get-bytevector-n port (* w h)))))

(define (invert pgm)
  (let ((parameters (car pgm))
        (matrix (cdr pgm)))
    (cons parameters
      (map
        (lambda (x) (- (caddr parameters) x))
        matrix))))

(define (write-ascii-number port x)
  (put-bytevector port
    (string->utf8 (number->string x))))

(define (write-parameters port parameters)
  ; (display parameters)
  (write-ascii-number port (car parameters))
  ; (display "momommo")
  (put-u8 port 32)
  (write-ascii-number port (cadr parameters))
  (put-u8 port 10)
  (write-ascii-number port (caddr parameters))
  (put-u8 port 10))
  ; (display 'done))
  ; (put-bytevector port #vu8((car parameters) 32 (cadr parameters) 10 (caddr parameters) 10)))

(define (write-binary-pgm-file file pgm)
  (let ((port (open-file-output-port file)))
    (put-bytevector port #vu8(80 53 10))           ; P5\n
    (put-u8 port 10)
    (write-parameters port (car pgm))
    (put-bytevector port (u8-list->bytevector (cdr pgm)))
    (close-port port)))