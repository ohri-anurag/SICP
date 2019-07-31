(import (rnrs)
        (rnrs io ports))

; This is a very basic PGM parser library.
; It does NOT correctly parse PGM files that have maxval greater than 255.
; Also, COMMENTS are supported just below the magic number line. Any other comments will cause an error.

; Read the PGM from a file
(define (read-binary-pgm-file file)
  (let ((port (open-file-input-port file)))
    (if (binary-pgm? port)
      (parse-parameters port)
      "Invalid input file.")))

; Check if the PGM is a binary PGM file, ie magic number should be P5.
(define (binary-pgm? binary-port)
  (let ((bv (get-bytevector-n binary-port 3))) ; 3 characters including newline
    (and
      (= (bytevector-u8-ref bv 0) 80)       ; For P
      (= (bytevector-u8-ref bv 1) 53))))    ; For 5

; Read a complete line
(define (read-line binary-port)
  (if (newline? (get-u8 binary-port))
    #t
    (read-line binary-port)))

(define (newline? x)
  (or 
    (= x 10)    ; Line Feed, LF
    (= x 13)))  ; Carriage Return, CR

(define (whitespace? x)
  (or 
    (= x 9)     ; TAB
    (= x 10)    ; Line Feed, LF
    (= x 13)    ; Carriage Return, CR
    (= x 32)))  ; SPACE

; Read an integer written in ASCII Characters from port
(define (read-ascii-integer port)
  (define (helper acc)
    (let ((b (get-u8 port)))
      (if (whitespace? b)
        (reverse acc)
        (helper (cons b acc)))))
  (string->number (utf8->string (u8-list->bytevector (helper '())))))

; Parse the width, height and maxval
(define (parse-parameters port)
  (if (= (lookahead-u8 port) 35)
    (read-line port))
  (let ((maxval (read-ascii-integer port))
        (h (read-ascii-integer port))
        (w (read-ascii-integer port)))
    (parse-matrix w h maxval port)))

; Parse the binary matrix(not exactly stored as a matrix) of w*h bytes.
(define (parse-matrix w h maxval port)
  (cons (list w h maxval) (bytevector->u8-list (get-bytevector-n port (* w h)))))

; Write the PGM to another file
(define (write-ascii-number port x)
  (put-bytevector port
    (string->utf8 (number->string x))))

; Write the width, height and maxval
(define (write-parameters port parameters)
  (write-ascii-number port (car parameters))   ; width
  (put-u8 port 32)    ; space
  (write-ascii-number port (cadr parameters))  ; height
  (put-u8 port 10)    ; newline
  (write-ascii-number port (caddr parameters)) ; maxval
  (put-u8 port 10))   ; newline

(define (write-binary-pgm-file file pgm)
  (let ((port (open-file-output-port file (file-options no-fail))))
    (put-bytevector port #vu8(80 53 10))           ; Magic Number (P5), followed by a newline
    (write-parameters port (car pgm))
    (put-bytevector port (u8-list->bytevector (cdr pgm)))
    (close-port port)))

; Helper functions
(define (split bytes len)
  (define (split-helper bytes i tmp acc)
    (cond ((null? bytes) (reverse (cons (reverse tmp) acc)))
          ((= i len) (split-helper bytes 0 '() (cons (reverse tmp) acc)))
          (else (split-helper (cdr bytes) (+ i 1) (cons (car bytes) tmp) acc))))
  (split-helper bytes 0 '() '()))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (repeat n x)
  (if (= n 0)
    '()
    (cons x (repeat (- n 1) x))))
; PGM Manipulation functions
; Invert the colors of a pgm, using maxval
(define (invert pgm)
  (let ((parameters (car pgm))
        (matrix (cdr pgm)))
    (cons parameters
      (map
        (lambda (x) (- (caddr parameters) x))
        matrix))))

(define (avg items) (floor (/ (accumulate + 0 items) (length items))))

(define (downscale-x bytes)
  (map avg (split bytes 2)))

(define (downscale-y bytes width)
  (accumulate append '()
    (map
      (lambda (tworows)
        (map (lambda (x y) (avg (list x y))) (car tworows) (cadr tworows)))
      (split (split bytes width) 2))))

; SICP Functions
; Assumes that both images have same width, height, maxval.
(define (beside a b)
  (cons (car a)
    (downscale-x
      (accumulate append '()
        (map append (split (cdr a) (caar a)) (split (cdr b) (caar b)))))))

; Assumes that both images have same width, height, maxval.
(define (below a b)
  (cons (car a)
        (downscale-y 
          (append (cdr a) (cdr b))
          (caar a))))

(define (flip-vert image)
  (cons (car image)
        (accumulate append '() 
          (reverse (split (cdr image) (caar image))))))

; rogers-path points to the PGM file containing the image of W.B. Rogers.
(define rogers-path "founder2.pgm")

(define rogers (read-binary-pgm-file rogers-path))