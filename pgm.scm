; Copyright 2019 Anurag Ohri

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.

; For more information, contact me at anuragohri92@gmail.com

; This is a very basic PGM parser library.
; It does NOT correctly parse PGM files that have maxval greater than 255.
; Also, COMMENTS are supported just below the magic number line. Any other comments will cause an error.

(import (rnrs)
        (rnrs io ports)
        (scheme))

(library (pgm)
  (export read-binary-pgm-file
          write-binary-pgm-file
          below
          beside
          flip-vert
          flip-horiz
          rogers)
  
  (import (scheme))

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
    (cons (list w h maxval)
      (split
        (bytevector->u8-list (get-bytevector-n port (* w h)))
        w)))

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
      (put-bytevector port (u8-list->bytevector (concat (cdr pgm))))
      (close-port port)))

  ; Helper functions
  (define (split-at n items)
    (define (iter items result i)
      (if (= i 0)
        (cons (reverse result) items)
        (iter (cdr items) (cons (car items) result) (- i 1))))
    (iter items '() n))

  (define (split bytes len)
    (define (iter items result)
      (if (null? items)
        (reverse result)
        (let ((x (split-at len items)))
          (iter (cdr x) (cons (car x) result)))))
    (iter bytes '()))

  (define (accumulate op initial sequence)
    (define (iter items result)
      (if (null? items)
        result
        (iter (cdr items) (op (car items) result))))
    (iter sequence initial))

  (define (concat lists)
    (accumulate append '() (reverse lists)))

  (define (repeat n x)
    (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))

  (define (map-matrix f img)
    (cons
      (car img)
      (map
        (lambda (row) (map f row))
        (cdr img))))

  ; PGM Manipulation functions
  ; Invert the colors of a pgm, using maxval
  (define (invert pgm)
    (map-matrix
      (lambda (x) (- (caddar pgm) x))
      pgm))

  (define (avg items) (floor (/ (accumulate + 0 items) (length items))))

  (define (downscale-x bytes)
    (map avg (split bytes 2)))

  (define (downscale-y rows)
    (map
      (lambda (tworows)
              (map
                (lambda (a b) (avg (list a b)))
                (car tworows) (cadr tworows)))
      (split rows 2)))

  ; SICP Functions
  ; Assumes that both images have same width, height, maxval.
  (define (beside a b)
    (cons (car a)
          (map
            (lambda (r1 r2) (downscale-x (append r1 r2)))
            (cdr a) (cdr b))))

  ; Assumes that both images have same width, height, maxval.
  (define (below a b)
    (cons (car a)
          (downscale-y 
            (append (cdr b) (cdr a)))))

  (define (flip-vert image)
    (cons (car image)
          (reverse (cdr image))))

  (define (flip-horiz image)
    (cons (car image)
          (map reverse (cdr image))))

  ; rogers-path points to the PGM file containing the image of W.B. Rogers.
  (define rogers-path "founder2.pgm")

  (define rogers (read-binary-pgm-file rogers-path))
  )