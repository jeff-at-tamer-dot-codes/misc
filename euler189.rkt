#lang slideshow

; Author: Naked Jeff <jeff@tamer.codes>
; See https://projecteuler.net/problem=189

(require memo)

(define-syntax assert-nonnegative-integer
  (syntax-rules ()
    [(assert-nonnegative-integer var body)
     (let ([nonnegative-integer?
            (lambda (expr) (and (integer? expr) (>= expr 0)))])
     (if
      (nonnegative-integer? var) body
      (raise
       (~a "`" (quote var) "`"
        " must be a nonnegative integer, but was: " var))))]))

(define (count-colourings num-triangles num-colours)
  (define/memoize
    (recurse num-triangles
             row-index column-index
             colour-index left-colour-index colour-queue mult)
    (if (zero? num-triangles) 1
    (if (zero? column-index)
        (recurse num-triangles
                 (add1 row-index) (add1 (* 2 row-index))
                 (sub1 num-colours) -1 colour-queue mult)
    (if (< colour-index 0) 0
    (let* (
      [even-column? (even? column-index)]
      [invalid-colour?
        (or (= colour-index left-colour-index)
            (and even-column?
                 (= colour-index (modulo colour-queue num-colours))))]
      [new-colour-queue
        (if even-column? (floor (/ colour-queue num-colours))
            (+ colour-queue (* mult colour-index)))]
      [new-mult
        ((if even-column? / *) mult num-colours)]
      [this-colour-count
        (if invalid-colour? 0
            (recurse (sub1 num-triangles)
                     row-index (sub1 column-index) (sub1 num-colours)
                     colour-index new-colour-queue new-mult))]
      [next-colour-count
        (recurse num-triangles
                 row-index column-index
                 (sub1 colour-index) left-colour-index colour-queue mult)])
    (+ this-colour-count next-colour-count))))))
  (assert-nonnegative-integer num-triangles
  (assert-nonnegative-integer num-colours
  (recurse num-triangles 0 0 0 0 0 1))))

(define count-colorings count-colourings)

; The above code solves the problem. The following code explores visualization:

(require racket/draw)
(require racket/gui)

(define SIZE 16)
(define HEIGHT (* SIZE (sqrt 3)))
(define CENTER 200)
(define target (make-bitmap (* 2 CENTER) (* 2 CENTER)))
(define dc (new bitmap-dc% [bitmap target]))

(define (triangle op x y color)
  (define path (new dc-path%))
  (define new-y (op y HEIGHT))
  (send path move-to x y)
  (send path line-to (- x SIZE) new-y)
  (send path line-to (+ x SIZE) new-y)
  (send path line-to x y)
  (send dc set-brush color 'solid)
  (send dc draw-path path))

(define (triangle-up x y color) (triangle + x y color))
(define (triangle-down x y color) (triangle - x y color))

(triangle-up CENTER 0 "RED")

(triangle-up (- CENTER SIZE) HEIGHT "BLUE")
(triangle-down CENTER (* 2 HEIGHT) "GREEN")
(triangle-up (+ CENTER SIZE) HEIGHT "BLUE")

(triangle-up   (+ CENTER (* -2 SIZE)) (* 2 HEIGHT) "GREEN")
(triangle-down (+ CENTER (* -1 SIZE)) (* 3 HEIGHT) "RED")
(triangle-up   (+ CENTER (*  0 SIZE)) (* 2 HEIGHT) "BLUE")
(triangle-down (+ CENTER (*  1 SIZE)) (* 3 HEIGHT) "RED")
(triangle-up   (+ CENTER (*  2 SIZE)) (* 2 HEIGHT) "BLUE")

(define my-triangle-image (make-object image-snip% target))
