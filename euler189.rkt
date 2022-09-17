#lang slideshow

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
