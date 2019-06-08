#lang racket

(define (make-rat n d)
  (let ([g (gcd n d)]
        [neg (xor (>= n 0)
                  (>= d 0))]
        [n (abs n)]
        [d (abs d)])
    (cons (/ n g (if neg -1 1))
          (/ d g))))

(define numer car)
(define denom cdr)

(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r)))

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
               (* (numer b) (denom b)))
            (* (denom a) (denom b))))

(define (mul-rat a b)
  (make-rat (* (numer a) (numer b))
            (* (denom a) (denom b))))

(define one-half (make-rat 1 2))

(print-rat one-half)

(add-rat one-half one-half)

(make-rat -3 4)
(make-rat 3 -4)
(make-rat -3 -4)
