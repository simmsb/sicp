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


;; Exercise 2.2

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(print-point
 (midpoint-segment
  (make-segment
   (make-point -5 -5)
   (make-point -5 5))))

;; Exercise 2.4

(define (cons-proc x y)
  (lambda (m) (m x y)))

(define (car-proc z)
  (z (lambda (p q) p)))

(define (cdr-proc z)
  (z (lambda (p q) q)))

;; Exercise 2.6

(define zero-church (const identity))

(define (succ-church n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one-church
  (lambda (f) (lambda (x) (f x))))

(define two-church
  (lambda (f) (lambda (x) (f (f x)))))

(define (add-church a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; (2 + 2) + (2 + 2)
(((add-church (add-church two-church two-church)
              (add-church two-church two-church))
  add1)
 0)

;; (2 + 2) + (2 + (2 + 1))
(((add-church (add-church two-church two-church)
              (add-church two-church
                          (succ-church two-church)))
  add1)
 0)
