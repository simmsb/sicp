#lang racket

(define size 2)

(define pi 3.14159)

(define radius 10)

(define circumference (* 2 pi radius))


(define (square x) (* x x))

(define (abs x)
  (cond [(< 0 x) x]
        [(= 0 x) 0]
        [(> 0 x) (- x)]))


;; Exercise 1.1


10 ; => 10

(+ 5 3 4) ; => 12

(- 9 1) ; => 8

(/ 6 2) ; => 3

(+ (* 2 4) (- 4 6)) ; => 6

(define a 3) ;=> #<void>

(define b (+ a 1)) ;=> #<void>

(+ a b (* a b)) ;=> 19

(= a b) ;=> #f

(if (and (> b a) (< b (* a b)))
    b
    a) ;=> 4

(cond [(= a 4) 5]
      [(= b 4) (+ 6 7 a)]
      (else 25)) ;=> 16

(+ 2 (if (> b a) b a)) ;=> 6

(* (cond [(> a b) a]
         [(< a b) b]
         [else -1])
   (+ a 1)) ;=> 16


;; Exercise 1.2


(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ;=> -37/150


;; Exercise 1.3


(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (max-square a b c)
  (cond [(= (min a b c) a) (sum-of-squares b c)]
        [(= (min a b c) b) (sum-of-squares a c)]
        [(= (min a b c) c) (sum-of-squares a b)]))

(max-square 1 2 3) ;=> 13

(= (max-square 6 9 4) (sum-of-squares 6 9)) ;=> #t


;; Exercise 1.4


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; if b > 0,  apply a b to +  (+ a b)
;; if b <= 0, apply a b to -  (- a b)


;; Exercise 1.5


(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))

;; for applicative order
;; evaluate (test 0 (p))
;; evaluate 0
;; evaluate (p)
;; evaluate ((p))
;; evaluate (((p)))
;; etc ...
;; recurse forever

;; for normal order
;; evaluate (test 0 (p))
;; evaluate (if (= 0 0)
;;              0
;;              (p))
;; evaluate (= 0 0)
;; evaluate 0
;; final result: 0


;; iterative sqrt

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

(sqrt-iter 1.0 25) ;=> 5.000023178253949


;; Exercise 1.6
;;
;; As new-if is now a procedure, all parameters are evaluated
;; As sqrt-iter is called in the parameter list the result is the recursive calling of sqrt-iter


;; Exercise 1.7
;; For small numbers, (- (square guess) x) quickly becomes smaller than 0.001, as (square guess) is very close to x, leading to small values of (- (sqrt guess) x)
;; For large numbers, takes many iterations to become smaller than 0.001 as (square guess) is significantly larger than x

(sqrt (square 0.000000000000000000001)) ;=> 0.03125


(define (sqrt-iter-better guess last x)
  (if (good-enough-better? guess last)
      guess
      (sqrt-iter-better (improve guess x)
                        guess
                        x)))

(define (good-enough-better? guess last)
  (< (abs (- guess last)) 0.000001))

(define (sqrt-better x)
  (sqrt-iter-better 1.0 x x))

(sqrt-better (square 0.000000000000000000001)) ;=> 9.5367431640625e-07


;; Exercise 1.8

(define (cbrt-iter guess last x)
  (if (good-enough-better? guess last)
      guess
      (cbrt-iter (cbrt-improve guess x)
                 guess
                 x)))

(define (cbrt-improve y x)
  (/ (+ (/ x (square y))
        (* 2 y))
     3))

(define (cbrt x)
  (cbrt-iter 1.0 x x))

(cbrt (* 3 3 3)) ;=> 3.0000000000000977
