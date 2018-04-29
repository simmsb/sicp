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


;; Exercise 1.9

;; (define (+ a b)
;;   (if (= a 0)
;;     b
;;     (inc (+ (dec a) b))))
;;
;; (+ 4 5)
;;
;; (inc (+ (dec 4) 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (inc (+ (dec 2) 5))))
;; (inc (inc (inc (inc (+ (dec 1) 5)))))
;; (inc (inc (inc (inc 5))))

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))
;;
;; (+ 4 5)
;;
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9


;; Exercise 1.10

(define (ackermann x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (ackermann (- x 1)
                         (ackermann x (- y 1)))]))

(ackermann 1 10) ;=> 1024

(ackermann 2 4) ;=> 65536

(ackermann 3 3) ;=> 65536


(define (f n) (ackermann 0 n))
;; (f n) = (* 2 n)

(f 1) ;=> 2
(f 2) ;=> 4
(f 3) ;=> 6

(define (g n) (ackermann 1 n))
;; (g n) = (ackermann 0 (ackermann 1 (- n 1)) ;; (f (ackermann 1 (- n 1)))
;; (g n) = (* 2 (ackermann 1 (- n 1)))
;; (g n) = (* 2 (ackermann 0 (ackermann 1 (- n 2))))
;; (g n) = (* 2 (* 2 (ackermann 1 (- n 3))))
;; (g n) = 2 ^ n given n > 0

(g 1) ;=> 2
(g 2) ;=> 4
(g 3) ;=> 8

(define (h n) (ackermann 2 n)) ;=> #<void>
;; (h n) = (ackermann 1 (ackermann 2 (- n 1)))
;; (h n) = (g (ackermann 2 (- n 1)))
;; (h n) = (g (g (ackermann 2 (- n 2))))
;; (h n) = (g (g (g (ackermann 2 (- n 3)))))
;; (h n) = 2 ^ (2 ^ (2 ^ (...)))

(h 1) ;=> 2
(h 2) ;=> 4
(h 3) ;=> 16


;; Exercise 1.11
(module* exc-1-11 #f
  (define (f-rec n)
    (printf "n = ~a\n" n)
    (cond [(< n 3) n]
          [else (+ (* (f-rec (- n 1)) 1)
                   (* (f-rec (- n 2)) 2)
                   (* (f-rec (- n 3)) 3))]))

  (f-rec 1) ;=> 1
  (f-rec 3) ;=> 4
  (f-rec 4) ;=> 11

  (define (f-iter n)
    (cond [(< n 3) n]
          [else (let go ([a 2]
                         [b 1]
                         [c 0]
                         [cnt 0])
                  (cond [(= cnt (- n 2)) a]
                        [else (go
                               (+ a
                                  (* 2 b)
                                  (* 3 c))
                               a
                               b
                               (add1 cnt))]))]))

  (f-iter 1) ;=> 1
  (f-iter 2) ;=> 2
  (f-iter 3) ;=> 4
  (f-iter 4) ;=> 11
)


;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1

;; Exercise 1.12
(module* exc-1-12 #f
  (define (pascal row elem)
    (cond [(<= elem 0)   1]
          [(>= elem row) 1]
          [else (+ (pascal (sub1 row) (sub1 elem))
                   (pascal (sub1 row) elem))]))

  (pascal 0 0)
  (pascal 2 1)
  )


;; Exercise 1.13
;; p = 𝟇 = (1 + sqrt(5)) / 2
;; s = 𝟁 = (1 - sqrt(5)) / 2
;; given fib(n) = (p^n - s^n) / sqrt(5)
;;
;; fib(0) = (1 - 1) / sqrt(5)
;; fib(0) = 0
;;
;; fib(1) = ( (1 + sqrt(5) / 2) - (1 - sqrt(5) / 2) ) / sqrt(5)
;; fib(1) = ( (sqrt(5) + sqrt(5)) / 2) / sqrt(5)
;; fib(1) = sqrt(5) / sqrt(5)
;; fib(1) = 1
;;
;;
;; try:
;; fib(n + 1) = (p^(n + 1) - s^(n + 1)) / sqrt(5)
;;
;; fib(n + 1) = fib(n) + fib(n - 1)
;; fib(n + 1) = (p^n - s^n) / sqrt(5) + (p^(n-1) - s^(n-1)) / sqrt(5)
;;            = ((p^n + p^(n-1)) - (s^n + s^(n-1))) / sqrt(5)
;;
;;            a^n       = a^(n + 1) / a
;;            a^(n - 1) = a^(n + 1) / a^2
;;
;;            a^n + a^(n-1) = a^(n + 1) * a^-1 + a^(n + 1) * a^-2
;;                          = a^(n + 1) * (a^-1 + a^-2)
;;
;;            = ((p^(n + 1) * (p^-1 + p^-2)) - (s^(n + 1) * (s^-1 + s^-2))) / sqrt(5)
;;
;;
;;                  p^-1 = 1 / p
;;                       = 1 / ((1 + sqrt(5)) / 2)
;;                       = (sqrt(5) - 1) / 2
;;                       = p - 2/2
;;                       = p - 1
;;
;;                  p^-2 = (p^-1)^2
;;                       = (p - 1)^2
;;                       = (3 - sqrt(5)) / 2
;;                       = 2 - p
;;
;;                 p^-1 + p^-2 = (p - 1) + (2 - p)
;;                             = 1
;;
;;
;;                 s^-1 = 1 / s
;;                      = 1 / ((1 - sqrt(5)) / 2)
;;                      = (-1 - sqrt(5)) / 2
;;                      = s - 1
;;
;;                 s^-2 = (s^-1)^2
;;                      = (s - 1)^2
;;                      = (3 + sqrt(5)) / 2
;;                      = 2 - s
;;
;;                 s^-1 + s^-2 = (s - 1) + (2 - s)
;;                             = 1
;;
;;           = ((p^(n + 1) * 1) - (s^(n + 1) * 1)) / sqrt(5)
;;           = (p^(n + 1) - s^(n + 1)) / sqrt(5)
;;
;; proving fib(n) = (p^n - s^n) / sqrt(5)
;;
;; for fib(n) to be the closest number to p^n/sqrt(5)
;; | fib(n) - p^n/sqrt(5) | <= 0.5
;;
;; thus
;; | ((p^n - s^n) - p^n) / sqrt(5) | <= 0.5
;; | -s^n / sqrt(5) | <= 0.5
;; | s^n | = 0.5 * sqrt(5)
;;
;; s^n = ((1 - sqrt(5)) / 2)^n
;;     = -0.618 ^ n
;;
;; | 0.618 ^ n | <= 1.118
;;
;; as any number x^n where 0 <= x <= 1 and n >= 0 is <= 1
;; 0.618 ^ n must always be less than 1 and thus less than 1.118
;;
;; proving fib(n) is always closest to p^n / sqrt(5)
