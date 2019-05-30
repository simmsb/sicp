#lang racket

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

(ackermann 1 10)

(ackermann 2 4)

(ackermann 3 3)


(define (f n) (ackermann 0 n))
;; (f n) = (* 2 n)

(f 1)
(f 2)
(f 3)

(define (g n) (ackermann 1 n))
;; (g n) = (ackermann 0 (ackermann 1 (- n 1)) ;; (f (ackermann 1 (- n 1)))
;; (g n) = (* 2 (ackermann 1 (- n 1)))
;; (g n) = (* 2 (ackermann 0 (ackermann 1 (- n 2))))
;; (g n) = (* 2 (* 2 (ackermann 1 (- n 3))))
;; (g n) = 2 ^ n given n > 0

(g 1)
(g 2)
(g 3)

(define (h n) (ackermann 2 n))
;; (h n) = (ackermann 1 (ackermann 2 (- n 1)))
;; (h n) = (g (ackermann 2 (- n 1)))
;; (h n) = (g (g (ackermann 2 (- n 2))))
;; (h n) = (g (g (g (ackermann 2 (- n 3)))))
;; (h n) = 2 ^ (2 ^ (2 ^ (...)))

(h 1)
(h 2)
(h 3)


;; Exercise 1.11
(define (f-rec n)
  (printf "n = ~a\n" n)
  (cond [(< n 3) n]
        [else (+ (* (f-rec (- n 1)) 1)
                 (* (f-rec (- n 2)) 2)
                 (* (f-rec (- n 3)) 3))]))

(f-rec 1)
(f-rec 3)
(f-rec 4)

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

(f-iter 1)
(f-iter 2)
(f-iter 3)
(f-iter 4)


;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1

;; Exercise 1.12
(define (pascal row elem)
  (cond [(<= elem 0)   1]
        [(>= elem row) 1]
        [else (+ (pascal (sub1 row) (sub1 elem))
                 (pascal (sub1 row) elem))]))


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

;; Exercise 1.14
;;
;; (count-change 11)
;; : 11 5
;;   : 11 4
;;    : 11 3
;;     : 11 2
;;      : 11 1
;;       : 11 0
;;       : 10 1
;;        : 10 0
;;        : 9 1
;;         : 9 0
;;         : 8 1
;;          : 8 0
;;          : 7 1
;;           : 7 0
;;           : 6 1
;;            : 6 0
;;            : 5 1
;;             : 5 0
;;             : 4 1
;;              : 4 0
;;              : 3 1
;;               : 3 0
;;               : 2 1
;;                : 2 0
;;                : 1 1
;;                 : 1 0
;;                 : 0 1
;;      : 6 2
;;       : 6 1
;;        : 6 0
;;        : 5 1
;;         : 5 0
;;         : 4 1
;;          : 4 0
;;          : 3 1
;;           : 3 0
;;           : 2 1
;;            : 2 0
;;            : 1 1
;;             : 1 0
;;             : 0 1
;;       : 1 2
;;        : 1 1
;;         : 1 0
;;         : 0 1
;;        : -4 2
;;     : 1 3
;;      : 1 2
;;       : 1 1
;;        : 1 0
;;        : 0 1
;;       : -4 2
;;      : -9 3
;;    : -14 4
;;   : -39 5
;; 4

;; n = amount to be changed
;; Space growth is O(n) -- nead n recursions deep when we get to the smallest cent
;; Iteration growth is O(n^2), as n increases the number of ways we can divide up
;; each remaining value increases


;; Exercise 1.15
;; a: Applied 4 times
;; b: Space and step space grow by same value as recursion is linear
;;    Function is O(log(n)) where n is the size of the number


;; Exercise 1.16


(define (square x) (* x x))

(define (exp b n)
  (let go ([a 1]
           [b b]
           [n n])
    (cond [(= n 0) a]
          [(even? n) (go a
                         (square b)
                         (/ n 2))]
          [else (go (* a b)
                    b
                    (- n 1))])))
(exp 3 2)
(exp 4 0)
(exp 4 1)
(exp 4 2)
(exp 4 3)
(exp 4 4)
(exp 4 5)
(exp 4 6)


(define (double x) (+ x x))
(define (halve x) (arithmetic-shift x -1))


(define (mul-0 x y)
  (cond [(= y 0) 0]
        [(= y 1) x]
        [(even? y) (double (mul-0 x (halve y)))]
        [else (+ x (mul-0 x (- y 1)))]))

(mul-0 0 2) ;=> 0
(mul-0 2 0) ;=> 0
(mul-0 1 2) ;=> 2
(mul-0 2 1) ;=> 2
(mul-0 3 4) ;=> 12
(mul-0 4 3) ;=> 12



(define (mul-1 x y)
  (define (go a y)
    (cond [(= y 0) 0]
          [(= y 1) a]
          [(even? y) (go (double a)
                         (halve y))]))
  (if (even? y)
      (go x y)
      (+ x (go x (- y 1)))))

(mul-1 0 2) ;=> 0
(mul-1 2 0) ;=> 0
(mul-1 1 2) ;=> 2
(mul-1 2 1) ;=> 2
(mul-1 3 4) ;=> 12
(mul-1 4 3) ;=> 12



;; Exercise 1.19

(module* exc-1-19 #f
  ;; a <- bq + aq + ap
  ;; b <- bp + aq
  ;;
  ;; a' <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
  ;; b' <- (bp + aq)p + (bq + aq + ap)q
  ;;
  ;; a' <- bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2
  ;; a' <- a(2q^2 + 2qp + p^2) + b(q^2 + 2qp)
  ;;
  ;; b' <- bp^2 + aqp + bq^2 + aq^2 + aqp
  ;; b' <- a(q^2 + 2qp) + b(q^2 + p^2)
  ;;
  ;; a' <- b(q^2 + 2qp) + a(q^2 + 2qp) + a(q^2 + p^2)
  ;; b' <- b(q^2 + p^2)  + a(q^2 + 2qp)
  ;;
  ;; q' <- q^2 + 2qp
  ;; p' <- q^2 + p^2

  (define (fib n)
    (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond [(= count 0) b]
          [(even? count)
           (fib-iter a
                     b
                     (+ (* q q) (* p p))
                     (+ (* q q) (* q p 2))
                     (/ count 2))]
          [else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))]))

  (fib 0) ;=> 0
  (fib 1) ;=> 1
  (fib 2) ;=> 1
  (fib 3) ;=> 2
  (fib 4) ;=> 3
  (fib 5)) ;=> 5



;; Exercise 1.20

;; (gcd 206 40)

;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))

;; (if (= (remainder 206 40) 0) -- 1 remainder
;;     40
;;     (gcd (remainder 206 40)
;;          (remainder 40 (remainder 206 40))))

;;  (if (= 6 0) ...)

;; (if (= (remainder 40 (remainder 206 40)) 0) -- 2 remainder
;;     (remainder 206 40)
;;     (gcd (remainder 40 (remainder 206 40))
;;          (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))))

;; (if (= 4 0) ...)

;; (if (= (remainder (remainder 206 40)
;;                   (remainder 40 (remainder 206 40))))  -- 4 remainder
;;     (remainder 40 (remainder 206 40))
;;     (gcd (remainder (remainder 206 40)
;;                     (remainder 40 (remainder 206 40)))
;;          (remainder (remainder 40 (remainder 206 40))
;;                     (remainder (remainder 206 40)
;;                                (remainder 40 (remainder 206 40))))))

;; (if (= 2 0) ...)

;; (if (= (remainder (remainder 40 (remainder 206 40))  -- 7 remainder
;;                   (remainder (remainder 206 40)
;;                              (remainder 40 (remainder 206 40))))
;;        0)
;;     (remainder (remainder 206 40)
;;                (remainder 40 (remainder 206 40)))
;;     (gcd (remainder (remainder 40 (remainder 206 40))
;;                     (remainder (remainder 206 40)
;;                                (remainder 40 (remainder 206 40))))
;;          (remainder (remainder (remainder 206 40)
;;                                (remainder 40 (remainder 206 40)))
;;                     (remainder (remainder 40 (remainder 206 40))
;;                                (remainder (remainder 206 40)
;;                                           (remainder 40 (remainder 206 40)))))))

;; (if (= 0 0) ...)

;; (remainder (remainder 206 40)
;;            (remainder 40 (remainder 206 40))) ;=> 2 -- 4 remainder

;; total remainder operations: 4 + 7 + 4 + 2 + 1
(+ 4 7 4 2 1) ;=> 18

;; applicative order:
;; (gcd 206 40)
;;
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40))) -- 1 remainder

;; (gcd 40 6)

;; (if (= 6 0)
;;     40
;;     (gcd 6 (remainder 40 6))) -- 1 remainder

;; (gcd 6 4)

;; (if (= 4 0)
;;     6
;;     (gcd 4 (remainder 6 4))) -- 1 remainder

;; (gcd 4 2)

;; (if (= 2 0)
;;     4
;;     (gcd 2 (remainder 4 2))) -- 1 remainder

;; (gcd 2 0)

;; (if (= 0 0)
;;     2
;;     (gcd 0 (remainder 2 0))) -- 1 remainder

;; 2
;; total remainder operations: 5



(define (smallest-divisor n)
  (find-divisor n 2))

(define (next-divisor-test n)
  (cond [(= n 2) 3]
        [else (+ 2 n)]))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (next-divisor-test test-divisor))]))

(define (divides? a b)
  (= (remainder b a) 0))


;; Exercise 1.21


(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)


;; Exercise 1.22


(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m)]
        [else (remainder (* base (expmod base (- exp 1) m))
                         m)]))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) #t]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else #f]))

(define (prime? n)
  (fast-prime? n 10))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (let ([isp (fast-prime? n 10)])
    (when isp
      (report-prime (- (current-inexact-milliseconds) start-time)))
    isp))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start count)
  (let go ([n (if (even? start)
                  (add1 start)
                  start)]
           [found '()])
    (cond [(= (length found) count) found]
          [(timed-prime-test n)
           (go (+ 2 n) (cons n found))]
          [else (go (+ 2 n) found)])))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

;; Exercise 1.27

(define (slow-prime? n)
  (= n (smallest-divisor n)))

(define (all-congruent? n)
  (let loop ([a 1])
    (cond [(= a n) #t]
          [(= (expmod a n n) a) (loop (add1 a))]
          [else #f])))

(all-congruent? 3)
(all-congruent? 561)

(define (carmichael? n)
  ;; carmichael numbers are where all-congruent is true, but not prime
  (and (all-congruent? n) (not (slow-prime? n))))

(carmichael? 561)
(carmichael? 3)
(carmichael? 2)
(carmichael? 7)


;; Excercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpsons-integral f a b n)
  (let ([h (/ (- b a) n)])
    (define (add-2h n)
      (+ n (* 2 h)))
    (let ([fst (f a)]
          [lst (f b)]
          [fours (sum f (+ a h) add-2h b)]
          [twos (sum f (add-2h a) add-2h b)])
      (/ (* h
            (+ fst
               (* 4 fours)
               (* 2 twos)
               lst))
         3))))

(define (cube x)
  (* x x x))

(simpsons-integral cube 0 1 100)
(simpsons-integral cube 0 1 1000)


;; Exercise 1.30

(define (sum-iter term a next b)
  (let loop ([a a]
             [result 0])
    (cond [(> a b) result]
          [else (loop (next a)
                      (+ result (term a)))])))

(sum identity 0 add1 10)
(sum-iter identity 0 add1 10)

;; Exercise 1.31

(define (product-iter term a next b)
  (let loop ([a a]
             [result 1])
    (cond [(> a b) result]
          [else (loop (next a)
                      (* result (term a)))])))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (fac n)
  (product-iter identity 1 add1 n))

(fac 0)
(fac 1)
(fac 2)
(fac 3)

(define (add2 n)
  (+ n 2))

(define (pi/4 n)
  (/ (* 2 n
        (square (product-iter identity 4 add2 (sub1 n))))
     (square (product-iter identity 3 add2 n))))

(pi/4 100)

;; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (let loop ([a a]
             [result null-value])
    (cond [(> a b) result]
          [else (loop (next a)
                      (combiner result (term a)))])))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))


;; Exercise 1.33

(define (filtered-accumulate pred combiner null-value term a next b)
  (let loop ([a a]
             [result null-value])
    (cond [(> a b) result]
          [(pred a) (loop (next a)
                          (combiner result (term a)))]
          [else (loop (next a) result)])))

(define (sum-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square a next-divisor-test b))


;; Exercise 1.37

(define (cont-frac n d k)
  (let loop ([k k]
             [acc 0])
    (if (= 0 k)
        acc
        (loop (sub1 k)
              (/ (n k)
                 (+ (d k)
                    acc))))))

(define (1/phi k)
  (cont-frac (const 1.0)
             (const 1.0)
             k))

(1/phi 100000)

;; Exercise 1.38

(define (e k)
  (cont-frac (const 1.0)
             (lambda (i)
               (if (divides? 3 (add1 i))
                   (/ (* 2 (add1 i)) 3)
                   1))
             k))

(e 100000)

;; Exercise 1.39

(define (tan-cf x k)
  (- (cont-frac (lambda (i) (- (exp x i)))
                (lambda (i) (- (* 2 i) 1))
                k)))

(tan-cf 0.1 100)
(tan-cf 3.14 100)
(tan-cf 0 100)
(tan-cf (/ 3.141 2) 100)
(tan-cf (/ 3.142 2) 100)

;; Exercise 1.41

(define (double-app f)
  ;; (compose f f)
  (lambda (x) (f (f x))))

;; Exercise 1.42

(define (compose-f f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (sub1 n)) f)))

((repeated square 3) 5)

;; Exercise 1.44

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3)))

((smooth square 0.01) 3.4)

(define (n-fold-smooth f n dx)
  (repeated (smooth f dx) n))

((n-fold-smooth square 4 0.1) 3.4)
