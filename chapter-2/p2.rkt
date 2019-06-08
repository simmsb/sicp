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

(define comp
  (curryr (lambda (x g f)
            (f (g x)))))

(define ap
  (curryr (lambda (x g f)
            (f x (g x)))))

(define join-f
  (curryr (lambda (r f)
            ((f r) r))))

(define succ-church-comb (ap comp))

(define one-church-comb identity)

(define two-church-comb (join-f comp))

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


;; Exercise 2.17
(define (last-pair-s l)
  (if (null? (cdr l))
      l
      (last-pair-s (cdr l))))

(last-pair-s '(23 72))

;; Exercise 2.18
(define (reverse-s l)
  (let loop ([acc null]
             [l l])
    (if (null? l)
        acc
        (loop (cons (car l) acc)
              (cdr l)))))

(reverse-s '(1 2 3 4 5))


;; Exercise 2.20
(define (same-parity p . r)
  (cons p (filter (if (even? p) even? odd?) r)))

(same-parity 2 3 4 5 6)
(same-parity 3 4 5 6 7)


(define (for-each-s f l)
  (let go ([l l])
    (unless (null? l)
      (f (car l))
      (go (cdr l)))))


;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (r)
                            (cons (car s) r))
                          rest)))))


(subsets '(1 2 3 4))


;; Exercise 2.34

(define (horner-eval x coeff-seq)
  (foldr (lambda (coeff higher)
           (+ coeff (* higher x)))
         0
         coeff-seq))

(horner-eval 2 '(1 3 0 5 0 1))

(define empty-board '())

(define (adjoin-position new-row k rest)
  (cons new-row rest))

(define (safe? k positions)
  (let ([first-position (car positions)])
    (let go ([positions (cdr positions)]
             [offset 1])
      (cond
       [(null? positions)
        true]
       [(or (= first-position (car positions))
            (= first-position (+ (car positions) offset))
            (= first-position (- (car positions) offset)))
        false]
       [else
        (go (cdr positions) (add1 offset))]))))

(define (queens board-size)
  (let go ([k board-size])
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (append-map (lambda (rest-of-queens)
                              (map (lambda (new-row)
                                     (adjoin-position new-row k rest-of-queens))
                                   (range 1 (add1 board-size))))
                            (go (sub1 k)))))))

(require sicp-pict)

(define (flipped-pairs p)
  (let ([p2 (beside p (flip-vert p))])
    (below p2 p2)))

(define (right-split p n)
  (if (= n 0)
      p
      (let ([smaller (right-split p (sub1 n))])
        (beside p (below smaller smaller)))))

(define (up-split p n)
  (if (= n 0)
      p
      (let ([smaller (up-split p (sub1 n))])
        (below p (beside smaller smaller)))))

(define (corner-split p n)
  (if (= n 0)
      p
      (let* ([up (up-split p (sub1 n))]
             [right (right-split p (sub1 n))]
             [top-left (beside up up)]
             [bottom-right (below right right)]
             [corner (corner-split p (sub1 n))])
        (beside (below p top-left)
                (below bottom-right corner)))))

(define (square-of-four tl tr bl br)
  (lambda (p)
    (let ([top (beside (tl p) (tr p))]
          [bot (beside (bl p) (br p))])
      (below bot top))))


;; (define flipped-pairs
;;   (square-of-four identity flip-vert identity flip-vert))

(define (square-limit p n)
  (let ([combine4 (square-of-four flip-horiz identity rotate180 flip-vert)])
    (combine4 (corner-split p n))))

(define (split f g)
  (lambda (p n)
    (let loop ([n n])
      (if (= n 0)
          p
          (let ([smaller (loop (sub1 n))])
            (f p (g smaller smaller)))))))

(define right-split-s (split beside below))
(define up-split-s (split below beside))

(require racket/match)

(define (deriv exp var)
  (cond
   [(number? exp)
    0]
   [(symbol? exp)
    (if (eq? exp var) 1 0)]
   [else
    (match exp
      [`(+ ,a ,b)
       `(+ ,(deriv a var)
           ,(deriv b var))]
      [`(* ,a ,b)
       `(+ (* ,a ,(deriv b var))
           (* ,(deriv a var) ,b))]
      [_ (error "bad expr" exp)])]))

(define (simpl exp)
  (let ([rexp (match exp
                [(or `(* ,_ 0) `(* 0 ,_))
                 0]
                [(or `(* ,a 1) `(* 1 ,a))
                 (simpl a)]
                [(or `(+ ,a 0) `(+ 0 ,a))
                 (simpl a)]
                [`(,op0 (,op1 ,a ,b) (,op1 ,b ,a))
                 `(,op0 (,op1 ,a ,b) (,op1 ,a, b))]
                [`(+ ,a ,a)
                 `(* ,(simpl a) 2)]
                [`(,op ,a ,b)
                 `(,op ,(simpl a) ,(simpl b))]
                [_ exp])])
    (if (equal? rexp exp)
        rexp
        (simpl rexp))))
