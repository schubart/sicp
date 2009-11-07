(define (assert a)
  (if a
      #t
      (error "Not true:" a)))

(define (assert-false a)
  (if (not a)
      #t
      (error "Not false:" a)))

(define (assert-= a b)
  (if (= a b)
      #t
      (error "Not equal:" a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-= 486 (+ 137 349))
(assert-= 666 (- 1000 334))
(assert-= 495 (* 5 99))
(assert-= 2 (/ 10 5))
(assert-= 12.7 (+ 2.7 10))

(assert-= 75 (+ 21 35 12 7))
(assert-= 1200 (* 25 4 12))

(assert-= 19 (+ (* 3 5) (- 10 6)))

(assert-= 57
          (+ (* 3
                (+ (* 2 4)
                   (+ 3 5)))
             (+ (- 10 7)
                6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define size 2)

(assert-= 2 size)
(assert-= 10 (* 5 size))
; etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-= 390
          (* (+ 2 (* 4 6))
             (+ 3 5 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(assert-= 441 (square 21))
(assert-= 49 (square (+ 2 5)))
(assert-= 81 (square (square 3)))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(assert-= 25 (sum-of-squares 3 4))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(assert-= 136 (f 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-abs abs-function)
  (assert-= 10 (abs-function  10))
  (assert-=  0 (abs-function   0))
  (assert-= 10 (abs-function -10)))
(test-abs abs)

(define (abs1 x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(test-abs abs1)

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))
(test-abs abs2)

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))
(test-abs abs3)

(define (test-ge ge-function)
  (assert       (ge-function 4 3))
  (assert       (ge-function 4 4))
  (assert-false (ge-function 3 4)))
(test-ge >=)

(define (ge1 x y)
  (or (> x y)
      (= x y)))
(test-ge ge1)

(define (ge2 x y)
  (not (< x y)))
(test-ge ge2)

; Exercise 1.1

(assert-= 10 10)
(assert-= 12 (+ 5 3 4))
(assert-= 8 (- 9 1))
(assert-= 3 (/ 6 2))
(assert-= 6 (+ (* 2 4) (- 4 6)))
(define a 3)
(define b (+ a 1))
(assert-= 19 (+ a b (* a b)))
(assert-false (= a b))
(assert-= 4 (if (and (> b a) (< b (* a b)))
                b
                a
                ))
(assert-= 16 (cond ((= a 4) 6)
                   ((= b 4) (+ 6 7 a))
                   (else 25)))
(assert-= 6 (+ 2 (if (> b a) b a)))
(assert-= 16 (* (cond ((> a b) a)
                      ((< a b) b)
                      (else -1))
                (+ a 1)))

(assert-= (/ -37 150)
          (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
             (* 3 (- 6 2) (- 2 7))))

; Exercise 1.2

(define (sum-of-squares-of-two-largest x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((<= y z)                (sum-of-squares x z))
        (else                    (sum-of-squares x y))))

(assert-= 25 (sum-of-squares-of-two-largest 2 3 4))
(assert-= 25 (sum-of-squares-of-two-largest 3 2 4))
(assert-= 25 (sum-of-squares-of-two-largest 2 4 3))
(assert-= 25 (sum-of-squares-of-two-largest 3 4 2))
(assert-= 25 (sum-of-squares-of-two-largest 4 2 3))
(assert-= 25 (sum-of-squares-of-two-largest 4 3 2))

; Exercise 1.4

; a and b get combined by "+" if b is positive, by "-" otherwise. This
; effectively adds (abs b) to a.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(assert-= 10 (a-plus-abs-b 7  3))
(assert-= 10 (a-plus-abs-b 7 -3))

; Exercise 1.5

; With applicative-order evaluation, "(p)" will be evaluated first before
; applying (test ...). Since p never terminates, the evaluation never 
; terminates.
;
; With normal-order-evaluation, (test ...) is applied first. Since x is 0,
; and (if) short-circuits, y is never evaluated. The expression terminates and
; returns 0.

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
; (test 0 (p)) ;; Not running this because it hangs the interpreter.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y)
     2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(assert-= 3.00009155413138   (sqrt 9))
(assert-= 11.704699917758145 (sqrt (+ 100 37)))
(assert-= 1.7739279023207892 (sqrt (+ (sqrt 2) (sqrt 3))))
(assert-= 1000.000369924366  (square (sqrt 1000)))

; Exercise 1.6

; new-if always evaluates its three arguments first, even if good-enough? 
; returns true. Therefore, the recursion does not terminate.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else      else-clause)))
(assert-= 5 (new-if (= 2 3) 0 5))
(assert-= 0 (new-if (= 1 1) 0 5))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (new-sqrt-iter (improve guess x)
			 x)))
; (new-sqrt-iter 1.0 2) ; Commented out because it causes stack overflow

