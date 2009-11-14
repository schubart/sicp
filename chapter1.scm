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

(define (test-sqrt)
  (assert-= 3.00009155413138   (sqrt 9))
  (assert-= 11.704699917758145 (sqrt (+ 100 37)))
  (assert-= 1.7739279023207892 (sqrt (+ (sqrt 2) (sqrt 3))))
  (assert-= 1000.000369924366  (square (sqrt 1000))))

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
(test-sqrt)

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

; Exercise 1.7

; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.1.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: "average" still defined outside.
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
(test-sqrt)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(test-sqrt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(assert-= 720 (factorial 6))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))
(assert-= 720 (factorial 6))

; Exercise 1.9

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

; plus1 generates a recursive process:
(define (plus1 a b)
  (if (= a 0)
      b
      (inc (plus1 (dec a) b))))

(assert-= 9 (plus1 4 5))
(assert-= 9 (inc (plus1 3 5)))
(assert-= 9 (inc (inc (plus1 2 5))))
(assert-= 9 (inc (inc (inc (plus1 1 5)))))
(assert-= 9 (inc (inc (inc (inc (plus1 0 5))))))
(assert-= 9 (inc (inc (inc (inc 5)))))
(assert-= 9 (inc (inc (inc 6))))
(assert-= 9 (inc (inc 7)))
(assert-= 9 (inc 8))
(assert-= 9 9)

; plus2 generates an iterative process:
(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))

(assert-= 9 (plus2 4 5))
(assert-= 9 (plus2 3 6))
(assert-= 9 (plus2 2 7))
(assert-= 9 (plus2 1 8))
(assert-= 9 (plus2 0 9))
(assert-= 9 9)

; Exercise 1.10

; This definition of the Ackermann function is strange. See
; http://eli.thegreenplace.net/2007/06/26/sicp-section-121/#fn3
; http://list.cs.brown.edu/pipermail/plt-scheme/2007-September/020754.html
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(assert-= 10 (A 0 5))
(assert-= 10 10)
; -> (A 0 n) computes 2*n:
(define (f n) (A 0 n))
(assert-= 0 (f 0))
(assert-= 2 (f 1))
(assert-= 4 (f 2))
(assert-= 6 (f 3))

(assert-= 1024 (A 1 10))
(assert-= 1024 (A 0 (A 1 9)))
(assert-= 1024 (A 0 (A 0 (A 1 8))))
; ...
(assert-= 1024 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1)))))))))))
(assert-= 1024 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))
(assert-= 1024 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4)))))))))
; ...
(assert-= 1024 (A 0 512))
(assert-= 1024 1024)
; -> (A 1 n) computes 2^n:
(define (g n) (A 1 n))
(assert-= 0    (g 0))
(assert-= 2    (g 1))
(assert-= 4    (g 2))
(assert-= 8    (g 3))
(assert-= 1024 (g 10))

(assert-= 65536 (A 2 4))
(assert-= 65536 (A 1 (A 2 3)))
(assert-= 65536 (A 1 (A 1 (A 1 2))))
(assert-= 65536 (A 1 (A 1 4)))
(assert-= 65536 (A 1 16))
(assert-= 65536 65536)
; -> (A 2 n) computes 2^(2^(...)):
(define (h n) (A 2 n))
(assert-= 0     (h 0))
(assert-= 2     (h 1))
(assert-= 4     (h 2))
(assert-= 16    (h 3))
(assert-= 65536 (h 4))

(assert-= 65536 (A 3 3))
(assert-= 65536 (A 2 (A 3 2)))
(assert-= 65536 (A 2 (A 2 (A 3 1))))
(assert-= 65536 (A 2 (A 2 2)))
(assert-= 65536 (A 2 4))
(assert-= 65536 65536)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-fib fib-fn) 
  (assert-= 0  (fib-fn 0))
  (assert-= 1  (fib-fn 1))
  (assert-= 1  (fib-fn 2))
  (assert-= 2  (fib-fn 3))
  (assert-= 3  (fib-fn 4))
  (assert-= 5  (fib-fn 5))
  (assert-= 8  (fib-fn 6))
  (assert-= 13 (fib-fn 7)))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))
(test-fib fib)

(define (fib2 n)
  (define (iter a b count)
    (if (= count 0)
	b
	(iter (+ a b) a (- count 1))))
  (iter 1 0 n))
(test-fib fib2)

(define (count-change amount coins)
  (cond ((= amount 0)  1)
	((null? coins) 0)
	((< amount 0)  0)
	(else (+ (count-change amount (cdr coins))
		 (count-change (- amount (car coins)) coins)))))
(assert-= 292 (count-change 100 '(1 5 10 25 50)))

; 5 + 5
; 5 + 1 + ... + 1
; 1 + ... + 1
(define cc count-change) ; Shorter name.
(assert-= 3 (cc 10 '(5 1)))
(assert-= 3 (+ (cc 10 '(1)) (cc 5 '(5 1))))
(assert-= 3 (+ (+ (cc 10 '()) (cc 9 '(1))) (cc 5 '(5 1))))
(assert-= 3 (+ (+ 0 (cc 9 '(1))) (cc 5 '(5 1))))
(assert-= 3 (+ (+ 0 (+ (cc 9 '()) (cc 8 '(1)))) (cc 5 '(5 1))))
(assert-= 3 (+ (+ 0 (+ 0 (cc 8 '(1)))) (cc 5 '(5 1))))
; ... Not keeping tack of number 0 terms here...
(assert-= 3 (+ (+ 0 (+ 0 (cc 0 '(1)))) (cc 5 '(5 1))))
(assert-= 3 (+ (+ 0 (+ 0 1)) (cc 5 '(5 1))))
(assert-= 3 (+ 1 (cc 5 '(5 1))))
; ...
(assert-= 3 (+ 1 (+ 1 (cc 0 '(5 1)))))
(assert-= 3 (+ 1 (+ 1 1)))

; TODO Challenge: More efficient count-change

; Exercise 1.11

(define (check-f f-fn)
  (assert-=  0 (f-fn 0))
  (assert-=  1 (f-fn 1))
  (assert-=  2 (f-fn 2))
  (assert-=  4 (f-fn 3))
  (assert-= 11 (f-fn 4))
  (assert-= 25 (f-fn 5))
  (assert-= 59 (f-fn 6)))

(define (f n)
  (if (< n 3)
      n
      (+ (* 1 (f (- n 1)))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))
(check-f f)

(define (f-iter n)
  (define (iter a b c count)
    (if (= count 0)
	c
	(iter (+ a b b c c c) a b (- count 1))))
  (iter 2 1 0 n))
(check-f f-iter)

; Exercise 1.12

(define (pascal row column)
  (cond ((= column 0)   1) ; Left-most column.
	((= column row) 1) ; Right-most column.
	(else (+ (pascal (- row 1) (- column 1))
		 (pascal (- row 1) column)))))

(assert-= 1 (pascal 0 0))

(assert-= 1 (pascal 1 0))
(assert-= 1 (pascal 1 1))

(assert-= 1 (pascal 2 0))
(assert-= 2 (pascal 2 1))
(assert-= 1 (pascal 2 2))

(assert-= 1 (pascal 3 0))
(assert-= 3 (pascal 3 1))
(assert-= 3 (pascal 3 2))
(assert-= 1 (pascal 3 3))

(assert-= 1 (pascal 4 0))
(assert-= 4 (pascal 4 1))
(assert-= 6 (pascal 4 2))
(assert-= 4 (pascal 4 3))
(assert-= 1 (pascal 4 4))

; Exercise 1.13 TODO
; Exercise 1.14 TODO
; Exercise 1.15 TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1.2.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-expt expt-fn)
  (assert-= 1 (expt-fn 0 0))
  (assert-= 0 (expt-fn 0 1))
  (assert-= 0 (expt-fn 0 2))
  (assert-= 0 (expt-fn 0 3))

  (assert-= 1 (expt-fn 1 0))
  (assert-= 1 (expt-fn 1 1))
  (assert-= 1 (expt-fn 1 2))
  (assert-= 1 (expt-fn 1 3))

  (assert-= 1 (expt-fn 2 0))
  (assert-= 2 (expt-fn 2 1))
  (assert-= 4 (expt-fn 2 2))
  (assert-= 8 (expt-fn 2 3)))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
(check-expt expt)

(define (expt-iter b n)
  (define (iter counter product)
    (if (= counter 0)
	product
	(iter (- counter 1) (* b product))))
  (iter n 1))
(check-expt expt-iter)

(define (fast-expt b n)
  (cond ((= n 0)   1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else      (* b (fast-expt b (- n 1))))))
(check-expt fast-expt)

; Exercise 1.16.

(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0)   a)
	  ((even? n) (iter (square b) (/ n 2) a))
	  (else      (iter b (- n 1) (* a b)))))
  (iter b n 1))
(check-expt fast-expt-iter)

; Exercise 1.17.

(define (check-mul mul-fn)
  (assert-=  0 (mul-fn 0 0))
  (assert-=  0 (mul-fn 0 1))
  (assert-=  0 (mul-fn 0 2))
  (assert-=  0 (mul-fn 0 5))

  (assert-=  0 (mul-fn 1 0))
  (assert-=  1 (mul-fn 1 1))
  (assert-=  2 (mul-fn 1 2))
  (assert-=  5 (mul-fn 1 5))

  (assert-=  0 (mul-fn 2 0))
  (assert-=  2 (mul-fn 2 1))
  (assert-=  4 (mul-fn 2 2))
  (assert-= 10 (mul-fn 2 5))

  (assert-=  0 (mul-fn 5 0))
  (assert-=  5 (mul-fn 5 1))
  (assert-= 10 (mul-fn 5 2))
  (assert-= 25 (mul-fn 5 5)))
(check-mul *)

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))
(check-mul mul)

(define (double n)
  (* n 2))
(define (half n)
  (if (even? n)
      (/ n 2)
      (error "Not even: " n)))
(define (fast-mul a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-mul a (half b))))
	(else (+ a (fast-mul a (- b 1))))))
(check-mul fast-mul)

(define (fast-mul-iter a b)
  (define (iter a b result)
    (cond ((= b 0)   result)
	  ((even? b) (iter (double a) (half b) result))
	  (else      (iter a (- b 1) (+ a result)))))
  (iter a b 0))
(check-mul fast-mul-iter)

