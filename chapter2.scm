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
      (error "Not =:" a b)))

(define (assert-eq? a b)
  (if (eq? a b)
      #t
      (error "Not eq?:" a b)))

(define (assert-equal-rat a b)
  (if (equal-rat? a b)
      #t
      (error "Not equal-rat:" a b)))

; Section 2.1.1

(define (check-rat)
  (define one-half  (make-rat 1 2))
  (define one-third (make-rat 1 3))
  (assert-equal-rat (make-rat  5 6) (add-rat one-half one-third))
  (assert-equal-rat (make-rat  1 6) (sub-rat one-half one-third))
  (assert-equal-rat (make-rat -1 6) (sub-rat one-third one-half))
  (assert-equal-rat (make-rat  1 6) (mul-rat one-half one-third))
  (assert-equal-rat (make-rat  3 2) (div-rat one-half one-third))
  (assert-equal-rat (make-rat  2 3) (div-rat one-third one-half))
  (assert-equal-rat (make-rat  2 3) (add-rat one-third one-third))
  (assert-equal-rat (make-rat  2 3) (make-rat  4  6))
  (assert-equal-rat (make-rat  2 3) (make-rat -4 -6)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(check-rat)

; Old implementation: Does not normalize.
(assert-= 4 (numer (make-rat 4 6)))
(assert-= 6 (denom (make-rat 4 6)))

(define (gcd a b) ; From section 1.2.5.
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(check-rat)

; New implementation: Does normalize.
(assert-= 2 (numer (make-rat 4 6)))
(assert-= 3 (denom (make-rat 4 6)))

; Exercise 2.1

; The above implemenation already satisfies this requirement, due to how
; gcd from 1.2.5 works. TODO: Am I missing something?
(assert-=  2 (gcd -4  6))
(assert-= -2 (gcd  4 -6))
(assert-= -2 (gcd -4 -6))

(assert-= -2 (numer (make-rat -4  6)))
(assert-=  3 (denom (make-rat -4  6)))

(assert-= -2 (numer (make-rat  4 -6)))
(assert-=  3 (denom (make-rat  4 -6)))

(assert-=  2 (numer (make-rat -4 -6)))
(assert-=  3 (denom (make-rat -4 -6)))

; Exercise 2.2

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define p34 (make-point 3 4))
(assert-= 3 (x-point p34))
(assert-= 4 (y-point p34))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define p57 (make-point 5 7))
(define s3457 (make-segment p34 p57))
(assert-= 3 (x-point (start-segment s3457)))
(assert-= 4 (y-point (start-segment s3457)))
(assert-= 5 (x-point (end-segment s3457)))
(assert-= 7 (y-point (end-segment s3457)))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
	(end (end-segment segment)))
    (make-point (/ (+ (x-point start) (x-point end)) 2.0)
		(/ (+ (y-point start) (y-point end)) 2.0))))
(assert-= 4.0 (x-point (midpoint-segment s3457)))
(assert-= 5.5 (y-point (midpoint-segment s3457)))

; Exercise 2.3

(define (area-rect rect)
  (* (width-rect  rect)
     (height-rect rect)))
(define (peri-rect rect)
  (* 2 (+ (width-rect rect)
	  (height-rect rect))))

; Represent rectangle as a point (far corner):

(define (make-rect p) p)
(define (width-rect r)  (x-point r))
(define (height-rect r) (y-point r))

(assert-= 12 (area-rect (make-rect (make-point 3 4))))
(assert-= 14 (peri-rect (make-rect (make-point 3 4))))

; Represent rectangle as two points (diagonal corners):

(define (make-rect p1 p2) (cons p1 p2))
(define (width-rect r) (abs (- (x-point (car r))
			       (x-point (cdr r)))))
(define (height-rect r) (abs (- (y-point (car r))
				(y-point (cdr r)))))

(assert-= 12 (area-rect (make-rect (make-point 1 2) (make-point 4 6))))
(assert-= 14 (peri-rect (make-rect (make-point 1 2) (make-point 4 6))))

; Section 2.1.3

(define (my-cons x y)
  (lambda (m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Invalid m:")))))
(define (my-car p) (p 0))
(define (my-cdr p) (p 1))

(assert-= 5 (my-car (my-cons 5 7)))
(assert-= 7 (my-cdr (my-cons 5 7)))

; Exercise 2.4

(define (my-cons x y) (lambda (m) (m x y)))
(define (my-car z) (z (lambda (x y) x)))
(define (my-cdr z) (z (lambda (x y) y)))

(assert-= 5 (my-car (my-cons 5 7)))
(assert-= 7 (my-cdr (my-cons 5 7)))

; Exercise 2.5

(define (my-cons a b) (* (expt 2 a)
			 (expt 3 b)))
(define (count-factors x b)
  (cond ((= (remainder x b) 0) (+ 1 (count-factors (/ x b) b)))
	(else 0)))
(define (my-car z) (count-factors z 2))
(define (my-cdr z) (count-factors z 3))
(assert-= 4 (my-car (my-cons 4 5)))
(assert-= 5 (my-cdr (my-cons 4 5)))

; Exercise 2.6

; Church numerals are represented as functions that take two arguments:
; A unary function f and a value x.
; The number n is represented by the function that applies f n times to x.
(define zero-cn
  (lambda (f) (lambda (x) x)))
(define (add-1-cn n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; A simple way to convert them to numbers is by using f = succ and x = 0:
(define (cn->number n)
  (define (succ x) (+ x 1))
  ((n succ) 0))
(assert-= 0 (cn->number zero-cn))
(assert-= 1 (cn->number (add-1 zero-cn)))
(assert-= 2 (cn->number (add-1 (add-1 zero-cn))))

(define one-cn
  (lambda (f) (lambda (x) (f x))))
(assert-= 1 (cn->number one-cn))

(define two-cn
  (lambda (f) (lambda (x) (f (f x)))))
(assert-= 2 (cn->number two-cn))

(define (plus-cn n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
(assert-= 0 (cn->number (plus-cn zero-cn zero-cn)))
(assert-= 1 (cn->number (plus-cn zero-cn one-cn)))
(assert-= 1 (cn->number (plus-cn one-cn zero-cn)))
(assert-= 4 (cn->number (plus-cn two-cn two-cn)))

