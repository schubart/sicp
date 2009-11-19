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

(define (assert-equal-rat a b)
  (if (equal-rat? a b)
      #t
      (error "Not equal:" a b)))

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
(assert-= 4 (x-point p34))

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

