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
      (error "Not eq:" a b)))

(define (assert-equal? a b)
  (if (equal? a b)
      #t
      (error "Not equal:" a b)))

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
(assert-= 1 (cn->number (add-1-cn zero-cn)))
(assert-= 2 (cn->number (add-1-cn (add-1-cn zero-cn)))   )

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

; Section 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

; Exercise 2.7

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(assert-= 1 (lower-bound (make-interval 1 2)))
(assert-= 2 (upper-bound (make-interval 1 2)))

(assert-= 6 (lower-bound (add-interval (make-interval 1 2)
				       (make-interval 5 7))))
(assert-= 9 (upper-bound (add-interval (make-interval 1 2)
				       (make-interval 5 7))))

; TODO Exercise 2.8 to 2.16

; Section 2.2.1

(assert-equal? '(1 2 3 4) (cons 1 (cons 2 (cons 3 (cons 4 '())))))

(define one-through-four (list 1 2 3 4))
(assert-= 1 (car one-through-four))
(assert-equal? '(2 3 4) (cdr one-through-four))
(assert-= 2 (car (cdr one-through-four)))
(assert-equal? `(10 1 2 3 4) (cons 10 one-through-four))
(assert-equal? `(5 1 2 3 4) (cons 5 one-through-four))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares '(1 4 9 16 25))
(assert-= 16 (list-ref squares 3))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds '(1 3 5 7))
(assert-= 4 (length odds))

(define (length items)
  (define (iter a count)
    (if (null? a)
	count
	(iter (cdr a) (+ count 1))))
  (iter items 0))
(assert-= 4 (length odds))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(assert-equal? '(1 4 9 16 25 1 3 5 7) (append squares odds))
(assert-equal? '(1 3 5 7 1 4 9 16 25) (append odds squares))

; Exercise 2.17

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(assert-equal? '(34) (last-pair '(23 72 149 34)))

; Exercise 2.18

(define (reverse items)
  (if (null? items)
      '()
      (append (reverse (cdr items))
	      (list (car items)))))
(assert-equal? '(7 5 3 1) (reverse odds))

; Exercise 2.19

; My implementation of section 1.2.2 already uses lists of denominations.
; first-denomination:        car
; except-first-denomination: cdr
; no-more?:                  null?
;
; Order does not matter. TODO: Why?

; Exercise 2.20

(define (myfilter predicate items)
  (cond ((null? items)
         '())
        ((predicate (car items))
         (cons (car items) (myfilter predicate (cdr items))))
        (else
         (myfilter predicate (cdr items)))))
(assert-equal? '(1 2 3) (myfilter (lambda (x) (< x 4)) '(1 2 3 4 5 6)))

(define (same-parity first . rest)
  (myfilter (lambda (x) (eq? (even? x) (even? first)))
            (cons first rest)))
(assert-equal? '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))
(assert-equal? '(2 4 6) (same-parity 2 3 4 5 6 7))

; Section 2.2.1 cont. (Mapping over lists)

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
(assert-equal? '(10 20 30 40 50) (scale-list '(1 2 3 4 5) 10))

(define (mymap proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (mymap proc (cdr items)))))
(assert-equal? '(10 2.5 11.6 17) (mymap abs '(-10 2.5 -11.6 17)))
(assert-equal? '(1 4 9 16)
               (mymap (lambda (x) (* x x))
                      '(1 2 3 4)))

(define (scale-list items factor)
  (mymap (lambda (x) (* x factor))
         items))
(assert-equal? '(10 20 30 40 50) (scale-list '(1 2 3 4 5) 10))

; Exercise 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))
(assert-equal? '(1 4 9 16) (square-list '(1 2 3 4)))

(define (square-list items)
  (map square items))
(assert-equal? '(1 4 9 16) (square-list '(1 2 3 4)))

; Exercise 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))
(assert-equal? '(16 9 4 1) (square-list '(1 2 3 4)))
; This function squares things left-to-right and builds answer right-to-left.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things)))))))
; This function tries to build answer left-to-right, but in order to build a
; list, the first argument to cons should be an item, the second a list. Here,
; though, the first (answer) is a list, the second (square ...) is an item,
; so this function does not build a list as expected.

