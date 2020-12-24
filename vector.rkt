;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 04, Problem 2
;; **********************************************
;;

;; (euclidean-norm v) Produces the square root of the sum of squares of the elements in vector v.
;; Examples:
(check-within (euclidean-norm (list 3 4)) 5 0.01)
(check-within (euclidean-norm (list 6 8)) 10 0.01)

;; euclidean-norm: (listof Num) -> Num
(define (euclidean-norm v) (sqrt (euclidean-norm-helper v)))

;; Tests:
(check-within (euclidean-norm empty) 0 0.01)
(check-within (euclidean-norm (list -3)) 3 0.01)
(check-within (euclidean-norm (list -3 -4)) 5 0.01)
(check-within (euclidean-norm (list -3 4)) 5 0.01)
(check-within (euclidean-norm (list 0 4 2)) 4.47 0.01)
(check-within (euclidean-norm (list 10 11 13)) 19.75 0.01)


;; (euclidean-norm-helper v) Produces the sum of squares of the elements in vector v.
;; Example:
(check-within (euclidean-norm-helper (list 2 4)) 20 0.01)

;; euclidean-norm-helper: (listof Num) -> Num
(define (euclidean-norm-helper v) (cond [(empty? v) 0]
                                   [else (+ (sqr (first v)) (euclidean-norm-helper (rest v)))]))


;; (unit-vector v) Produces a list of each element of vector v divided by the Euclidean Norm.
;; Examples:
(check-within (unit-vector (list 3 4)) (list 0.6 0.8) 0.01)
(check-within (unit-vector (list 6 8)) (list 0.6 0.8) 0.01)

;; unit-vector: (ne-listof Num) -> (ne-listof Num)
(define (unit-vector v) (unit-vector-recurse v (euclidean-norm v)))

;; Tests:
(check-within (unit-vector (list -3)) (list -1) 0.01)
(check-within (unit-vector (list -3 -4)) (list -0.6 -0.8) 0.01)
(check-within (unit-vector (list -3 4)) (list -0.6 0.8) 0.01)
(check-within (unit-vector (list 0 4 2)) (list 0 0.89 0.45) 0.01)
(check-within (unit-vector (list 10 11 13)) (list 0.51 0.56 0.66) 0.01)


;; (unit-vector-recurse v norm) Produces a list of each element of vector v divided by norm.
;; Examples and tests: see wrapper function unit-vector

;; unit-vector-recurse: (ne-listof Num) Num -> (ne-listof Num)
;; Requires:
;; norm cannot be zero
(define (unit-vector-recurse v norm) (cond [(empty? (rest v)) (cons (/ (first v) norm) empty)]
                                         [else (cons (/ (first v) norm)
                                                     (unit-vector-recurse (rest v) norm))]))


;; (cos-between a b) Produces the cosine of the angle between vector a and vector b.
;; Examples:
(check-within (cos-between (list 3 4) (list 0 6)) 0.8 0.01)
(check-within (cos-between (list 3) (list 6)) 1 0.01)

;; cos-between: (ne-listof Num) (ne-listof Num) -> Num
;; Requires: a and b are the same length and non-zero vectors
(define (cos-between a b) (dot-product (unit-vector a) (unit-vector b)))

;; Tests:
(check-within (cos-between (list -3) (list -1)) 1 0.01)
(check-within (cos-between (list -3 -4) (list -5 -6)) 1 0.01)
(check-within (cos-between (list -3 4) (list 0 -5)) -0.8 0.01)
(check-within (cos-between (list 0 4 2) (list 1 2 3)) 0.84 0.01)
(check-within (cos-between (list 10 11 13) (list 3 2 5)) 0.96 0.01)


;; (dot-product a b) Produces the dot product of a and b.
;; Examples and tests: see wrapper function cos-between

;; dot-product: (ne-listof Num) (ne-listof Num) -> Num
;; Requires: a and b are the same length
(define (dot-product a b) (cond [(empty? (rest a)) (* (first a) (first b))]
                                [else (+ (* (first a) (first b))
                                         (dot-product (rest a) (rest b)))]))