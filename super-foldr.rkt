;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super-foldr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 08, Problem 5
;; **********************************************
;;

;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))

;; 5a)
;; (super-foldr f base lst) Produces f applied to lst with base case base.
;; Examples:
(check-expect (super-foldr + 0 '(1 (5 5 (1 3)) (10 2) 2)) 29)
(check-expect (super-foldr - 0 '(1 (5 5 (1 3)) (10 2) 2)) 9)

;; super-foldr: (X Y -> Y) Y (nested-listof X) -> Y
(define (super-foldr f base lst)
  (cond [(empty? lst) base]
        [(list? (first lst))
         (f (super-foldr f base (first lst))
            (super-foldr f base (rest lst)))]
        [else (f (first lst) (super-foldr f base (rest lst)))]))

;; Tests:
(check-expect (super-foldr append empty (list empty empty)) empty)
(check-expect (super-foldr string-append "apple" empty) "apple")
(check-expect (super-foldr * 1 '(4 5 6)) 120)
(check-expect (super-foldr cons empty '(1 (5 5 (1 3)) (10 2) 2))
              '(1 (5 5 (1 3)) (10 2) 2))


;; 5b)
;; (magnitudes nl) Produces the sum of all the absolute values of the elements
;; in nl.
;; Examples:
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)
(check-expect (magnitudes '(1 (-5 10 4) (-10 2) 2)) 34)

;; magnitudes: (nested-listof Num) -> Num
(define (magnitudes nl)
  (super-foldr (lambda (x rorr) (cond [(< x 0) (- rorr x)]
                                      [else (+ rorr x)])) 0 nl))

;; Tests:
(check-expect (magnitudes empty) 0)
(check-expect (magnitudes '(0 (0 0 (0 0)) (0 0) 0)) 0)
(check-expect (magnitudes '(3 2 1 4 5 6)) 21)
(check-expect (magnitudes '(-3 -2 -1 -4 -5 -6)) 21)


;; 5c)
;; (super-filter pred? lst) keeps all the items in lst that satisfy pred?.
;; Examples:
(check-expect
 (super-filter odd?
               (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect
 (super-filter even?
               (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred? lst)
  (super-foldr (lambda (x rorr) (cond [(list? x) (cons x rorr)]
                                      [(pred? x) (cons x rorr)]
                                 [else rorr])) empty lst))

;; Tests:
(check-expect (super-filter symbol? empty) empty)
(check-expect
 (super-filter number?
               (list 5 (list 4 "hello" "bye" (list 10) "hi") 'huge 0.99))
 (list 5 (list 4 (list 10)) 0.99))
(check-expect
 (super-filter odd?
               (list 1 (list (list 3) 5 (list 7 9)) 11))
 (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect
 (super-filter even?
               (list 1 (list (list 3) 5 (list 7 9)) 11))
 (list (list (list) (list))))