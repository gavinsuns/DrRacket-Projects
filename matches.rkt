;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matches) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 08, Problem 4
;; **********************************************
;;

;; (matches-func? f list) Determines if each pair in list is of the form
;; (x (f x)), which is the second value in the pair is the result of applying
;; the provided function to the first value in the pair.
;; Examples:
(check-expect (matches-func? sqr '((5 25) (2 4) (10 100))) true)
(check-expect (matches-func? add1 '((1 2) (3 5) (10 15))) false)

;; matches-func?: (X -> Y) (listof (list X Any)) -> Bool
(define (matches-func? f list)
  (foldl (lambda (x rorr) (and (equal? (f (first x)) (second x)) rorr))
         true list))

;; Tests:
(check-expect (matches-func? sqr empty) true)
(check-expect (matches-func? number? '((5 #true) ("hello" #false))) true)
(check-expect (matches-func? sub1 '((5 "hello") (2 "hi") (10 9))) false)
(check-expect (matches-func? (lambda (x) x) '((1 1) ("a" "a") (grr grr))) true)
(check-expect (matches-func? sub1 '((5 4) (2 1) (10 9))) true)