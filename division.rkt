;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname division) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 03, Problem 5
;; **********************************************
;;

;; (divide a b) Produces the quotient and remainder of a/b.
;; Examples:
(check-expect (divide 17 5) (cons 3 (cons 2 empty)))
(check-expect (divide 40 7) (cons 5 (cons 5 empty)))

;; divide: Nat Nat => (ne-listof Nat)
;; Requires:
;; b > 0
(define (divide a b) (cons (quot a b)
                           (cons (- a (* (quot a b) b)) empty)))

;; Tests:
(check-expect (divide 37 2) (cons 18 (cons 1 empty)))
(check-expect (divide 50 13) (cons 3 (cons 11 empty)))


;; (quot a b) Produces the quotient of a/b.
;; Example:
(check-expect (quot 40 7) 5)

;; quot: Nat Nat => Nat
;; Requires:
;; b > 0
(define (quot a b) (cond [(< a b) 0]
                             [else (+ 1 (quot (- a b) b))]))