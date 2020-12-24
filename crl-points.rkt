;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname crl-points) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 01, Problem 4
;; ***************************************************
;;

;; (crl-points a b c p) Produces the players total points based on the number of first a,
;;  the number of second b, the number of third c, and the penalty status p.
;; Examples:
(check-expect (crl-points 1 1 1 'good-standing) 80)
(check-expect (crl-points 2 1 0 'minor-violation) 114)

;; crl-points: Int Int Int Sym -> Int
;; Requires:
;;    a >= 0
;;    b >= 0
;;    c >= 0
(define (crl-points a b c p) (cond [(symbol=? p 'disqualified) 0]
                                   [(symbol=? p 'minor-violation)
                                    (floor (* (+(points-prior a b c)
                                         (consistency-points a b c)) 0.95))]
                                   [(symbol=? p 'major-violation)
                                    (floor (* (+(points-prior a b c)
                                         (consistency-points a b c)) 0.75))]
                                   [else (+(points-prior a b c)
                                         (consistency-points a b c))]))

;; Tests:
(check-expect (crl-points 2 3 4 'good-standing) 215)
(check-expect (crl-points 3 2 2 'minor-violation) 213)
(check-expect (crl-points 4 2 2 'major-violation) 206)
(check-expect (crl-points 3 10 4 'disqualified) 0)


;; (points-prior a b c) Produces the players total points based on the number of
;  first a, the number of second b, the number of third c, without accounting the penalty.
;; Example:
(check-expect (points-prior 2 2 2) 160)

;; points-prior: Int Int Int -> Int
;; Requires:
;;    a >= 0
;;    b >= 0
;;    c >= 0
(define (points-prior a b c) (+ (*  a 50) (* b 20) (* c 10)))


;; (consistency-points a b c) Produces the amount of bonus points the player receives
;;  depending on their total amount of top 3 results a + b + c.
;; Example:
(check-expect (consistency-points 1 2 1) 0)
(check-expect (consistency-points 1 3 4) 15)

;; consistency-points: Int Int Int -> Int
;; Requires:
;;    a >= 0
;;    b >= 0
;;    c >= 0
(define (consistency-points a b c) (cond [(>= (+ a b c) 5) 15]
                                         [else 0]))