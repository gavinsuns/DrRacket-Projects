;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 03, Problem 3
;; **********************************************
;;

;; (differneces x) Produces a list of the differences between each number and the number before it in list x.
;; Examples:
(check-expect
(differences (cons 4 (cons 7 (cons 1 empty))))
(cons 3 (cons -6 empty)))
(check-expect
(differences (cons 4 (cons 5 (cons 2 empty))))
(cons 1 (cons -3 empty)))

;; differences: (ne-listof Num) => (listof Num)
(define (differences x) (cond [(empty? (rest x)) empty]
                              [else (cons (- (first (rest x)) (first x)) (differences (rest x)))]))

;; Tests:
(check-expect
 (differences (cons 1 empty)) empty)
(check-expect
(differences (cons 5 (cons 4 (cons 6 (cons 1 empty)))))
(cons -1 (cons 2 (cons -5 empty))))