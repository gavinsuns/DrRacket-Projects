;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 02, Problem 3
;; **********************************************
;;

;; (cs135-grade self-check assign mt1 mt2 final) Produces the final grade received in CS135.
;; Examples:
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0.95) 68.3)
(check-expect (cs135-grade 0.8 0.6 0.4 0.9 0) 46)

;; cs135-grade: Num Num Num Num Num => Num
;; Requires:
;; self-check <= 0
;; assign <= 0
;; mt1 <= 0
;; mt2 <= 0
;; final <= 0
(define (cs135-grade self-check assign mt1 mt2 final)
  (cond [(and (> final 0.5) (> assign 0.5))
         (grade self-check assign mt1 mt2 final)]
        [(> (grade self-check assign mt1 mt2 final) 46) 46]
        [else (grade self-check assign mt1 mt2 final)]))

;; Tests:
(check-expect (cs135-grade 0.9 0.6 0.7 1 0) 46)
(check-expect (cs135-grade 0.5 0.7 0.8 0.9 0.6) 68.5)
(check-expect (cs135-grade 0.2 0.3 0.4 0.5 0.1) 27.9)


;; (grade self-check assign mt1 mt2 final) Produces the total grade.
;; Examples:
(check-expect (grade 0.8 0.6 0.4 0.9 0.95) 68.3)

;; grade: Num Num Num Num Num => Num
;; Requires:
;; self-check <= 0
;; assign <= 0
;; mt1 <= 0
;; mt2 <= 0
;; final <= 0
(define (grade self-check assign mt1 mt2 final)
  (+ (* self-check 10) (* assign 60) (* mt1 7) (* mt2 7) (* final 16)))