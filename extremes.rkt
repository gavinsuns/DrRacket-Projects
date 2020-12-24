;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extremes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 02, Problem 2
;; **********************************************
;;

;; (smallest num) Produces the smallest number in the non-empty list num.
;; Examples:
(check-expect (smallest (cons 5 empty)) 5)
(check-expect (smallest (cons 3 (cons 8 empty))) 3)

;; smallest: (ne-listof Num) -> Num
(define (smallest num)
  (cond [(empty? (rest num)) (first num)]
        [else (min (first num) (smallest(rest num)))])
)

;; Tests:
(check-expect (smallest (cons -5 (cons 2 (cons 10.5 empty)))) -5)
(check-expect (smallest (cons 0 (cons 2 (cons 34 empty)))) 0)


;; (largest num) Produces the largest number in the non-empty list num.
;; Examples:
(check-expect (largest (cons 5 empty)) 5)
(check-expect (largest (cons 3 (cons 8 empty))) 8)

;; largest: (ne-listof Num) -> Num
(define (largest num)
  (cond [(empty? (rest num)) (first num)]
        [else (max (first num) (largest (rest num)))])
)

;; Tests:
(check-expect (largest (cons -5 (cons 2 (cons 10.5 empty)))) 10.5)
(check-expect (largest (cons 0 (cons 2 (cons 34 empty)))) 34)


;; (max-diff num) Produce the largest differece between any two elements in the list.
;; Examples:
(check-expect (max-diff (cons 5 empty)) 0)
(check-expect (max-diff (cons 3 (cons 8 empty))) 5)

;; max-diff: (ne-listof Num) -> Num
(define (max-diff num)
  (- (largest num) (smallest num)))

;; Tests:
(check-expect (max-diff (cons -5 (cons 2 (cons 10.5 empty)))) 15.5)
(check-expect (max-diff (cons 0 (cons 2 (cons 34 empty)))) 34)