;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 01, Problem 4
;; **********************************************
;;

(define prices1 (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))))
(define prices2 (cons 4 (cons 20 (cons 45 (cons 55 empty)))))

;; (change-due costs paid) Produces the amount of change the customer should receive back.
;; Examples:
(check-expect (change-due prices1 100) 6.07)
(check-expect (change-due prices1 93.93) 0)

;; change-due: (ne-listof Num) Num => Num
;; Requires:
;; costs >= 0
;; paid >= 0
(define (change-due costs paid)
  (- paid (total-costs costs)))

;; Tests:
(check-expect (change-due prices2 124) 0)
(check-expect (change-due prices2 130) 6)


;; (total-costs costs) Produces the total cost of stuff.
;; Examples:
(check-expect (total-costs prices2) 124)

;; total-costs: (ne-listof Num) => Num
;; Requires:
;; costs >= 0
(define (total-costs costs)
  (cond
  [(empty? (rest costs)) (first costs)]
  [else (+ (first costs) (total-costs (rest costs)))]))


;; (paid-enough? costs paid) Determines whether or not the customer paid enough.
;; Examples:
(check-expect (paid-enough? prices1 100) true)
(check-expect (paid-enough? prices2 100) false)

;; paid-enough?: (ne-listof Num) Num => Num
;; Requires:
;; costs >= 0
;; paid >= 0
(define (paid-enough? costs paid) (>= paid (total-costs costs)))

;; Tests:
(check-expect (paid-enough? prices1 93.93) true)
(check-expect (paid-enough? prices2 123.99) false)


;; (free-item costs paid) Produces the first item in the list that is large enough
;; that if reduced to 0 would mean the customer has enough to pay for their bill.
;; Examples:
(check-expect (free-item prices1 90) 23.30)
(check-expect (free-item prices2 120) 4)

;; free-item: (ne-listof Num) Num => Num
;; Requires:
;; costs >= 0
;; paid >= 0
(define (free-item costs paid)
  (free-item2 costs paid (total-costs costs)))

;; Tests:
(check-expect (free-item prices1 60) 59.99)
(check-expect (free-item prices2 123) 4)


;; (free-item2 costs paid x) Produces the first item in the list costs that is greater
;; than the difference between the total elements in list costs minus paid.
;; Examples and tests: see wrapper function free-item

;; free-item2: (ne-listof Num) Num Num => Num
(define (free-item2 costs paid x)
  (cond
    [(>= (first costs) (- x paid)) (first costs)]
    [else (free-item2 (rest costs) paid x)]))