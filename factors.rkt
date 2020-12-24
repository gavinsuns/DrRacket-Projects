;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname factors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 03, Problem 6
;; **********************************************
;;

;; (all-factors n) Produces all factors of n in a list in ascending order.
;; Examples:
(check-expect
(all-factors 30)
(cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15
empty))))))))
(check-expect
(all-factors 6)
(cons 1 (cons 2 (cons 3 empty))))

;; all-factors: Nat => (ne-listof Nat)
;; Requires:
;; n > 0
(define (all-factors n) (all-factors2 n 1))

;; Tests:
(check-expect (all-factors 23) (cons 1 empty))
(check-expect (all-factors 1) empty)


;; (all-factors2 n m) Produces all factors of n, that are greater than or
;; equal to m in a list in ascending order.
;; Examples and tests: see wrapper function all-factors

;; all-factors2: Nat Nat => (ne-listof Nat)
;; Requires:
;; n > 0
;; m > 0
(define (all-factors2 n m) (cond [(= m n) empty]
                                 [(and (>= (/ n 2) m) (integer? (/ n m)))
                                  (cons m (all-factors2 n (add1 m)))]
                                 [else (all-factors2 n (add1 m))]))


;; (is-prime? n) Determines whether or not n is a prime number.
;; Examples:
(check-expect (is-prime? 20) false)
(check-expect (is-prime? 3) true)

;; is-prime? Nat => Bool
(define (is-prime? n) (cond[(= n 0) false]
                           [(equal? (all-factors n) (cons 1 empty)) true]
                           [else false]))

;; Tests:
(check-expect (is-prime? 1) false)
(check-expect (is-prime? 0) false)
(check-expect (is-prime? 7) true)

;; (is-composite? n) Determines whether or not n is a composite number.
(check-expect (is-composite? 20) true)
(check-expect (is-composite? 3) false)

;; is-composite? Nat => Bool
(define (is-composite? n) (cond [(or (= n 1) (= n 0)) false]
                                [else (not (is-prime? n))]))

;; Tests:
(check-expect (is-composite? 1) false)
(check-expect (is-composite? 0) false)
(check-expect (is-composite? 23) false)
(check-expect (is-composite? 63) true)