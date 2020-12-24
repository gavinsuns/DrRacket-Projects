;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 01, Problem 3
;; ***************************************************
;;

;; (sequence-type a b c d) Produces the type of sequence
;;  for the sequence a, b, c, d.
;; Examples:
(check-expect (sequence-type 5 10 15 20) 'arithmetic)
(check-expect (sequence-type 5 10 20 40) 'geometric)

;; sequence-type: Num Num Num Num -> Sym
(define (sequence-type a b c d)
  (cond [(and (arithmetic? a b c d) (geometric? a b c d)) 'both]
        [(arithmetic? a b c d) 'arithmetic]
        [(geometric? a b c d) 'geometric]
        [else 'neither]))

;; Tests:
(check-expect (sequence-type 2 3 2 0) 'neither)
(check-expect (sequence-type 1 1 1 1) 'both)
(check-expect (sequence-type 0 0 0 0) 'arithmetic)
(check-expect (sequence-type 0 2 4 6) 'arithmetic)


;; (arithmetic? a b c d) Determines if the sequence a, b, c, d
;;  is arithmetic.
;; Example:
(check-expect (arithmetic? 0 1 2 3) true)

;; arithmetic?: Num Num Num Num -> Bool 
(define (arithmetic? a b c d) (= (- b a) (- c b) (- d c)))


;; (geometric? a b c d) Determines if the sequence a, b, c, d
;;  is geometric.
;; Example:
(check-expect (geometric? 2 4 8 16) true)

;; geometric?: Num Num Num Num -> Bool
(define (geometric? a b c d)
  (cond [(or (= a 0) (= b 0) (= c 0) (= d 0)) false]
        [else (= (/ b a) (/ c b) (/ d c))]))