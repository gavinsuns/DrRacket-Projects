;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname parity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 02, Problem 5
;; **********************************************
;;

;; (parity x) Produces the parity of a binary string.
;; Examples:
(check-expect (parity "10101001") 'even)
(check-expect (parity "10000000") 'odd)

;; parity: Str => Sym
(define (parity x) (cond [(empty? (string->list x)) 'even]
                         [(even? (num-of-1 (string->list x))) 'even]
                         [(odd? (num-of-1 (string->list x))) 'odd]))

; Tests:
(check-expect (parity "") 'even)
(check-expect (parity "101010110") 'odd)


;; (num-of-1 x) Produces the number of ones in a list.
;; Example:
(check-expect (num-of-1 (cons #\1 (cons #\0 empty))) 1)

;; num-of-1: (ne-listof Char) => Nat
(define (num-of-1 x)
  (cond [(and (empty? (rest x)) (char=? (first x) #\1)) 1]
        [(and (empty? (rest x)) (char=? (first x) #\0)) 0]
        [(char=? (first x) #\1) (+ 1 (num-of-1 (rest x)))]
        [else (num-of-1 (rest x))]
  )
)