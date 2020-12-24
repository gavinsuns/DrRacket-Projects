;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname translations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 01, Problem 2
;; ***************************************************
;;

;; (volume r) Produce the volume of a sphere with radius r.
;; Examples:
(check-within (volume 5) (* 500/3 pi) 0.01)
(check-within (volume 1) (* 4/3 pi) 0.01)

;; volume: Num -> Num
;; Requires:
;;    r >= 0
(define (volume r) (* 4/3 pi (expt r 3)))

;; Tests
(check-within (volume 0) 0 0.01)
(check-within (volume 2.5) (* 125/6 pi) 0.01) 


(define phi (/ (+ 1 (sqrt 5)) 2)) ; The mathematical constant φ.


;; (fib n) Produces the nth Fibonacci number.
;; Examples:
(check-within (fib 3) 2 0.01)
(check-within (fib 4) 3 0.01)

;; fib: Nat -> Num
(define (fib n)
  (/ (- (expt phi n) (expt (* -1 phi) (* -1 n)))
     (- (* 2 phi) 1)))

;; Tests:
(check-within (fib 1) 1 0.01)
(check-within (fib 2) 1 0.01)
(check-within (fib 10) 55 0.01)
               
               
               

