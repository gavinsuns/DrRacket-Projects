;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 07, Problem 1
;; **********************************************
;;

;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))

;; (super-filter pred? lst) keeps all the items in lst that satisfy pred?.
;; Examples:
(check-expect
 (super-filter odd?
               (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect
 (super-filter even?
               (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred? lst)
  (cond [(empty? lst) empty]
        [(list? (first lst))
         (cons (super-filter pred? (first lst))
               (super-filter pred? (rest lst)))]
        [(pred? (first lst))
         (cons (first lst) (super-filter pred? (rest lst)))]
        [else (super-filter pred? (rest lst))]))

;; Tests:
(check-expect (super-filter symbol? empty) empty)
(check-expect
 (super-filter number?
               (list 5 (list 4 "hello" "bye" (list 10) "hi") 'huge 0.99))
 (list 5 (list 4 (list 10)) 0.99))
(check-expect
 (super-filter odd?
               (list 1 (list (list 3) 5 (list 7 9)) 11))
 (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect
 (super-filter even?
               (list 1 (list (list 3) 5 (list 7 9)) 11))
 (list (list (list) (list))))
 

;; (ruthless lst) keeps all the Sym in lst that are not 'ruth.
;; Examples:
(check-expect
 (ruthless
  (list 'rabbit
        (list 'apple 'pluto
              (list 'ruth 'blue) 'ruth) 'hello))
 (list 'rabbit
       (list 'apple 'pluto
             (list 'blue)) 'hello))
(check-expect
 (ruthless
  (list 'ruth
        (list 'ruth 'pluto
              (list 'ruth 'blue) 'ruth) 'hello))
 (list
  (list 'pluto
        (list 'blue)) 'hello))

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless lst)
  (local[(define (not-symbol-ruth? item) (not (symbol=? item 'ruth)))]
    (super-filter not-symbol-ruth? lst)))

;; Tests:
(check-expect (ruthless empty) empty)
(check-expect (ruthless
               (list
                (list 'pluto
                      (list 'blue)) 'hello))
              (list
               (list 'pluto
                     (list 'blue)) 'hello))
(check-expect (ruthless (list 'ruth)) empty)
(check-expect (ruthless (list 'ruth (list 'a 'b) 'c))
              (list (list 'a 'b) 'c))


;; (supersize n lst) keeps all the Nat in lst that are not less than n.
;; Examples:
(check-expect
 (supersize 4 (list 8 1 (list 2 6 3) 10 1))
 (list 8 (list 6) 10))
(check-expect
 (supersize 7 (list 8 1 (list 2 6 3) 10 1))
 (list 8 (list) 10))

;; supersize: Nat (nested-listof Nat) -> (nested-listof Nat)
(define (supersize n lst)
  (local [(define (not-less-than-n? num) (>= num n))]
    (super-filter not-less-than-n? lst)))

;; Tests:
(check-expect (supersize 100 empty) empty)
(check-expect (supersize 1 (list 2 3 4 (list 5 6)))
              (list 2 3 4 (list 5 6)))
(check-expect (supersize 10 (list 2 3 4 (list 5 6)))
              (list (list)))
(check-expect (supersize 4 (list 2 3 4 (list 5 6)))
              (list 4 (list 5 6)))


;; (super-keeper pred? lst) keeps all the items in lst that do no satisfy pred?.
;; Examples:
(check-expect
 (super-keeper
  odd?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list (list 2 (list 2 4) 6 (list 8)) 10 12))
(check-expect
 (super-keeper
  even?
  (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
 (list 1 (list (list 3) 5 (list 7 9)) 11))

;; super-keeper: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-keeper pred? lst)
  (local [(define (not-pred? item) (not (pred? item)))]
    (super-filter not-pred? lst)))

;; Tests:
(check-expect (super-keeper symbol? empty) empty)
(check-expect
 (super-keeper number?
               (list 5 (list 4 "hello" "bye" (list 10) "hi") 'huge 0.99))
 (list (list "hello" "bye" (list) "hi") 'huge))
(check-expect
 (super-keeper odd?
               (list 1 (list (list 3) 5 (list 7 9)) 11))
 (list (list (list) (list))))
(check-expect
 (super-keeper even?
               (list 1 (list (list 3) 5 (list 7 9)) 11))
 (list 1 (list (list 3) 5 (list 7 9)) 11))