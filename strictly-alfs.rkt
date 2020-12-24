;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 08, Problem 3
;; **********************************************
;;

;; 3a)
;; (occurrences n lst) Produces the number of times n occurs in lst.
;; Examples:
(check-expect (occurrences 2 '(1 2 1 2 2 3 1)) 3)
(check-expect (occurrences 1 '(1 0 1 0 1 0 1 0 1)) 5)

;; occurrences: Num (listof Num) -> Nat
(define (occurrences n lst)
  (foldr (lambda (x rorr) (cond [(= x n) (add1 rorr)]
                                [else rorr])) 0 lst))

;; Tests:
(check-expect (occurrences 5 '(1 2 1 2 2 3 1)) 0)
(check-expect (occurrences 5 empty) 0)
(check-expect (occurrences 1 '(1 2 1 2 2 3 1 1 1 1 1)) 7)
(check-expect (occurrences 1 '(1 2 1)) 2)


;; 3b)
;; (zip lst1 lst2) Produces a list of pairs where the ith pair contains the ith
;; element of the lst1 followed by the ith element of the lst2.
;; Examples:
(check-expect (zip '(1 2 3) '(a b c)) '((1 a)(2 b)(3 c)))
(check-expect (zip '(1 2 3) '(3 2 1)) '((1 3)(2 2)(3 1)))

;; zip: (listof X) (listof Y) -> (listof (list X Y))
;; Requires: lst1 and lst2 contain the same number of elements
(define (zip lst1 lst2)
  (foldr (lambda (x y rorr) (cons (list x y) rorr)) empty lst1 lst2))

;; Tests:
(check-expect (zip empty empty) '())
(check-expect (zip '(1) '(a)) '((1 a)))
(check-expect (zip '(1 2 3 empty) '(a b c d)) '((1 a)(2 b)(3 c) (empty d)))
(check-expect (zip '("hi" "hello") '(0.9 0.1)) '(("hi" 0.9)("hello" 0.1)))


;; 3c)
;; (unzip lst) Produces a list of two lists. The first list contains the first
;; element from each pair in lst, and the second list contains the second
;; element from each pair in lst, in the original order. 
;; Examples:
(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '((1 3)(2 2)(3 1))) '((1 2 3) (3 2 1)))

;; unzip: (listof (list X Y)) -> (list (listof X) (listof Y))
(define (unzip lst)
  (foldr (lambda (x rorr) (list (cons (first x) (first rorr))
                                (cons (second x) (second rorr))))
         (list empty empty) lst))

;; Tests:
(check-expect (unzip '()) '(()()))
(check-expect (unzip '((1 a))) '((1) (a)))
(check-expect (unzip '((1 a)(2 b)(3 c) (empty d))) '((1 2 3 empty) (a b c d)))
(check-expect (unzip '(("hi" 0.9)("hello" 0.1))) '(("hi" "hello") (0.9 0.1)))


;; 3d)
;; (subsequence lst from to) Produces  the subsequence from lst that begins at
;; index from and ends just before index to.
;; Examples:
(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))
(check-expect (subsequence '(1 2 3 4 5 6 7 8 9 0) 4 6) '(5 6))

;; subsequence: (listof X) Nat Nat -> (listof X)
(define (subsequence lst from to)
  (cond [(< (length lst) from) empty]
        [(< (length lst) to)
         (foldr (lambda (x rorr)
                  (cond [(< (length rorr) (- (length lst) from))
                         (cons x rorr)]
                        [else rorr])) empty lst)]
        [else (foldl (lambda (x rorr)
                       (cond [(< (length rorr) (- to from))
                              (cons x rorr)]
                             [else rorr]))
                     empty
                     (foldl (lambda (x rorr)
                              (cond [(< (length rorr) to)
                                     (cons x rorr)]
                                    [else rorr])) empty lst))]))

;; Tests:
(check-expect (subsequence empty 10 10) empty)
(check-expect (subsequence '(a b c d e f g) 1 1) empty)
(check-expect (subsequence '("apple" "berry" "cherry")  10 20) empty)
(check-expect (subsequence '(a b 1 d) 2 400) '(1 d))
(check-expect (subsequence '(a b c d) 0 400) '(a b c d))