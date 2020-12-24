;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recipe-needed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 09, Problem 5
;; **********************************************
;;

;; (m1 a) Determines if a reads the same forwards and backwards. Upper and lower
;; case letters are considered different characters.
;; Examples:
(check-expect (m1 "hello") false)
(check-expect (m1 "helleh") true)

;; m1: Str -> Bool
(define (m1 a)
  (local
    [;; Constant containing a list of all letters in a.
     (define b (string->list a))]
    (foldr
     (lambda (x y z)
       (and (char=? x y) z))
     true
     b
     (foldl cons empty b))))

;; Tests:
(check-expect (m1 "") true)
(check-expect (m1 "h") true)
(check-expect (m1 "9h9") true)
(check-expect (m1 "helleH") false)


;; (m2 a) Produces a function that determines if the given string is the same
;; sorted alphabetically as an element in a sorted alphabetically, but not
;; the same when they are not sorted.
;; Examples:
(check-expect ((m2 (list "hi" "a")) "ih") true)
(check-expect ((m2 (list "bob" "eric" "donald")) "jeffery") false)

;; (listof Str) -> (Str -> Bool)
(define (m2 a)
  (lambda (s)
    (local
      [;; (r x) Produces a list of the letters in x in alphabetical order.
       ;; r: Str -> (listof Char)
       (define (r x) (quicksort (string->list x) char<?))
       ;; Constant containing the association list of each element in a paired
       ;; with a list of the letters in that element in alphabetical order.
       (define u (map (lambda (x) (list x (r x))) a))
       ;; (t x) Produces the number of elements in x.
       ;; (listof Any) -> Nat
       (define (t x) (foldr (lambda (y z) (add1 z)) 0 x))
       ]
      (foldr
       (lambda (x y)
         (cond
           [(string=? s (first x)) y]
           [(not (= (t (r s)) (t (second x)))) y]
           [else
            (or y (foldr
                   (lambda (x y)
                     (cond
                       [(and x y) y]
                       [else false]))
                   true
                   (map (lambda (a b) (char=? a b)) (r s) (second x))))]))
       false
       u))))

;; Tests:
(check-expect ((m2 (list "hi" "a")) "hi") false)
(check-expect ((m2 (list "hill" "a")) "b") false)
(check-expect ((m2 (list "hill" "a")) "a") false)
(check-expect ((m2 (list "hill" "a")) "ilhl") true)
(check-expect ((m2 (list "ilhl" "hill" "a")) "ilhl") true)


;; (m3 a b) Produces function a applied to the difference betweeen the largest
;; integer in b and smallest integer in b.
;; Examples:
(check-expect (m3 zero? (list 4 5 2 3 1)) false)
(check-expect (m3 zero? (list 5 5 5 5 5)) true)

;; m3: (Int -> Any) (ne-listof Any) -> Any
;; Requires: b contains at least 1 integer
(define (m3 a b)
   (local
     [;; Constant that contains all integer values in b.
      (define c (filter integer? b))
      ;; (d e f) Produces the element in e that produces true for f with any
      ;; other element in e.
      ;; (listof Int) (Int Int -> Bool) -> Int
      (define (d e f)
        (foldl (lambda (x y)
                 (cond
                   [(f x y) x]
                   [else y]))
               (first e)
               e))]
     (a (- (d c >) (d c <)))))

;; Tests:
(check-expect (m3 (lambda (x) x) (list 1 2 3 'b "hello")) 2)
(check-expect (m3 even? (list 1)) true)
(check-expect (m3 add1 (list 4 5 2 3 1)) 5)
(check-expect (m3 sub1 (list "hi" "hello" 0 9 -10)) 18)