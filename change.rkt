;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 09, Problem 4
;; **********************************************
;;

;; A ChangeList is a (listof (list Nat (listof Nat)))

;; (fewest-coins sum lst) Produces a list of the fewest coins needed to make
;; sum, using the coin values in lst.
;; Examples:
(check-expect (fewest-coins 45 '(1 5 10 25 100 200)) '(25 10 10))
(check-expect (fewest-coins 50 '(1 5 10 45 100 200)) '(45 5))

;; fewest-coins: Nat (listof Nat) -> (listof Nat)
;; Requires: lst contains 1 and is sorted in strictly increasing order with no
;; duplicates
(define (fewest-coins sum lst)
  (local [;; (element-main? x lst) Determines if x is in lst.
          ;; element-main?: Nat (listof Nat) -> Bool
          (define (element-main? x lst)
            (cond [(empty? lst) false]
                  [(= (first lst) x) true]
                  [else (element-main? x (rest lst))]))
          ;; (element x best-lst) Produces the element in best-lst corresponding
          ;; with x.
          ;; element: Nat ChangeList -> (list Nat (listof Nat))
          (define (element x best-lst)
            (cond [(= x (first (first best-lst))) (first best-lst)]
                  [else (element x (rest best-lst))]))
          ;; (shortest-length lstr) Produces the list in lst with the shortest
          ;; length.
          ;; shortest-length: (ne-listof (listof Num)) -> (listof Num)
          (define (shortest-length lst)
            (foldl (lambda (x y)
                     (cond
                       [(< (length x) (length y)) x]
                       [else y]))
                   (first lst)
                   lst))
          ;; (best-change sum x best-lst) Produces a list of all potential
          ;; candidates for being the fewest coins for sum from best-lst,
          ;; given x is a counter.
          ;; Nat Nat (listof (listof Num)) -> (listof (listof Num))
          (define (best-change sum x best-lst)
            (cond [(element-main? sum lst) (list (list sum))]
                  [(= x sum) empty]
                  [else (cons (append (second (element x best-lst))
                                      (second (element (- sum x) best-lst)))
                              (best-change sum (add1 x) best-lst))]))
          ;; (change-list sum x best-lst) Produces list best-lst containing the
          ;; lists containing the fewest coins needed to make each value prior
          ;; to sum, given x is a counter.
          ;; change-list Nat Nat ChangeList -> ChangeList
          (define (change-list sum x best-lst)
            (cond [(element-main? sum lst) (list sum)]
                  [(= x sum) best-lst]
                  [else (change-list sum (add1 x)
                                     (cons (list x
                                                 (shortest-length
                                                  (best-change x 1 best-lst)))
                                           best-lst))]))]
    (cond [(zero? sum) empty]
          [else (foldl cons empty (shortest-length
                                   (best-change sum 1
                                                (change-list sum 1 empty))))])))

;; Tests:
(check-expect (fewest-coins 0 '(1 5 10 45 100 200)) '())
(check-expect (fewest-coins 45 '(1 5 10 45 100 200)) '(45))
(check-expect (fewest-coins 10 '(1)) '(1 1 1 1 1 1 1 1 1 1))
(check-expect (fewest-coins 4 '(1 5 10 45 100 200)) '(1 1 1 1))
(check-expect (fewest-coins 123 '(1 5 10 45 100 200)) '(100 10 10 1 1 1))