;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname groceries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 05, Problem 2
;; **********************************************
;;

(define-struct grocery (dept name cost mass))
;; A Grocery is a (make-grocery Str Str Num Num)
;; Requires: cost >= 0, mass > 0.

;; A Store is a (listof Grocery)
;; Requires: no two items have both the same dept and same name.

;; A StrPatt is a (anyof Str 'dontcare)

(define-struct query (dept name cost mass))
;; A GroceryQuery is a
;; (make-query StrPatt StrPatt Interval Interval)

(define-struct interval (lo hi))
;; An Interval is a (make-interval (anyof 'dontcare Num)
;;                                 (anyof 'dontcare Num))


(define try-n-save
  (list (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "seed" "rice" 0.95 1000)
        (make-grocery "dairy" "milk" 3.99 4000)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "potato" 2.99 5000)
        (make-grocery "chips" "potato" 1.99 250)
        (make-grocery "chips" "corn" 1.99 275)
        (make-grocery "seed" "wheat" 0.49 500)
        (make-grocery "produce" "banana" 0.69 450)
        (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "chips" "banana" 1.99 50)
        (make-grocery "produce" "peach" 3.99 400)
        (make-grocery "seed" "lentil" 2.99 800)
        (make-grocery "produce" "corn" 0.99 100)
        (make-grocery "seed" "corn" 4.99 850)
        (make-grocery "dairy" "kefir" 5.99 1000)))

(define kwik-e-mart
  (list (make-grocery "seed" "rice" 0.38 400)
        (make-grocery "can" "corn" 4.00 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "apple" 2.99 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "pineapple" 3.17 250)))

(define student-shop
  (list (make-grocery "produce" "apple" 2.99 300)
        (make-grocery "produce" "orange" 3.99 400)
        (make-grocery "produce" "grapes" 5.99 500)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "seed" "sunflower" 3.49 800)
        (make-grocery "seed" "lentil" 10.49 300)
        (make-grocery "dairy" "milk" 3.99 600)
        (make-grocery "dairy" "kefir" 5.99 1000)
        (make-grocery "dairy" "cheese" 6.49 1000)
        (make-grocery "dairy" "icecream" 5.99 400)))


;; (in-interval? num interval) Determines if number num is in Interval interval.
;; Examples:
(check-expect (in-interval? 50
                            (make-interval 23 40)) false)
(check-expect (in-interval? 20
                            (make-interval 5 30)) true)

;; in-interval?: Num Interval -> Bool
(define (in-interval? num interval)
  (cond [(and (equal? (interval-lo interval) 'dontcare)
              (equal? (interval-hi interval) 'dontcare)) true]
        [(equal? (interval-lo interval) 'dontcare) (<= num (interval-hi interval))]
        [(equal? (interval-hi interval) 'dontcare) (>= num (interval-lo interval))]
        [else (and (>= num (interval-lo interval)) (<= num (interval-hi interval)))]))

;; Tests:
(check-expect (in-interval? 42
                            (make-interval 'dontcare 'dontcare)) true)
(check-expect (in-interval? 34
                            (make-interval 35 'dontcare)) false)
(check-expect (in-interval? 34
                            (make-interval 'dontcare 35)) true)


;; (find-matches lstof-g gq) Produces all Grocery in lstof-g that fit GroceryQuery gq.
;; Examples:
(check-expect
 (find-matches try-n-save (make-query "seed" 'dontcare
                                      (make-interval 'dontcare 'dontcare)
                                      (make-interval 'dontcare 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)
  (make-grocery "seed" "pinto" 2.49 500)
  (make-grocery "seed" "wheat" 0.49 500)
  (make-grocery "seed" "lentil" 2.99 800)
  (make-grocery "seed" "corn" 4.99 850)))
(check-expect
 (find-matches try-n-save (make-query 'dontcare "corn"
                                      (make-interval 'dontcare 'dontcare)
                                      (make-interval 'dontcare 'dontcare)))
 (list (make-grocery "chips" "corn" 1.99 275)
       (make-grocery "produce" "corn" 0.99 100)
       (make-grocery "seed" "corn" 4.99 850)))

;; find-matches (listof Grocery) GroceryQuery -> (listof Grocery)
(define (find-matches lstof-g gq)
  (cond [(empty? lstof-g) empty]
        [(fit? (first lstof-g) gq) (cons (first lstof-g) (find-matches (rest lstof-g) gq))]
        [else (find-matches (rest lstof-g) gq)]))

;; Tests:
(check-expect
 (find-matches try-n-save (make-query "seed" 'dontcare
                                      (make-interval 'dontcare 3.00)
                                      (make-interval 600 'dontcare)))
 (list
  (make-grocery "seed" "rice" 0.95 1000)
  (make-grocery "seed" "lentil" 2.99 800)))


;; (fit? g gq) Determines whether Grocery g fits in GroceryQuery gq.
;; Example:
(check-expect (fit? (make-grocery "seed" "rice" 0.95 1000)
                    (make-query "seed" 'dontcare
                                (make-interval 'dontcare 3.00)
                                (make-interval 600 'dontcare))) true)

;; fit? Grocery GroceryQuery -> Bool
(define (fit? g gq)
  (and (or (equal? (query-dept gq) 'dontcare) (string=? (grocery-dept g) (query-dept gq)))
       (or (equal? (query-name gq) 'dontcare) (string=? (grocery-name g) (query-name gq)))
       (in-interval? (grocery-cost g) (query-cost gq))
       (in-interval? (grocery-mass g) (query-mass gq))))


;; (sort-dept-name store) Produces Store store sorted in alphabetical order.
;; Examples:
(check-expect (sort-dept-name try-n-save)
              (list
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "produce" "apple" 2.49 600)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "seed" "wheat" 0.49 500)))
(check-expect (sort-dept-name kwik-e-mart)
              (list
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.38 400)))

;; sort-dept-name: Store -> Store
(define (sort-dept-name store)
  (dept-sort (name-sort store)))

;; Tests:
(check-expect (sort-dept-name empty) empty)
(check-expect (sort-dept-name
               (list (make-grocery "can" "corn" 4 400)))
              (list (make-grocery "can" "corn" 4 400)))
(check-expect (sort-dept-name student-shop)
              (list
               (make-grocery "dairy" "cheese" 6.49 1000)
               (make-grocery "dairy" "icecream" 5.99 400)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "milk" 3.99 600)
               (make-grocery "produce" "apple" 2.99 300)
               (make-grocery "produce" "grapes" 5.99 500)
               (make-grocery "produce" "orange" 3.99 400)
               (make-grocery "seed" "lentil" 10.49 300)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "sunflower" 3.49 800)))


;; (name-sort store) sorts the names of Store store in alphabetical order.
;; Example:
(check-expect (name-sort student-shop)
              (list
               (make-grocery "produce" "apple" 2.99 300)
               (make-grocery "dairy" "cheese" 6.49 1000)
               (make-grocery "produce" "grapes" 5.99 500)
               (make-grocery "dairy" "icecream" 5.99 400)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "seed" "lentil" 10.49 300)
               (make-grocery "dairy" "milk" 3.99 600)
               (make-grocery "produce" "orange" 3.99 400)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "sunflower" 3.49 800)))

;; name-sort: Store --> Store
(define (name-sort log)
  (cond [(empty? log) empty]
        [else (name-insert (first log) (name-sort (rest log)))]))


;; (name-insert g slog) inserts the Grocery g into the sorted list slog
;; so that the resulting list is also sorted.
;; Examples and tests: see wrapper function name-sort

;; insert: Grocery Store --> Store
;; requires: slog is sorted in alphabetical order for names
(define (name-insert g slog)
  (cond [(empty? slog) (cons g empty)]
        [(string<=? (grocery-name g) (grocery-name (first slog))) (cons g slog)]
        [else (cons (first slog) (name-insert g (rest slog)))]))


;; (dept-sort store) sorts the departments of Store store in alphabetical order.
;; Example:
(check-expect (dept-sort student-shop)
              (list
               (make-grocery "dairy" "milk" 3.99 600)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "cheese" 6.49 1000)
               (make-grocery "dairy" "icecream" 5.99 400)
               (make-grocery "produce" "apple" 2.99 300)
               (make-grocery "produce" "orange" 3.99 400)
               (make-grocery "produce" "grapes" 5.99 500)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "sunflower" 3.49 800)
               (make-grocery "seed" "lentil" 10.49 300)))

;; dept-sort: Store --> Store
(define (dept-sort log)
  (cond [(empty? log) empty]
        [else (dept-insert (first log) (dept-sort (rest log)))]))


;; (dept-insert g slog) inserts the Grocery g into the sorted list slog
;; so that the resulting list is also sorted.
;; Examples and tests: see wrapper function dept-sort

;; dept-insert: Num (listof Num) --> (listof Num)
;; requires: slog is sorted in alphabetical order for departments
(define (dept-insert g slog)
  (cond [(empty? slog) (cons g empty)]
        [(string<=? (grocery-dept g) (grocery-dept (first slog))) (cons g slog)]
        [else (cons (first slog) (dept-insert g (rest slog)))]))


;; (overlap store1 store2) Produces a new Store containing only those
;; items available in both Store store1 and Store store2.
;; Examples:
(check-expect
 (overlap kwik-e-mart try-n-save)
 (list (make-grocery "produce" "apple" 2.49 600)
       (make-grocery "seed" "pinto" 2.49 500)
       (make-grocery "seed" "rice" 0.38 400)))
(check-expect
 (overlap kwik-e-mart student-shop)
 (list (make-grocery "produce" "apple" 2.99 400)
       (make-grocery "seed" "pinto" 2.49 500)))

;; overlap: Store Store -> Store
(define (overlap store1 store2)
  (overlap-recurse (sort-dept-name store1) (sort-dept-name store2)))

;; Tests:
(check-expect (overlap kwik-e-mart empty) empty)
(check-expect (overlap student-shop (list (make-grocery "dairy" "cheese" 6.49 1000)))
              (list (make-grocery "dairy" "cheese" 6.49 1000)))
(check-expect
 (overlap kwik-e-mart student-shop)
 (list (make-grocery "produce" "apple" 2.99 400)
       (make-grocery "seed" "pinto" 2.49 500)))
(check-expect
 (overlap try-n-save student-shop)
 (list (make-grocery "dairy" "cheese" 6.49 1000)
       (make-grocery "dairy" "kefir" 5.99 1000)
       (make-grocery "dairy" "milk" 3.99 4000)
       (make-grocery "produce" "apple" 2.49 600)
       (make-grocery "seed" "lentil" 2.99 800)
       (make-grocery "seed" "pinto" 2.49 500)))


;; (overlap-recurse store1 store2) Produces a new Store containing only those
;; items available in both Store store1 and Store store2.
;; Examples and tests: see wrapper function overlap

;; overlap-recurse: Store Store -> Store
(define (overlap-recurse store1 store2)
  (cond [(or (empty? store1) (empty? store2)) empty]
        [(string=? (grocery-dept (first store1)) (grocery-dept (first store2)))
         (cond [(string=? (grocery-name (first store1)) (grocery-name (first store2)))
                (cons (value (first store1) (first store2)) (overlap-recurse (rest store1) (rest store2)))]
               [(string>? (grocery-name (first store1)) (grocery-name (first store2)))
                (overlap-recurse store1 (rest store2))]
               [else (overlap-recurse (rest store1) store2)])]
        [(string>? (grocery-dept (first store1)) (grocery-dept (first store2)))
         (overlap-recurse store1 (rest store2))]
        [else (overlap-recurse (rest store1) store2)]))


;; (value g1 g2) Produces the more value option between g1 and g2.
;; Example:
(check-expect (value (make-grocery "produce" "apple" 2.99 400)
                     (make-grocery "produce" "apple" 10.99 100))
              (make-grocery "produce" "apple" 2.99 400))

;; value: Grocery Grocery -> Grocery
(define (value g1 g2)
  (cond [(> (/ (grocery-cost g1) (grocery-mass g1))
            (/ (grocery-cost g2) (grocery-mass g2)))
         g2]
        [(> (/ (grocery-cost g2) (grocery-mass g2))
            (/ (grocery-cost g1) (grocery-mass g1)))
         g1]
        [(> (grocery-mass g1) (grocery-mass g2)) g2]
        [else g1]))


;; (scale-prices store gq ratio) Produces a new Store where all items in Store store
;; that satisfy the GroceryQuery gq have their prices changed by ratio and the
;; order of the items remain the same.
;; Examples:
(check-expect (scale-prices
               kwik-e-mart
               (make-query "can"
                           'dontcare
                           (make-interval 'dontcare 'dontcare)
                           (make-interval 'dontcare 'dontcare)) 1.10)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 4.40 400)
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 2.41 350)
                    (make-grocery "produce" "pineapple" 3.17 250)))
(check-expect (scale-prices try-n-save
                            (make-query "seed"
                                        'dontcare
                                        (make-interval 0 5)
                                        (make-interval 'dontcare 'dontcare)) 1.5)
              (list (make-grocery "produce" "apple" 2.49 600)
                    (make-grocery "seed" "rice" 1.42 1000)
                    (make-grocery "dairy" "milk" 3.99 4000)
                    (make-grocery "seed" "pinto" 3.74 500)
                    (make-grocery "produce" "potato" 2.99 5000)
                    (make-grocery "chips" "potato" 1.99 250)
                    (make-grocery "chips" "corn" 1.99 275)
                    (make-grocery "seed" "wheat" 0.74 500)
                    (make-grocery "produce" "banana" 0.69 450)
                    (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "chips" "banana" 1.99 50)
                    (make-grocery "produce" "peach" 3.99 400)
                    (make-grocery "seed" "lentil" 4.48 800)
                    (make-grocery "produce" "corn" 0.99 100)
                    (make-grocery "seed" "corn" 7.48 850)
                    (make-grocery "dairy" "kefir" 5.99 1000)))

;; scale-prices: Store GroceryQuery Nat -> Store
(define (scale-prices store gq ratio)
  (cond [(empty? store) empty]
        [(fit? (first store) gq)
         (cons  (inflate (first store) ratio) (scale-prices (rest store) gq ratio))]
        [else (cons (first store) (scale-prices (rest store) gq ratio))]))

;; Tests:
(check-expect (scale-prices student-shop
                            (make-query "produce"
                                        'dontcare
                                        (make-interval 'dontcare 7)
                                        (make-interval 'dontcare 'dontcare)) 0)
              (list
               (make-grocery "produce" "apple" 0 300)
               (make-grocery "produce" "orange" 0 400)
               (make-grocery "produce" "grapes" 0 500)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "sunflower" 3.49 800)
               (make-grocery "seed" "lentil" 10.49 300)
               (make-grocery "dairy" "milk" 3.99 600)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "cheese" 6.49 1000)
               (make-grocery "dairy" "icecream" 5.99 400)))
(check-expect (scale-prices student-shop
                            (make-query "can"
                                        'dontcare
                                        (make-interval 'dontcare 7)
                                        (make-interval 'dontcare 10)) 1000)
              student-shop)
(check-expect (scale-prices try-n-save
                            (make-query 'dontcare
                                        'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare)) 3.22)
              (list
               (make-grocery "produce" "apple" 8.02 600)
               (make-grocery "seed" "rice" 3.06 1000)
               (make-grocery "dairy" "milk" 12.85 4000)
               (make-grocery "seed" "pinto" 8.02 500)
               (make-grocery "produce" "potato" 9.63 5000)
               (make-grocery "chips" "potato" 6.41 250)
               (make-grocery "chips" "corn" 6.41 275)
               (make-grocery "seed" "wheat" 1.58 500)
               (make-grocery "produce" "banana" 2.22 450)
               (make-grocery "dairy" "cheese" 20.9 900)
               (make-grocery "chips" "banana" 6.41 50)
               (make-grocery "produce" "peach" 12.85 400)
               (make-grocery "seed" "lentil" 9.63 800)
               (make-grocery "produce" "corn" 3.19 100)
               (make-grocery "seed" "corn" 16.07 850)
               (make-grocery "dairy" "kefir" 19.29 1000)))
(check-expect (scale-prices kwik-e-mart
                            (make-query "dairy"
                                        "icecream"
                                        (make-interval 'dontcare 10)
                                        (make-interval 'dontcare 20)) 23.45)
              (list
               (make-grocery "seed" "rice" 0.38 400)
               (make-grocery "can" "corn" 4 400)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "pineapple" 3.17 250)))


;; (inflate g ratio) Produces Grocery g with its price changed by ratio.
;; Example:
(check-expect (inflate (make-grocery "seed" "rice" 0.38 400) 5)
              (make-grocery "seed" "rice" 1.90 400))

;; inflate: Grocery Nat -> Grocery
(define (inflate g ratio)
  (make-grocery
   (grocery-dept g)
   (grocery-name g)
   (/ (round (* (grocery-cost g) ratio 100)) 100)
   (grocery-mass g)))