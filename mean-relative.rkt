;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mean-relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 02, Problem 6
;; **********************************************
;;

;; (mean-relative x) Produces whether each element in list x is on the mean, below-mean, or above-mean.
;; Examples:
(check-expect (mean-relative (cons 5 (cons 7 (cons 9 (cons 12 empty))))) (cons 'below-mean (cons 'below-mean (cons 'above-mean
(cons 'above-mean empty)))))
(check-expect (mean-relative (cons 5 (cons 5 empty))) (cons 'mean (cons 'mean empty)))

;; mean-realtive: (ne-listof Num) => (ne-listof Sym)
(define (mean-relative x) (mean-relative2 x (mean x)))

;; Tests:
(check-expect (mean-relative (cons 5 empty)) (cons 'mean empty))
(check-expect (mean-relative (cons 6 (cons 2 (cons 7 (cons 5 empty))))) (cons 'above-mean (cons 'below-mean (cons 'above-mean(cons 'mean empty)))))


;; (mean-realtive2 x y) Produces whether each element in list x is on, below, or above the y.
;; Examples and tests: see wrapper function mean-realtive

;; mean-realtive2: (ne-listof Num) Num => (ne-listof Sym)
(define (mean-relative2 x y) (cond[(and (empty? (rest x)) (> (first x) y)) (cons 'above-mean empty)]
                                  [(and (empty? (rest x)) (< (first x) y)) (cons 'below-mean empty)]
                                  [(and (empty? (rest x)) (= (first x) y)) (cons 'mean empty)]
                                  [(> (first x) y) (cons 'above-mean (mean-relative2 (rest x) y))]
                                  [(< (first x) y) (cons 'below-mean (mean-relative2 (rest x) y))]
                                  [(= (first x) y) (cons 'mean (mean-relative2 (rest x) y))]))


;; (mean x) Produces the mean of list x of Num.
;; Example:
(check-expect (mean (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 0 empty))))))))))) 4.5)

;; mean: (ne-listof Num) => Num
(define (mean x) (/ (sum x) (length x)))


;; (sum x) Produces the sum of list x of Num.
;; Example:
(check-expect (sum (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 5 empty))))))))))) 50)

;; sum: (ne-listof Num) => Num
(define (sum x) (cond [(empty? (rest x)) (first x)]
                       [else (+ (first x) (sum (rest x)))]))