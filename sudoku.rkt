;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 02, Problem 7
;; **********************************************
;;

(define valid (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 8 (cons 7 (cons 6 empty))))))))))


;; (sudoku-valid? x) Determines whether the list x is a valid sudoku or not.
;; Examples:
(check-expect (sudoku-valid? (cons 1 valid)) false)
(check-expect (sudoku-valid? (cons 0 valid)) false)

;; sudoku-valid?: (listof Num) => Bool
(define (sudoku-valid? x) (cond [(and (= (length x) 9)
                                      (= (product x) 362880)
                                      (= (sum x) 45)
                                      (sudoku-valid2? x)) true]
                                [else false]))

;; Tests:
(check-expect (sudoku-valid? empty) false)
(check-expect (sudoku-valid? valid) true)
(check-expect (sudoku-valid? (cons 1 (cons 0 valid))) false)
(check-expect (sudoku-valid? (cons 2 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 8 (cons 7 (cons 3 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 2 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 8 (cons 9 (cons 3 empty)))))))))) false)


;; (sudoku-valid2? x)  Determines whether the list x contains only values of a sudoku board.
;; Examples and tests: see wrapper function sudoku-valid?

;; sudoku-valid2?: (listof Num) => Bool
(define (sudoku-valid2? x) (cond[(empty? x) true]
                                [(= (first x) 1) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 2) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 3) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 4) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 5) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 6) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 7) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 8) (and true (sudoku-valid2? (rest x)))]
                                [(= (first x) 9) (and true (sudoku-valid2? (rest x)))]
                                [else false]))


;; (product x) Produces the product of all the values in list x.
;; Example:
(check-expect (product (cons 2 (cons 4 empty))) 8)

;; product: (ne-listof Num) => Num 
(define (product x) (cond[(empty? (rest x)) (first x)]
                         [else (* (first x) (product (rest x)))]))


;; (sum x) Produces the sum of all the values in list x.
;; Example:
(check-expect (sum (cons 2 (cons 4 empty))) 6)

;; sum: (ne-listof Num) => Num 
(define (sum x) (cond[(empty? (rest x)) (first x)]
                         [else (+ (first x) (sum (rest x)))]))