;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 03, Problem 2
;; **********************************************
;;

;; (total-vowels x) Produces the total number of vowels in list x.
;; Examples:
(check-expect (total-vowels (cons "test" (cons "look" empty))) 3)
(check-expect (total-vowels (cons "toddler" (cons "look" empty))) 4)

;; total-vowels: (listof Str) => Nat
(define (total-vowels x) (cond [(empty? x) 0]
                               [else (+ (vowels (string->list (first x))) (total-vowels (rest x)))]))

;; Tests:
(check-expect (total-vowels (cons "tst" (cons "lk" empty))) 0)
(check-expect (total-vowels empty) 0)
(check-expect (total-vowels (cons "test" (cons "aeeoiau" (cons "look" empty)))) 10)


;; (vowels x) Produces the number of vowels in string x.
;; Examples:
(check-expect (vowels (cons #\f (cons #\e (cons #\e (cons #\t empty))))) 2)
(check-expect (vowels (cons #\d (cons #\o (cons #\g empty)))) 1)

;; vowels: (listof char) => Nat
(define (vowels x) (cond[(empty? x) 0]
                        [(or (char=? (first x) #\a) (char=? (first x) #\e) (char=? (first x) #\i)
                             (char=? (first x) #\o) (char=? (first x) #\u)) (+ 1 (vowels (rest x)))]
                        [else (vowels (rest x))]))

;; Tests:
(check-expect (vowels empty) 0)
(check-expect (vowels (cons #\f empty)) 0)
(check-expect (vowels (cons #\a (cons #\o (cons #\u empty)))) 3)
