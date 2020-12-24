;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 05, Problem 1
;; **********************************************
;;

(define-struct ls (first rest))
;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))


;; (ls-length ls) Produces the length of ls.
;; Examples:
(check-expect (ls-length
               (make-ls "!" (make-ls 'huh (make-ls 42 'nothing)))) 3)
(check-expect (ls-length
               (make-ls 10 (make-ls 'oops 'nothing))) 2)

;; ls-length: (lsof Any) -> Nat 
(define (ls-length ls)
  (cond [(equal? ls 'nothing) 0]
        [else (add1 (ls-length (ls-rest ls)))]))

;; Tests:
(check-expect (ls-length 'nothing) 0)
(check-expect (ls-length (make-ls "asfds" 'nothing)) 1)
(check-expect (ls-length (make-ls 21
                                  (make-ls "dog"
                                           (make-ls "john"
                                                    (make-ls 'jeff 'nothing))))) 4)


;; (ls-max ls-num) Produces the largest value in ls-num.
;; Examples:
(check-expect (ls-max (make-ls 5 (make-ls 9 (make-ls 7 'nothing)))) 9)
(check-expect (ls-max (make-ls 4.5 (make-ls 3.9 (make-ls 10 'nothing)))) 10)

;; ls-max: (lsof Num) -> Num
;; Requires: ls-num contains at least one value
(define (ls-max ls-num) (ls-max-recurse ls-num (ls-first ls-num)))

;; Tests:
(check-expect (ls-max (make-ls 3.14 'nothing)) 3.14)
(check-expect (ls-max (make-ls -10 (make-ls -20 'nothing))) -10)
(check-expect (ls-max (make-ls 50.97 (make-ls 0 (make-ls 13.49 (make-ls -15.64 'nothing))))) 50.97)


;; (ls-max-recurse ls-num x) Produces x which is the largest value in ls-num.
;; Examples and tests: see wrapper function ls-max

;; ls-max-recurse: (lsof Num) Num -> Num
;; Requires: ls-num contains at least one value
(define (ls-max-recurse ls-num x)
  (cond [(equal? ls-num 'nothing) x]
        [(< x (ls-first ls-num))
         (ls-max-recurse (ls-rest ls-num) (ls-first ls-num))]
        [else (ls-max-recurse (ls-rest ls-num) x)]))