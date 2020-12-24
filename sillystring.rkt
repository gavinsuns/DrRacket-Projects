;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sillystring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 05, Problem 3
;; **********************************************
;;

(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)
;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct


;; (sillify s) Produces the the corresponding SillyStr for string s.
;; Examples:
(check-expect (sillify "Babbage")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))
(check-expect (sillify "Lovelace")
              (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))

;; sillify: Str -> SillyStr
(define (sillify s) (sillify-recurse (string->list s)))

;; Tests:
(check-expect (sillify "") empty)
(check-expect (sillify "a") #\a)
(check-expect (sillify "BobbY")
              (make-silly-string
               #\B
               (make-silly-string
                #\o
                 #\b
                 #\b)
                #\Y))


;; (sillify-recurse s) Produces the corresponding SillyStr for list s.
;; Examples and tests: see wrapper function (sillify s)

;; sillify-recurse: (listof Char) -> SillyStr
(define (sillify-recurse s)
  (cond [(empty? s) empty]
        [(empty? (rest s)) (first s)]
        [else (make-silly-string (first s) (sillify-recurse
                                            (silly (rest (silly (rest s)))))
                                 (first (silly s)))]))


;; (silly lst) Produces list lst with all its elements in reverse.
;; Example:
(check-expect (silly (list 1 2 3 4 5))
                     (list 5 4 3 2 1))

;; silly: (listof Any) -> (listof Any)
(define (silly lst)
  (silly-recurse lst empty))


;; (silly-recurse Produces acc, which is list lst with all its elements reversed.
;; Examples and tests: see wrapper function silly

;; silly-recurse: (listof Any) (listof Any) -> (listof Any)
(define (silly-recurse lst acc)
  (cond [(empty? lst) acc]
        [else (silly-recurse
               (rest lst) (cons (first lst) acc))]))


;; (unsillify ss) Produces the corresponding Str for SillyStr ss.
;; Examples:
(check-expect (unsillify
               (make-silly-string
                #\L
                (make-silly-string
                 #\o
                 (make-silly-string
                  #\v
                  (make-silly-string
                   #\e
                   empty
                   #\l)
                  #\a)
                 #\c)
                #\e))
              "Lovelace")
(check-expect (unsillify
               (make-silly-string
                #\B
                (make-silly-string
                 #\a
                 (make-silly-string
                  #\b
                  #\b
                  #\a)
                 #\g)
                #\e))
              "Babbage")

;; unsillify: SillyStr -> Str
(define (unsillify ss) (unsillify-recurse ss empty empty))

;; Tests:
(check-expect (unsillify empty) "")
(check-expect (unsillify #\a) "a")
(check-expect (unsillify
               (make-silly-string #\B
                                  (make-silly-string
                                   #\o
                                   #\b
                                   #\b)
                                  #\Y))
               "BobbY")


;; (unsillify-recurse ss front back) Produces Str front appeneded to Str back, which is
;; the corresponding Str for SillyStr ss.
;; Examples and tests: see wrapper function unsillify

;; unsillify-recurse: SillyStr (listof Char) (listof Char) -> Str
(define (unsillify-recurse ss front back)
  (cond [(empty? ss) (list->string (append (silly front) back))]
        [(char? ss)
         (unsillify-recurse empty front (cons ss back))]
        [else (unsillify-recurse (silly-string-middle ss) (cons (silly-string-first ss) front)
                                 (cons (silly-string-last ss) back))]))


;; (palindrome? ss) Determines whether SillyStr ss is a palindrome.
;; Examples:
(check-expect (palindrome?
               (make-silly-string
                #\r
                (make-silly-string #\a #\d #\a)
                #\r)) true)
(check-expect (palindrome?
               (make-silly-string
                #\s
                (make-silly-string #\o #\n #\a)
                #\r)) false)

;; palindrome?: SillyStr -> Bool
(define (palindrome? ss)
  (cond [(empty? ss) true]
        [(char? ss) true]
        [(not (char=? (silly-string-first ss) (silly-string-last ss))) false]
        [else (palindrome? (silly-string-middle ss))]))

;; Tests:
(check-expect (palindrome? empty) true)
(check-expect (palindrome? #\@) true)
(check-expect (palindrome? (sillify "racecar")) true)
(check-expect (palindrome? (sillify "Hanah")) false)
(check-expect (palindrome? (sillify "Koenigsegg")) false)