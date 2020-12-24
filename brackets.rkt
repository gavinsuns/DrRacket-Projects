;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 09, Problem 2
;; **********************************************
;;

;; (balanced? str) Determines if the brackets in str are balanced.
;; Examples:
(check-expect (balanced? "(<)>") false)
(check-expect (balanced? "((<>[])<>)[]") true)

;; balanced?: Str -> Bool
;; Requires: str contains only the characters #/( #/) #/[ #/] #/< #/>
(define (balanced? str)
  (local [;; (bal? lstr stk) Determines if the braackets in lstr are
          ;; balanced, given stack stk, which stores open brackets.
          (define (bal? lstr stk)
            (cond [(empty? lstr) true]
                  [(or (char=? (first lstr) #\()
                       (char=? (first lstr) #\<)
                       (char=? (first lstr) #\[))
                   (bal? (rest lstr) (cons (first lstr) stk))]
                  [(and (or (char=? (first lstr) #\))
                            (char=? (first lstr) #\>)
                            (char=? (first lstr) #\])) (empty? stk))
                   false]
                  [(or (and (char=? (first lstr) #\)) (char=? #\( (first stk)))
                       (and (char=? (first lstr) #\>) (char=? #\< (first stk)))
                       (and (char=? (first lstr) #\]) (char=? #\[ (first stk))))
                   (bal? (rest lstr) (rest stk))]
                  [(or (and (char=? (first lstr) #\))
                            (not (char=? #\( (first stk))))
                       (and (char=? (first lstr) #\>)
                            (not (char=? #\< (first stk))))
                       (and (char=? (first lstr) #\])
                            (not (char=? #\[ (first stk)))))
                   false]
                  [else (bal? (rest lstr) stk)]))]
    (cond [(= (length (string->list str)) 1) false]
          [else (bal? (string->list str) empty)])))

;; Tests:
(check-expect (balanced? "") true)
(check-expect (balanced? "(") false)
(check-expect (balanced? "(]") false)
(check-expect (balanced? "]()[") false)
(check-expect (balanced? "[<]>") false)
(check-expect (balanced? "<aaaaaa(>>") false)