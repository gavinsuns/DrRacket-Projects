;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 09, Problem 1
;; **********************************************
;;

;; (flip-case lstr) Produces lstr with its string attributes adjusted.
;; Examples:
(check-expect (flip-case '("Mr" "Goose!")) '("Mr" "Goose!"))
(check-expect
(flip-case '("OnLiNE" "ClAsSEs" "ArE" "sOo" "mUcH" "FuN!"))
'("OnLiNE" "ClAsSEs" "are" "SOO" "MUCH" "fun!"))

;; flip-case: (listof Str) -> (listof Str)
(define (flip-case lstr)
  (local [;; (flip lstr str1 str2) Produces lstr with each of its string 
          ;; attributes adjusted, given str1, which is the element right before
          ;; the current element, and given str2, which is the element 2 spots 
          ;; before the current element.
          ;; flip: (listof Str) Str Str -> (listof Str)
          (define (flip lstr str1 str2)
            (cond [(empty? lstr) empty]
                  [(or (empty? str1) (empty? str2))
                   (cons (first lstr) (flip (rest lstr) (first lstr) str1))]
                  [(even? (+ (length (string->list str1))
                             (length (string->list str2))))
                   (cons (string-upcase (first lstr))
                         (flip (rest lstr) (first lstr) str1))]
                  [else (cons (string-downcase (first lstr))
                              (flip (rest lstr) (first lstr) str1))]))]
    (flip lstr empty empty)))

;; Tests:
(check-expect (flip-case '()) '())
(check-expect (flip-case '("OnLiNE")) '("OnLiNE"))
(check-expect (flip-case '("online" "classes" "are" "SOO"))
              '("online" "classes" "are" "SOO"))
(check-expect (flip-case '("online" "classes" "ARE" "soo"))
              '("online" "classes" "are" "SOO"))


;; (function-go-round fn-list data-list) Produces a new list, where each element
;; of fn-list is applied to the element in data-list with the corresponding
;; position.
;; Examples:
(check-expect
(function-go-round (list string-length string-upcase
(lambda (x) (string-append x "!!")))
'("joy" "anger" "disgust" "sadness" "fear"))
'(3 "ANGER" "disgust!!" 7 "FEAR"))
(check-expect
(function-go-round
(list even? odd? add1 (lambda (x) (> 3 x)) even?) '(8 9 2 1))
(list true true 3 true))

;; function-go-around: (ne-listof (X -> Y)) (listof X) -> (listof Y)
(define (function-go-round fn-list data-list)
  (local [;; (fn-go fn-list data-list full-fn-list) Produces a new list, where
          ;; each element of fn-list is applied to the element in data-list
          ;; with the corresponding position, given fn-full-list stores the
          ;; original list of functions.
          ;; fn-go: (ne-listof (X -> Y)) (listof X) (ne-listof (X -> Y)) ->
          ;; (listof Y)
          (define (fn-go fn-list data-list full-fn-list)
            (cond [(empty? data-list) empty]
                  [(empty? (rest fn-list))
                   (cons ((first fn-list) (first data-list))
                         (fn-go full-fn-list (rest data-list)
                                full-fn-list))]
                  [else (cons ((first fn-list) (first data-list))
                              (fn-go (rest fn-list) (rest data-list)
                                     full-fn-list))]))]
    (fn-go fn-list data-list fn-list)))

;; Tests:
(check-expect (function-go-round (list even? odd? add1 even?) '()) '())
(check-expect
(function-go-round
(list even? odd? add1 even?) '(8))
(list true))
(check-expect
(function-go-round
(list even? odd? add1 even?) '(8 9 2 1))
(list true true 3 false))
(check-expect
(function-go-round
(list even? odd? add1 even?) '(8 9 2 1 4 3 4))
(list true true 3 false true true 5))