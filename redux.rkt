;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 08, Problem 2
;; **********************************************
;;

;; 2a)
;; (parity x) Produces the parity of a binary string.
;; Examples:
(check-expect (parity "10101001") 'even)
(check-expect (parity "10000000") 'odd)

;; parity: Str -> Sym
;; Requires: each of the contained charcters in x is either #\1 or #\0. 
(define (parity x)
  (local [(define count-1
            (length (filter (lambda (x) (char=? x #\1)) (string->list x))))]
    (cond [(even? count-1) 'even]
          [(odd? count-1) 'odd])))

; Tests:
(check-expect (parity "") 'even)
(check-expect (parity "101010110") 'odd)
(check-expect (parity "11111") 'odd)
(check-expect (parity "00000") 'even)


;; 2b)
;; (replace-word str1 str2 lst) Produces a new list where all occurences of str1
;; have been replaced by str2 in list lst.
;; Examples:
(check-expect
 (replace-word "exam" "assessment"
               (cons "content"
                     (cons "exam"
                           (cons "assignment" empty))))
 (cons "content" (cons "assessment" (cons "assignment" empty))))
(check-expect
 (replace-word "content" "hi"
               (cons "content"
                     (cons "exam"
                           (cons "assignment" empty))))
 (cons "hi" (cons "exam" (cons "assignment" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)
(define (replace-word str1 str2 lst)
  (foldr (lambda (x rorr) (cond [(string=? x str1) (cons str2 rorr)]
                                [else (cons x rorr)])) empty lst))

;; Tests:
(check-expect
 (replace-word "exam" "assessment" empty) empty)
(check-expect
 (replace-word "exam" "assessment" (list "exam")) (list "assessment"))
(check-expect
 (replace-word "hi" "he" (list "he" "he" "hi")) (list "he" "he" "he")) 
 (check-expect
  (replace-word "exam" "assessment"
                (list "content" "exam" "exam"))
  (list "content" "assessment" "assessment"))


 ;; 2c)
 ;; (all-factors n) Produces all factors of n in a list in ascending order.
 ;; Examples:
 (check-expect
  (all-factors 30)
  (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15
                                                         empty))))))))
 (check-expect
  (all-factors 6)
  (cons 1 (cons 2 (cons 3 empty))))

 ;; all-factors: Nat -> (listof Nat)
 ;; Requires:
 (define (all-factors n)
   (cond [(zero? n) empty]
         [else (filter (lambda (x) (integer? (/ n x)))
           (rest (build-list n (lambda (x) x))))]))

 ;; Tests:
 (check-expect (all-factors 23) (cons 1 empty))
 (check-expect (all-factors 1) empty)
(check-expect (all-factors 0) empty)
 (check-expect (all-factors 10) (cons 1 (cons 2 (cons 5 empty))))


 ;; 2d)
 ;; (mean-relative x) Produces whether each element in list x is on the mean,
 ;; below-mean, or above-mean.
 ;; Examples:
 (check-expect (mean-relative (cons 5 (cons 7 (cons 9 (cons 12 empty)))))
               (cons 'below-mean
                     (cons 'below-mean
                           (cons 'above-mean (cons 'above-mean empty)))))
 (check-expect (mean-relative (cons 5 (cons 5 empty)))
               (cons 'mean (cons 'mean empty)))

 ;; mean-realtive: (listof Num) -> (listof Sym)
 (define (mean-relative x)
   (cond [(empty? x) empty]
         [else (local [(define mean (/ (foldl + 0 x) (length x)))]
                 (foldr (lambda (x rorr)
                          (cond [(= x mean) (cons 'mean rorr)]
                                [(> x mean) (cons 'above-mean rorr)]
                                [else (cons 'below-mean rorr)])) empty x))]))

 ;; Tests:
 (check-expect (mean-relative empty) empty)
 (check-expect (mean-relative (cons 5 empty)) (cons 'mean empty))
 (check-expect (mean-relative (cons 5 (cons 7 empty)))
               (cons 'below-mean (cons 'above-mean empty)))
 (check-expect (mean-relative (cons 6 (cons 2 (cons 7 (cons 5 empty)))))
               (cons 'above-mean (cons 'below-mean
                                       (cons 'above-mean(cons 'mean empty)))))