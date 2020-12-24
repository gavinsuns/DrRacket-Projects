;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 08, Problem Bonus
;; **********************************************
;;

(define (subsets1 lon)
  (foldl (λ (x rorr) (append rorr (map (λ (y) (cons x y)) rorr))) '(()) lon))


(define (subsets2 lon)
  (foldl (λ (x rorr) (append rorr (map (λ (y) (cons x y)) rorr))) '(()) lon))

(define (subsets3 lon)
  (foldl (λ (x rorr) (append rorr (map (λ (y) (cons x y)) rorr))) '(()) lon))