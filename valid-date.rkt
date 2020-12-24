;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 01, Problem 5
;; **********************************************
;;

;; (valid-date? x) Determines if the date x is a valid date or not.
;; Examples:
(check-expect (valid-date? 21231) true)
(check-expect (valid-date? 20023210) false)

;; valid-date?: Nat -> Bool
(define (valid-date? x) (cond [(and (> x 17520902) (< x 17520914)) false]
                              [(and (valid-month? (month x)) (valid-day? x)) true]
                              [else false]))

;; Tests:
(check-expect (valid-date? 0) false)
(check-expect (valid-date? 101) true)
(check-expect (valid-date? 324140300) false)
(check-expect (valid-date? 24230014) false)
(check-expect (valid-date? 20000435) false)
(check-expect (valid-date? 23040556) false)
(check-expect (valid-date? 23480515) true)
(check-expect (valid-date? 17520902) true)
(check-expect (valid-date? 17520903) false)
(check-expect (valid-date? 17520913) false)
(check-expect (valid-date? 17520914) true)
(check-expect (valid-date? 20000229) true)
(check-expect (valid-date? 20010229) false)
(check-expect (valid-date? 20340531) true)
(check-expect (valid-date? 20380732) false)
(check-expect (valid-date? 20930430) true)
(check-expect (valid-date? 20850631) false)


;; (leap-year? x) Determines if year x is a leap year or not.
;; Examples:
(check-expect (leap-year? 2020) true)
(check-expect (leap-year? 123) false)

;; leap-year?: Nat -> Bool
(define (leap-year? x) (cond [(integer? (/ x 400)) true]
                             [(integer? (/ x 100)) false]
                             [(integer? (/ x 4)) true]
                             [else false]))

;; Tests:
(check-expect (leap-year? 1900) false)
(check-expect (leap-year? 2000) true)
(check-expect (leap-year? 124) true)
(check-expect (leap-year? 1235) false)


;; (year x) Produces the year of date x.
;;  Example:
(check-expect (year 200212) 20)

;; year: Nat -> Nat
(define (year x) (floor (/ x 10000)))


;; (month x) Produces the month of date x.
;; Example:
(check-expect (month 20001230) 12)

;; month: Nat -> Nat
(define (month x) (floor (/ (remainder x 10000) 100)))


;; (day x) Produces the day of date x.
;; Examples:
(check-expect (day 13451030) 30)

;; day: Nat -> Nat
(define (day x) (remainder x 100))


;; (valid-month? x) Determines if x is a valid month.
;; Example:
(check-expect (valid-month? 35) false)

;; valid-month?: Nat -> Bool
(define (valid-month? x) (and (<= x 12) (> x 0)))


;; (valid-day? x) dtermines if the day of date x is a valid day for the specific month and year.
;; Example:
(check-expect (valid-day? 3246346) false)

;; valid-day?: Nat -> Bool
(define (valid-day? x) (cond [(= (day x) 0) false]
                       [(or (= (month x) 1) (= (month x) 3) (= (month x) 5) (= (month x) 7)
                            (= (month x) 8) (= (month x) 10) (= (month x) 12)) (<= (day x) 31)]
                       [(or (= (month x) 4) (= (month x) 6) (= (month x) 9) (= (month x) 11)) (<= (day x) 30)]
                       [(and (leap-year? (year x)) (= (month x) 2)) (<= (day x) 29)]
                       [(and (false? (leap-year? (year x))) (= (month x) 2)) (<= (day x) 28)]
                       [else false]))
                               
                               

                               


                               