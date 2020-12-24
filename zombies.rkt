;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname zombies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 10
;; **********************************************
;;

;; A Location is a Nat
;; A Town is a (listof (list Location (listof Location)))
;; Requires: Town represents a valid graph as defined in Module 16

;; A Horde is a (listof (list Location Nat))

(define waterloo '((0 (1 2 3))
                   (1 (2 3))
                   (2 (0 4))
                   (3 (1))
                   (4 (5))
                   (5 (3))))

(define markham '((0 (1 2))
                  (1 (2))
                  (2 (1))))

(define scarborough '((0 (0))))


;; i)
;; (infect town zombies) Produces a horde with zombies amount of zombies
;; at each location in town.
;; Examples:
(check-expect (infect waterloo 1000)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))
(check-expect (infect waterloo 500)
              (list (list 0 500) (list 1 500) (list 2 500)
                    (list 3 500) (list 4 500) (list 5 500)))

;; infect: Town Nat -> Horde
(define (infect town zombies)
  (cond [(empty? town) empty]
        [else (cons (list (first (first town)) zombies)
                    (infect (rest town) zombies))]))

;; Tests:
(check-expect (infect waterloo 0)
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))
(check-expect (infect markham 0)
              (list (list 0 0) (list 1 0) (list 2 0)))
(check-expect (infect scarborough 0)
              (list (list 0 0)))
(check-expect (infect scarborough 10)
              (list (list 0 10)))


;; ii)
;; (sink horde) Produces a list of 2 elements The first element is the total
;; number of zombies that sink into the earth; the second element is horde
;; after those zombies have sunken into the earth.
;; Examples:
(check-expect (sink (infect waterloo 1000))
              (list 300 (list (list 5 950) (list 4 950) (list 3 950)
                              (list 2 950) (list 1 950) (list 0 950))))
(check-expect (sink (infect waterloo 500))
              (list 150 (list (list 5 475) (list 4 475) (list 3 475)
                              (list 2 475) (list 1 475) (list 0 475))))

;; sink: Horde -> (list Nat Horde)
(define (sink horde)
  (local [(define (sinkz horde sinked new)
            (local [(define first-sink
                      (cond [(empty? horde) empty]
                            [else (round (* 0.05 (second (first horde))))]))]
              (cond [(empty? horde) (list sinked new)]
                    [else (sinkz (rest horde) (+ first-sink sinked)
                                 (cons (list (first (first horde))
                                             (- (second (first horde))
                                                first-sink))
                                       new))])))]
    (sinkz horde 0 empty)))

;; Tests:
(check-expect (sink (infect waterloo 0))
              (list 0 (list (list 5 0) (list 4 0) (list 3 0)
                            (list 2 0) (list 1 0) (list 0 0))))
(check-expect (sink (infect scarborough 0)) (list 0 (list (list 0 0))))
(check-expect (sink (infect markham 50))
              (list 6 (list (list 2 48) (list 1 48) (list 0 48))))
(check-expect (sink (infect scarborough 50)) (list 2 (list (list 0 48))))


;; iii)
;; (apportion zombies n) Produces a list of exactly n natural numbers.
;; The numbers in n must add up to zombies  and the difference between
;; any two numbers can't be greater than 1.
;; Examples:
(check-expect (apportion 49 2) (list 25 24))
(check-expect (apportion 32 5) (list 7 7 6 6 6))

;; apportion: Nat Nat -> (listof Nat)
;; Requires: n > 0
(define  (apportion zombies n)
  (local [(define split-n (round (/ zombies n)))
          (define (make-n zombies n)
            (cond [(zero? n) empty]
                  [(zero? zombies) (cons 0 (make-n zombies (sub1 n)))]
                  [else (cons 1 (make-n (sub1 zombies) (sub1 n)))]))
          (define (rest-apportion n x)
            (cond [(= n x) empty]
                  [else (cons split-n (rest-apportion n (add1 x)))]))
          (define (make-same x n)
            (cond [(zero? n) empty]
                  [else (cons x (make-same x (sub1 n)))]))]
    (cond [(> n zombies) (make-n zombies n)]
          [(= (* n split-n) zombies) (rest-apportion n 0)]
          [(> (* n split-n) zombies)
           (append (rest-apportion n (- (* n split-n) zombies))
                   (make-same (sub1 split-n) (- (* n split-n) zombies)))]
          [(< (* n split-n) zombies)
           (append (make-same (add1 split-n) (- zombies (* n split-n)))
                   (rest-apportion n (- zombies (* n split-n))))])))

;; Tests:
(check-expect (apportion 100 6) (list 17 17 17 17 16 16))
(check-expect (apportion 100 3) (list 34 33 33))
(check-expect (apportion 99 3) (list 33 33 33))
(check-expect (apportion 98 3) (list 33 33 32))
(check-expect (apportion 1 3) (list 1 0 0))
(check-expect (apportion 0 4) (list 0 0 0 0))


;; iv)
(define braaaaains (second (sink (infect waterloo 1000))))


;; (shamble town horde) Produces the horde that results from all the zombies in
;; horde infesting in town apportioning themselves into nearly equal groups and
;; shambling along the edges connecting the locations.
;; Examples:
(check-expect (shamble waterloo braaaaains)
              (list (list 0 475) (list 1 1267) (list 2 792)
                    (list 3 1741) (list 4 475) (list 5 950)))
(check-expect (shamble waterloo (infect waterloo 500))
              (list (list 0 250) (list 1 667) (list 2 417)
                    (list 3 916) (list 4 250) (list 5 500)))

;; shamble: Town Horde -> Horde
(define (shamble town horde)
  (local [(define (apportioned location horde-loc)
            (apportion (second horde-loc)
                       (length (second location))))
          (define (make-split location apport)
            (cond [(empty? location) empty]
                  [else (cons (list (first location) (first apport))
                              (make-split (rest location) (rest apport)))]))
          (define (find town horde)
            (cond [(= (first (first horde)) town) (first horde)]
                  [else (find town (rest horde))]))
          (define (split-total town)
            (cond [(empty? town) empty]
                  [else (append (make-split
                                 (second (first town))
                                 (apportioned (first town)
                                              (find (first (first town))
                                                    horde)))
                                (split-total (rest town)))]))
          (define (count-total loc split)
            (cond [(empty? split) 0]
                  [(= (first (first split)) loc)
                   (+ (second (first split)) (count-total loc (rest split)))]
                  [else (count-total loc (rest split))]))
          (define split (split-total town))
          (define (shambler town)
            (cond [(empty? town) empty]
                  [else (cons (list (first (first town))
                                    (count-total (first (first town))
                                                 split))
                              (shambler (rest town)))]))]
    (shambler town)))

;; Tests:
(check-expect (shamble waterloo (infect waterloo 0))
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))
(check-expect (shamble scarborough (infect scarborough 0))
              (list (list 0 0)))
(check-expect (shamble markham (infect markham 50))
              (list (list 0 0) (list 1 75) (list 2 75)))
(check-expect (shamble scarborough (infect scarborough 1000))
              (list (list 0 1000)))


;; v)
(define braaaaaaains (shamble waterloo braaaaains))


;; (rise zombies horde) Produces a new horde with zombies apportioned as equally
;; as possible between locations from the original horde.
;; Examples:
(check-expect (rise 300 braaaaaaains)
              (list (list 0 525) (list 1 1317) (list 2 842)
                    (list 3 1791) (list 4 525) (list 5 1000)))
(check-expect (rise 270 braaaaaaains)
              (list (list 0 520) (list 1 1312) (list 2 837)
                    (list 3 1786) (list 4 520) (list 5 995)))

;; rise: Nat Horde -> Horde
(define (rise zombies horde)
  (local [(define apportioned (apportion zombies
                                         (length horde)))
          (define (riser horde apport)
            (cond [(empty? horde) empty]
                  [else (cons (list (first (first horde))
                                    (+ (second (first horde)) (first apport)))
                              (riser (rest horde) (rest apport)))]))]
    (riser horde apportioned)))

;; Tests:
(check-expect (rise 0 (infect waterloo 1000))
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))
(check-expect (rise 0 (infect scarborough 0))
              (list (list 0 0)))
(check-expect (rise 100 (infect scarborough 1000))
              (list (list 0 1100)))
(check-expect (rise 200 (infect markham 400))
              (list (list 0 467) (list 1 467) (list 2 466)))


;; vi)
;; Produces a new horde after horde goes through a night in town.
;; Examples:
(check-expect (night waterloo (infect waterloo 1000))
              (list (list 0 525) (list 1 1317) (list 2 842)
                    (list 3 1791) (list 4 525) (list 5 1000)))
(check-expect (night waterloo (infect waterloo 500))
              (list (list 0 263) (list 1 659) (list 2 421)
                    (list 3 895) (list 4 262) (list 5 500)))

;; night: Town Horde -> Horde
(define (night town horde)
  (local [(define sinked (sink horde))]
    (rise (first sinked) (shamble town (second sinked)))))

;; Tests:
(check-expect (night waterloo (infect waterloo 0))
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))
(check-expect (night scarborough (infect scarborough 0))
              (list (list 0 0)))
(check-expect (night scarborough (infect scarborough 20))
              (list (list 0 20)))
(check-expect (night markham (infect markham 300))
              (list (list 0 15) (list 1 443) (list 2 442)))


;; (apocalypse town infection nights) Produces the horde after nights
;; amount of nights have passed in town, which started with infection
;; amount of zombies in each location.
;; Examples:
(check-expect (apocalypse waterloo 1000 3)
              (list (list 0 450) (list 1 1894) (list 2 1104)
                    (list 3 1625) (list 4 450) (list 5 477)))
(check-expect (apocalypse waterloo 1000 7)
              (list (list 0 544) (list 1 1747) (list 2 1016)
                    (list 3 1576) (list 4 543) (list 5 574)))

;; apocalypse: Town Nat Nat -> Horde
(define (apocalypse town infection nights)
  (local [(define initial-horde (infect town infection))
          (define (apoc nights horde)
            (cond [(zero? nights) horde]
                  [else (apoc (sub1 nights) (night town horde))]))]
    (apoc nights initial-horde)))

;; Tests:
(check-expect (apocalypse waterloo 0 7)
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))
(check-expect (apocalypse scarborough 0 10)
              (list (list 0 0)))
(check-expect (apocalypse markham 1000 10)
              (list (list 0 50) (list 1 1475) (list 2 1475)))
(check-expect (apocalypse scarborough 100 10)
              (list (list 0 100)))