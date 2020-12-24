;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 04, Problem 3 & 4
;; **********************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;; (get-x p) Produces the x-coordinate of point p.
;; Example:
(check-expect (get-x (list 4 5)) 4)

;; get-x: Point -> Num
(define (get-x point) (first point))


;; (get-y p) Produces the y-coordinate of point p.
;; Example:
(check-expect (get-y (list 4 5)) 5)

;; get-y: Point -> Num
(define (get-y point) (second point))


;; (translate-gesture g x-offset y-offset) Produces gesture g with each point
;; offset by x-offset and y-offset.
;; Example:
(check-expect (translate-gesture (list (list 4 5) (list 2 3.5)) 10 20)
              (list (list 14 25) (list 12 23.5)))

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture g x-offset y-offset)
  (cond [(empty? g) empty]
        [else (cons (list (+ (get-x (first g)) x-offset)
                          (+ (get-y (first g)) y-offset))
                    (translate-gesture (rest g) x-offset y-offset))]))


;; (scale-gesture g x-scale y-scale) Produces gesture g with each point scaled
;; by x-scale for the x-coordinate and y-scale for the y-coordinate.
;; Example:
(check-expect (scale-gesture (list (list 1 3) (list 2 4)) 10 20)
              (list (list 10 60) (list 20 80)))

;; scale-gesture: Gesture Num Num -> Gesture
;; Requires:
;; x-scale > 0
;; y-scale > 0
(define (scale-gesture g x-scale y-scale)
  (cond [(empty? g) empty]
        [else (cons (list (* (get-x (first g)) x-scale)
                          (* (get-y (first g)) y-scale))
                    (scale-gesture (rest g) x-scale y-scale))]))


;; (get-b-box g) Produces gesture g's BoundingBox.
;; Example:
(check-expect (get-b-box (list (list 1 10) (list 2 5)))
              (list (list 1 5) (list 2 10)))

;; get-b-box: Gesture -> BB
;; Requires: gesture is non-empty
(define (get-b-box g) (list (min-point g (get-x (first g)) (get-y (first g)))
                              (max-point g (get-x (first g)) (get-y (first g)))))


;; (min-point g x y) Produces a point that contains the smallest x-value
;; and y-value of gesture g.
;; Example:
(check-expect (min-point (list (list 1 10) (list 2 5)) 1 10) (list 1 5))

;; min-point: Gesture Num Num -> Point
;; Requires: gesture is non-empty
(define (min-point g x y) (cond [(empty? g) (list x y)]
                                  [else (min-point (rest g) (min (get-x (first g)) x)
                                                   (min (get-y (first g)) y))]))


;; (max-point g x y) Produces a point that contains the largest x-value
;; and y-value of gesture g and the point (x, y).
;; Example:
(check-expect (max-point (list (list 1 10) (list 2 5)) 1 10) (list 2 10))

;; max-point: Gesture Num Num -> Point
;; Requires: gesture is non-empty
(define (max-point g x y) (cond [(empty? g) (list x y)]
                                  [else (max-point (rest g) (max (get-x (first g)) x)
                                                   (max (get-y (first g)) y))]))
                                  

;; 3b)
;; Full design recipe required.

;; (gesture-length g) Produces the length of gesture g.
;; Examples:
(check-within (gesture-length (list (list 2 3) (list 4 5))) 2.83 0.01)
(check-within (gesture-length (list (list 2 4) (list 3 4) (list 2 7))) 4.16 0.01)

;; gesture-length: Gesture -> Num
(define (gesture-length g) (cond [(or (empty? g) (empty? (rest g))) 0]
                                   [else (+ (distance (first g) (second g))
                                            (gesture-length (rest g)))]))

;; Tests:
(check-within (gesture-length empty) 0 0.01)
(check-within (gesture-length (list (list 2 4))) 0 0.01)
(check-within (gesture-length (list (list -4 5) (list 10 -10) (list -9 5))) 44.73 0.01)


;; (distance p1 p2) Produces the distance between p1 and p2.
;; Example:
(check-within (distance (list 2 3) (list 4 5)) 2.83 0.01)

;; distance: Point Point -> Num
(define (distance p1 p2) (sqrt (+ (sqr (- (get-x p2) (get-x p1)))
                                  (sqr (- (get-y p2) (get-y p1))))))


(define mygest (list (list 100 0) (list 200 100) (list 100 200)
(list 0 100) (list 100 50)))


;; (get-points g lst) Produces a gesture that maps lst indexes to gesture g points.
;; Examples:
(check-expect (get-points mygest (list 0 0 2 4 4))
(list (list 100 0) (list 100 0) (list 100 200) (list 100 50)
(list 100 50)))
(check-expect (get-points mygest (list 0 0 2))
(list (list 100 0) (list 100 0) (list 100 200)))

;; get-points: Gesture (listof Nat) -> Gesture
;; Requires:
;; lst must be a non-decreasing list of Nat
;; Elements in lst must be in the range
;; [0, (number of points in g) - 1]
(define (get-points g lst) (cond [(or (empty? g) (empty? lst)) empty]
                                   [else (cons (first (get-helper g (first lst)))
                                               (get-points g (rest lst)))]))

;; Tests:
(check-expect (get-points mygest empty) empty)
(check-expect (get-points empty (list 0)) empty)
(check-expect (get-points mygest (list 0)) (list (list 100 0)))
(check-expect (get-points mygest (list 0 0 1 2 3 4))
              (list (list 100 0) (list 100 0) (list 200 100) (list 100 200)
                    (list 0 100) (list 100 50)))


;; (get-helper g x) Produces the elements of gesture g from index x
;; to the index of the last element.
;; Example:
(check-expect (get-helper (list (list 2 3) (list 4 5)) 1) (list (list 4 5)))

;; get-helper: Gesture Nat -> Gesture
;; Requires: g is non-empty
;; x must be in the range [0, (length of g) - 1]
(define (get-helper g x) (cond [(= x 0) g]
                                 [else (rest (get-helper g (sub1 x)))]))


;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))


;; five-sample: Gesture -> Gesture
;; Requires: gesture is non-empty
(define (five-sample gesture) (list (first gesture)
                              (first (get-helper gesture (floor (* 0.25 (length gesture)))))
                              (first (get-helper gesture (floor (* 0.5 (length gesture)))))
                              (first (get-helper gesture (floor (* 0.75 (length gesture)))))
                              (first (get-helper gesture (- (length gesture) 1)))))
                                             

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))


;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale).
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; Requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (move-and-scale-recurse gesture x-scale y-scale
               (get-x (min-point gesture
                          (get-x (first gesture))
                          (get-y (first gesture))))
               (get-y (min-point gesture
                          (get-x (first gesture))
                          (get-y (first gesture)))))) 

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))


;;(move-and-scale-recurse gesture x-scale y-scale x y) moves gesture by (x, y) and
;; scales it by (x-scale)x(y-scale).
;; Examples and tests: see wrapper function move-and-scale

;; move-and-scale-recurse Gesture Num Num Num Num -> Gesture
;; Requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale-recurse gesture x-scale y-scale x y)
  (scale-gesture (translate-gesture gesture (- 0 x) (- 0 y)) x-scale y-scale))
  

;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size.
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; Requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (normalize-gesture-recurse gesture (get-b-box gesture)))


;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)


;; (normalize-gesture-recurse gesture b-box) normalizes gesture to (0,0), using  bounding box b-box,
;; and a standard size.
;; Examples and tests: see wrapper function normalize-gesture

;; normalize-gesture-recurse: Gesture BoundingBox -> Gesture
;; Requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture-recurse gesture b-box)
  (cond [(< (- (get-y (second b-box)) (get-y (first b-box))) min-height)
         (move-and-scale-recurse gesture (/ 200 (- (get-x (second b-box)) (get-x (first b-box))))
          1 (get-x (first b-box)) (get-y (first b-box)))]
        [(< (- (get-x (second b-box)) (get-x (first b-box))) min-width)
         (move-and-scale-recurse gesture 1 (/ 200 (- (get-y (second b-box)) (get-y (first b-box))))
          (get-x (first b-box)) (get-y (first b-box)))]
  [else (move-and-scale-recurse gesture
   (/ 200 (- (get-x (second b-box)) (get-x (first b-box))))
   (/ 200 (- (get-y (second b-box)) (get-y (first b-box))))
   (get-x (first b-box)) (get-y (first b-box)))]))


;; 3civ)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with 5 points.
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
              16.16 0.01)
(check-within (geometric-5match
               (list (list 0 0) (list 100 30))
               (list (list 0 0) (list 100 29)))
              102.6 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; Requires: gesture1 and gesture2 are each not both vertical and horizontal
;; gesture1 and gesture2 are non-empty
(define (geometric-5match gesture1 gesture2) (/ (geometric-recurse
                                              (normalize-gesture (five-sample gesture1))
                                              (normalize-gesture (five-sample gesture2))) 5))

;; Tests:
(check-within (geometric-5match
               (list (list 0 0) (list 100 30))
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70)))
              37.71 0.01)
(check-within (geometric-5match
               (list (list 10 20) (list 50 90) (list 52 20) (list 42 70) (list 80 53) (list 30 200))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45) (list 300 20)))
              135.52 0.01)
(check-within (geometric-5match
               (list (list 10 20) (list 50 90) (list 52 20))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45) (list 300 20)))
              109.22 0.01)
(check-within (geometric-5match
               (list (list 10 20) (list 50 90) (list 52 20) (list 42 70) (list 80 53))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45)))
              102.53 0.01)


;; (geometric-recurse gesture1 gesture2) Produces the sum of all the distances between same index
;; points in gesture1 and gesture2.
;; Examples and tests: see wrapper function geometric-5match.

;; geometric-recurse: Gesture Gesture -> Num
;; Requires: gesture1 and gesture2 are each not both vertical and horizontal
;; length of gesture1 = length of gesture2
;; gesture1 and gesture2 are non-empty
(define (geometric-recurse gesture1 gesture2) (cond [(empty? (rest gesture1))
                                                    (distance (first gesture1)
                                                              (first gesture2))]
                                                   [else (+ (distance (first gesture1)
                                                                      (first gesture2))
                                                            (geometric-recurse (rest gesture1)
                                                                              (rest gesture2)))]))


;; 3cv)

;; (five-point-rec candidate template-library) produces the symbol in
;; template-library closest to candidate.
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec: Gesture TL -> Sym
;; Requires: candidate is not both vertical and horizontal
;; candidate is non-empty
(define (five-point-rec candidate template-library)
  (cond [(empty? (rest template-library)) (first (first template-library))]
        [(> (geometric-5match candidate (second (first template-library)))
                 (geometric-5match candidate (second (second template-library))))
              (five-point-rec candidate (rest template-library))]
        [else (five-point-rec candidate (cons (first template-library)
                                                     (rest (rest template-library))))]))

;; Tests
(check-expect (five-point-rec testa templates) 'a)
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testt templates) 't)
(check-expect (five-point-rec testy templates) 'y)


;; 3d)

;; (sub-sample gesture k) Produces a sampling of gesture, gesture with k points,
;; the first, n/(k - 1)th, 2n/(k - 1)th,..., and last point.
;; Examples:
(check-expect (sub-sample (list (list 10 20) (list 50 90) (list 52 20)
                                (list 42 70) (list 80 53)) 4)
              (list (list 10 20) (list 50 90) (list 42 70) (list 80 53)))
(check-expect (sub-sample (list (list 10 20) (list 50 90) (list 52 20)
                                (list 42 70) (list 80 53)) 6)
              (list (list 10 20) (list 50 90) (list 52 20) (list 42 70)
                    (list 80 53) (list 80 53)))

;; sub-sample: Gesture Nat -> Gesture
;; Requires: gesture is non-empty
;; k > 2
(define (sub-sample gesture k) (sub-sample-recurse gesture k 0))

;; Tests:
(check-expect (sub-sample (list (list 10 20) (list 50 90)
                                (list 52 20) (list 42 70)
                                (list 80 53)) 5)
              (list (list 10 20) (list 50 90) (list 52 20)
                    (list 42 70) (list 80 53)))
(check-expect (sub-sample (list (list 20 30)) 10)
              (list (list 20 30) (list 20 30) (list 20 30)
                    (list 20 30) (list 20 30) (list 20 30)
                    (list 20 30) (list 20 30) (list 20 30)
                    (list 20 30)))
(check-expect (sub-sample (list (list 10 20) (list 50 90)
                                (list 52 20) (list 42 70)
                                (list 80 53)) 3)
              (list (list 10 20) (list 52 20) (list 80 53)))
(check-expect (sub-sample (list (list 145 314) (list 144 314)
                                (list 142 314) (list 139 314)
                                (list 136 314) (list 131 314)
                                (list 125 314) (list 120 314)
                                (list 112 310) (list 107 309)
                                (list 105 306) (list 100 304)
                                (list 94 301) (list 90 296)
                                (list 83 279) (list 80 277)
                                (list 80 276)) 12)
              (list (list 145 314) (list 144 314) (list 139 314)
                    (list 136 314) (list 125 314) (list 120 314)
                    (list 107 309) (list 105 306) (list 94 301)
                    (list 90 296) (list 80 277) (list 80 276)))


;; (sub-sample-recurse gesture k x) Produces a sampling of gesture, gesture with k points,
;; the first, xn/(k - 1)th,..., and last point:
;; x is the counter that counts up to (k - 1) to cons last point and stop function.
;; Examples and tests: see wrapper function sub-sample.

;; sub-sample-recurse: Gesture Nat -> Gesture
;; Requires: gesture is non-empty
;; k > 2
(define (sub-sample-recurse gesture k x)
  (cond [(= (add1 x) k) (cons (first (get-helper gesture (- (length gesture) 1))) empty)]
        [(zero? x) (cons (first gesture) (sub-sample-recurse gesture k (add1 x)))]
        [else (cons (first (get-helper gesture (floor (* (/ x (sub1 k)) (length gesture)))))
                    (sub-sample-recurse gesture k (add1 x)))]))


;; (geometric-match gesture1 gesture2 k) produces the average distance between
;;  points in gesture1 and gesture2 after sub-sampling them with k points.
;; Examples:
(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 4)
              13.47 0.01)
(check-within (geometric-match
               (list (list 0 0) (list 100 30))
               (list (list 0 0) (list 100 29)) 6)
              85.50 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
;; Requires: gesture1 and gesture2 are each not both vertical and horizontal
;; gesture1 and gesture2 are non-empty
;; k > 2
(define (geometric-match gesture1 gesture2 k)
  (/ (geometric-recurse (normalize-gesture (sub-sample gesture1 k))
                       (normalize-gesture (sub-sample gesture2 k))) k))

;; Tests:
(check-within (geometric-match
               (list (list 0 0) (list 100 30))
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70)) 3)
              31.43 0.01)
(check-within (geometric-match
               (list (list 10 20) (list 50 90) (list 52 20) (list 42 70) (list 80 53) (list 30 200))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45) (list 300 20)) 10)
              139.40 0.01)
(check-within (geometric-match
               (list (list 10 20) (list 50 90) (list 52 20))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45) (list 300 20)) 19)
              118.74 0.01)
(check-within (geometric-match
               (list (list 10 20) (list 50 90) (list 52 20) (list 42 70) (list 80 53))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45)) 50)
              102.53 0.01)


;; (k-point-rec candidate template-library k) produces the symbol in template-library
;; closest to candidate after normalizing and sub-sampling both by k.
;; Examples:
(check-expect (k-point-rec testd templates 5) 'd)
(check-expect (k-point-rec testk templates 10) 'k)

;; k-point-rec: Gesture TL Nat -> Sym
;; Requires: candidate is not both vertical and horizontal
;; candidate is non-empty
;; k > 2
(define (k-point-rec candidate template-library k)
  (cond [(empty? (rest template-library)) (first (first template-library))]
        [(> (geometric-match candidate (second (first template-library)) k)
                 (geometric-match candidate (second (second template-library)) k))
              (k-point-rec candidate (rest template-library) k)]
        [else (k-point-rec candidate (cons (first template-library)
                                                     (rest (rest template-library))) k)]))

;; Tests:
(check-expect (k-point-rec testa templates 3) 'a)
(check-expect (k-point-rec tests templates 15) 's)
(check-expect (k-point-rec testt templates 20) 't)
(check-expect (k-point-rec testy templates 30) 'y)


;; 4)

(define perfect-square (list (list 50 100) (list 250 100)
(list 250 300) (list 50 300) (list 50 100)))


;; (spatial-sub-sample gesture k) Produces a new gesture that has k points that are equally
;; spaced along the path of the original gesture, gesture.
;; Examples:
(check-within (spatial-sub-sample perfect-square 4)
(list (list 50 100) (list 250 166.66) (list 116.66 300)
(list 50 100)) 0.01)
(check-within (spatial-sub-sample perfect-square 3)
(list (list 50 100) (list 250 300) (list 50 100)) 0.01)

;; spatial-sub-sample: Gesture Nat -> Gesture
;; Requires:
;; gesture is non-empty
;; k > 2
(define (spatial-sub-sample gesture k)
  (cond [(empty? (rest gesture)) (build-same (first gesture) k)]
        [else (spatial-sub-sample-recurse gesture k (/ (gesture-length gesture) (sub1 k)) 0)]))

;; Tests:
(check-within (spatial-sub-sample (list (list 20 30)) 3)
              (list (list 20 30) (list 20 30) (list 20 30)) 0.01)
(check-within (spatial-sub-sample perfect-square 6)
              (list (list 50 100) (list 210 100) (list 250 220) (list 170 300)
                    (list 50 260) (list 50 100)) 0.01)
(check-within (spatial-sub-sample (list (list 50 50) (list 250 250)) 5)
              (list (list 50 50) (list 100 100)
                    (list 150 150) (list 200 200)
                    (list 250 250)) 0.01)
(check-within (spatial-sub-sample (list (list 50 50) (list 100 123)) 10)
              (list (list 50 50) (list 55.55 58.11) (list 61.11 66.22)
                    (list 66.66 74.33) (list 72.22 82.44)
                    (list 77.77 90.55) (list 83.33 98.66)
                    (list 88.88 106.77) (list 94.44 114.88)
                    (list 100 123)) 0.01)


;; (spatial-sub-sample-recurse gesture k d x) Produces a new gesture that has k points that are
;; equally spaced by d along the path of the original gesture, gesture:
;; x is the counter that counts up to (k - 1) in order to tell the function to print
;; last point and stop.
;; Examples and tests: see wrapper function spatial-sub-sample

;; spatial-sub-sample-recurse: Gesture Nat Nat Nat -> Gesture
;; Requires:
;; gesture contains at least 2 Points
;; x < k
;; k > 2
(define (spatial-sub-sample-recurse gesture k d x)
  (cond [(= (add1 x) k) (cons (first (get-helper gesture (- (length gesture) 1))) empty)]
        [(zero? x) (cons (first gesture) (spatial-sub-sample-recurse gesture k d (add1 x)))]
        [(>= (distance (first gesture) (second gesture)) d)
         (cons (new-point-helper (first gesture) (second gesture) d)
               (spatial-sub-sample-recurse
                (cons (new-point-helper (first gesture) (second gesture) d)
                      (rest gesture)) k d (add1 x)))]
        [else (cons (new-point-helper (first (get-helper gesture (second (kill gesture d 0 0))))
                                      (second (get-helper gesture (second (kill gesture d 0 0))))
                                      (- d (first (kill gesture d 0 0))))
                    (spatial-sub-sample-recurse
                     (cons (new-point-helper
                            (first (get-helper gesture (second (kill gesture d 0 0))))
                            (second (get-helper gesture (second (kill gesture d 0 0))))
                            (- d (first (kill gesture d 0 0))))
                           (get-helper gesture (add1 (second (kill gesture d 0 0))))) k d (add1 x)))]))


;; (new-point-helper p1 p2 d) Produces the new point that is inbetween
;; p1 and p2 and is d units from p1.
;; Example:
(check-within (new-point-helper (list 1 1) (list 100 100) 40) (list 29.28 29.28) 0.01)

;; new-point-helper: Point Point Num -> Point
;; Requires:
;; d >= 0
;; d <= distance from p1 to p2
(define (new-point-helper p1 p2 d)
  (list (+ (get-x p1)
           (* (/ d (distance p1 p2))
              (- (get-x p2) (get-x p1))))
        (+ (get-y p1)
           (* (/ d (distance p1 p2))
              (- (get-y p2) (get-y p1))))))


;; (kill gesture d close-to-d index) Produces the max length of elements in gesture that total to less than
;; distance d, represented by close-to-d, and the index at which this occurs, represented by index.
;; Example:
(check-within (kill (list (list 55 130) (list 251 104)
(list 252 340)) 400 0 0) (list 197.72 1) 0.01)

;; kill: Gesture Num Nat Nat -> (list Num Nat)
;; Requires: gesture is not both vertical and horizontal
;; gesture is non-empty
(define (kill gesture d close-to-d index)
  (cond [(> (+ close-to-d (distance (first (get-helper gesture index))
                   (second (get-helper gesture index)))) d) (list close-to-d index)]
        [else (kill gesture d (+ close-to-d (distance (first(get-helper gesture index))
                   (second (get-helper gesture  index)))) (add1 index))]))


;; (build-same p k) Produces a Gesture, where point p is repeated k times.
;; Example:
(check-expect (build-same (list 2 30) 4)
              (list (list 2 30) (list 2 30) (list 2 30) (list 2 30)))

;; build-same: Point Nat -> Gesture
;; Requires:
;; k > 2
(define (build-same element k) (cond [(= k 0) empty]
                                     [else (cons element (build-same element (sub1 k)))]))


;; (spatial-rec candidate template-library k) Produces the symbol in template-library
;; closest to candidate after spatial-sub-sampling both candidate and the symbol by k.
;; Examples:
(check-expect (spatial-rec testd templates 5) 'd)
(check-expect (spatial-rec testk templates 10) 'k)

;; spatial-rec: Gesture TL Nat -> Sym
;; Requires: candidate is not both vertical and horizontal
;; candidate is non-empty
;; k > 2
(define (spatial-rec candidate template-library k)
  (cond [(empty? (rest template-library)) (first (first template-library))]
        [(> (spatial-match candidate (second (first template-library)) k)
                 (spatial-match candidate (second (second template-library)) k))
              (spatial-rec candidate (rest template-library) k)]
        [else (spatial-rec candidate (cons (first template-library)
                                                     (rest (rest template-library))) k)]))

;; Tests:
(check-expect (spatial-rec testa templates 3) 'a)
(check-expect (spatial-rec tests templates 15) 's)
(check-expect (spatial-rec testt templates 20) 't)
(check-expect (spatial-rec testy templates 30) 'y)


;; (spatial-match gesture1 gesture2 k) produces the average distance between points
;; in gesture1 and gesture2 after spatial-sub-sampling them with k points.
;; Examples:
(check-within (spatial-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 4)
              0.00 0.01)
(check-within (spatial-match
               (list (list 0 0) (list 100 30))
               (list (list 0 0) (list 100 29)) 6)
              85.50 0.01)

;; spatial-match: Gesture Gesture Nat -> Num
;; Requires: gesture1 and gesture2 are each not both vertical and horizontal
;; gesture1 and gesture2 are non-empty
;; k > 2
(define (spatial-match gesture1 gesture2 k)
  (/ (geometric-recurse (normalize-gesture (spatial-sub-sample gesture1 k))
                       (normalize-gesture (spatial-sub-sample gesture2 k))) k))

;; Tests:
(check-within (spatial-match
               (list (list 0 0) (list 100 30))
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70)) 3)
              0.00 0.01)
(check-within (spatial-match
               (list (list 10 20) (list 50 90) (list 52 20) (list 42 70) (list 80 53) (list 30 200))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45) (list 300 20)) 10)
              105.60 0.01)
(check-within (spatial-match
               (list (list 10 20) (list 50 90) (list 52 20))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45) (list 300 20)) 19)
              80.53 0.01)
(check-within (spatial-match
               (list (list 10 20) (list 50 90) (list 52 20) (list 42 70) (list 80 53))
               (list (list 1 1) (list 15 24) (list 40 35) (list 47 59) (list 20 45)) 50)
              82.80 0.01)