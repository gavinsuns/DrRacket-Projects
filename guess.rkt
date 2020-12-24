;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 07, Problem 2
;; **********************************************
;;

;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

(require "animals.rkt")

(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

;; a)
;; (collect-attributes examples) Produces a list of all attributes contained in
;; examples with no duplicates.
;; Examples:
(check-expect (collect-attributes seen)
              (list 'medium 'flies 'swims 'large 'angry 'small))
(check-expect (collect-attributes
               (list (list 'squirrel 'small 'angry)
                     (list 'goose 'large 'swims 'flies 'angry)))
              (list 'flies 'swims 'large 'angry 'small))

;; (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local [(define (check-example example attributes examples)
            (cond [(empty? example) (collect (rest examples) attributes)]
                  [(member? (first example) attributes)
                   (check-example (rest example) attributes examples)]
                  [else
                   (check-example
                    (rest example)
                    (cons (first example) attributes) examples)]))
          (define (collect examples attributes)
            (cond [(empty? examples) attributes]
                  [else (check-example (rest (first examples))
                                       attributes examples)]))]
    (collect examples empty)))

;; Tests:
(check-expect (collect-attributes empty) empty)
(check-expect (collect-attributes (list (list 'squirrel 'small 'angry)))
              (list 'angry 'small))
(check-expect (collect-attributes (list (list 'squirrel))) empty)


;; b)
;; (split-examples examples symbol) Produces a list of two lists of examples,
;; with the first containing the examples from examples containing symbol and
;; the second containing the examples from examples not containing the symbol.
;; Examples:
(check-expect (split-examples seen 'goose)
              (list
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'squirrel 'small 'angry))))
(check-expect (split-examples seen 'small)
              (list
               (list
                (list 'squirrel 'small 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))))

;; split-examples: (listof Example) Sym ->
;; (list (listof Example) (listof Example))
(define (split-examples examples symbol)
  (local [(define (split examples symbol l1 l2)
            (cond [(empty? examples) (list l1 l2)]
                  [(member? symbol (first examples))
                   (split (rest examples) symbol
                          (cons (first examples) l1) l2)]
                  [else (split (rest examples) symbol
                               l1 (cons (first examples) l2))]))]
    (split examples symbol empty empty)))

;; Tests:
(check-expect (split-examples empty 'goose) (list empty empty))
(check-expect (split-examples (list (list 'goose 'medium 'fies)) 'goose)
              (list (list (list 'goose 'medium 'fies)) empty))
(check-expect (split-examples seen 'bear)
              (list empty
                    (list (list 'crow 'medium 'flies 'angry)
                          (list 'goose 'large 'swims 'flies 'angry)
                          (list 'goose 'large 'swims 'flies 'angry)
                          (list 'squirrel 'small 'angry))))


;; c)
;; (histogram examples) Produces a histogram indicating how many times
;; each attribute appeears in examples.
;; Examples:
(check-expect (histogram seen)
              (list (list 'medium 1) (list 'flies 3) (list 'swims 2)
                    (list 'large 2) (list 'angry 4) (list 'small 1)))
(check-expect (histogram (list (list 'squirrel 'small 'angry)
                               (list 'goose 'large 'swims 'flies 'angry)))
              (list (list 'flies 1) (list 'swims 1) (list 'large 1)
                    (list 'angry 2) (list 'small 1)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local [(define (add-appear attribute hist)
            (cond [(empty? hist) empty]
                  [(symbol=? (first (first hist)) attribute)
                   (cons (list (first (first hist))
                               (add1 (second (first hist))))
                         (add-appear attribute (rest hist)))]
                  [else (cons (first hist)
                              (add-appear attribute (rest hist)))]))
          (define (add-exist attributes hist existing-atribs examples)
            (cond [(empty? attributes)
                   (histogram-local (rest examples) hist existing-atribs)]
                  [(member? (first attributes) existing-atribs)
                   (add-exist (rest attributes)
                              (add-appear (first attributes) hist)
                              existing-atribs examples)]
                  [else (add-exist (rest attributes)
                                   (cons (list (first attributes) 1) hist)
                                   (cons (first attributes) existing-atribs)
                                   examples)]))
          (define (histogram-local examples hist existing-atribs)
            (cond [(empty? examples) hist]
                  [else (add-exist (rest (first examples))
                                   hist existing-atribs examples)]))]
    (histogram-local examples empty empty)))

;; Tests:
(check-expect (histogram empty) empty)
(check-expect (histogram (list (list 'crow 'medium 'flies 'angry)))
              (list (list 'angry 1) (list 'flies 1) (list 'medium 1)))
(check-expect (histogram (list (list 'cow))) empty)


;; d)
;; (augment-histogram histogram attributes total) Produces an augmented
;; histogram from histogram using attributes (the list of all attributes)
;; and total (the total for the number of examples).
;; Examples:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c) 200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'c 'a 'b) 200)
 (list (list 'c 50 150) (list 'a 100 100) (list 'b 0 200)))

;; augment-histogram: Histogram (listof Sym) Nat -> AH
(define (augment-histogram histogram attributes total)
  (local [(define (member-of? sym histogram)
            (cond [(empty? histogram) false]
                  [(symbol=? (first (first histogram)) sym)
                   true]
                  [else (member-of? sym (rest histogram))]))
          (define (remove-h sym histogram)
            (cond [(empty? histogram) empty]
                  [(symbol=? (first (first histogram)) sym)
                   (remove-h sym (rest histogram))]
                  [else (cons (first histogram)
                              (remove-h sym (rest histogram)))]))
          (define (adjust-h sym histogram total)
            (cond [(symbol=? (first (first histogram)) sym)
                   (list (first (first histogram)) (second (first histogram))
                         (- total (second (first histogram))))]
                  [else (adjust-h sym (rest histogram) total)]))
          (define (augment histogram attributes total)
            (cond [(empty? attributes) empty]
                  [(member-of? (first attributes) histogram)
                   (cons (adjust-h (first attributes) histogram total)
                         (augment (remove-h (first attributes) histogram)
                                  (rest attributes)
                                  total))]
                  [else (cons (list (first attributes) 0 total)
                              (augment histogram (rest attributes) total))]))]
    (augment histogram attributes total)))

;; Tests:
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))
(check-expect
 (augment-histogram
  (list (list 'a 100))
  (list 'c 'a 'b) 200)
 (list (list 'c 0 200) (list 'a 100 100) (list 'b 0 200)))
(check-expect
 (augment-histogram
  (list (list 'a 200))
  (list 'c 'a 'b) 200)
 (list (list 'c 0 200) (list 'a 200 0) (list 'b 0 200)))
(check-expect
 (augment-histogram
  (list (list 'c 50) (list 'a 100))
  (list 'a 'b 'c) 200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))


;; e)
;; (entropy positive-counts negative-counts) Produces the entropy of
;; positive-counts and negative-counts.

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
;; Requires: The elements are for the same attribute and are taken from the
;; augmented histogram for each split
(define (entropy positive-counts negative-counts)
  (local [(define (p n m)
            (cond [(zero? (+ n m)) 0.5]
                  [else (/ n (+ n m))]))
          (define (e p)
            (cond [(zero? p) 0]
                  [else (* (- p) (log p 2))]))
          (define a (second positive-counts))
          (define b (second negative-counts))
          (define c (third positive-counts))
          (define d (third negative-counts))]
    (+ (* (p (+ a b) (+ c d)) (+ (e (p a b)) (e (p b a))))
       (* (p (+ c d) (+ a b)) (+ (e (p c d)) (e (p d c)))))))

;; Tests:
(check-within (entropy (list 'large 126 59) (list 'large 146 669)) 0.566 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361)) 0.583 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0 0.001)


;; f)
;; (entropy-attributes positive negative) Produces the entropy of each attribute
;; in both positive and negative, producing a list of attribute/entropy pairs.

;; entropy-attributes: AH AH -> EAL
;; Requires: The same attributes will be included in both positive and negative
;; and that the attributes will appear in the same order
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))

;; Test:
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large 0.566) (list 'angry 0.645)
               (list 'small 0.583) (list 'flies 0.670)
               (list 'swims 0.602) (list 'medium 0.690)) 0.001)


;; g)
;; (best-attribute entropies) Produces the attribute with the minimum entropy
;; in entropies.
;; Examples:
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.6901071708677))) 'large)
(check-expect (best-attribute
                (list
                 (list 'large #i0.8063948489858)
                 (list 'angry #i0.6447688190492))) 'angry)

;; best-attribute: (ne-listof (list Sym Num)) -> Sym
(define (best-attribute entropies)
  (local [(define (best-a e min)
            (cond [(empty? e) (first min)]
                  [(> ( second min) (second (first e)))
                   (best-a (rest e) (first e))]
                  [else (best-a (rest e) min)]))]
    (best-a entropies (first entropies))))

;; Tests:
(check-expect  (best-attribute
                (list
                 (list 'angry #i0.6447688190492))) 'angry)
(check-expect  (best-attribute
                (list
                 (list 'boo 100) (list 'waa 300))) 'boo)
(check-expect  (best-attribute
                (list
                 (list 'food -100) (list 'drinks -400))) 'drinks)


;; h)
;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

;; (build-dt examples label) Produces a decision tree for label using examples.

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [(define attributes (collect-attributes examples))
          (define split (split-examples examples label))
          (define (dt-builder examples label)
            (local
              [(define root (best-attribute (entropy-attributes
                                             (augment-histogram
                                              (histogram (first split))
                                              attributes
                                              (length (first split)))
                                             (augment-histogram
                                              (histogram (second split))
                                              attributes
                                              (length (second split))))))]
              (cond [(equal?
                      (build-dt (first (split-root examples root)) label)
                      (build-dt (second (split-root examples root)) label))
                     (build-dt (second (split-root examples root)) label)]
                    [else
                     (list root
                           (build-dt (first (split-root examples root)) label)
                           (build-dt (second (split-root examples root))
                                     label))])))
          (define (delete-root root lst)
            (cond [(empty? lst) empty]
                  [(symbol=? (first lst) root)
                   (delete-root root (rest lst))]
                  [else (cons (first lst) (delete-root root (rest lst)))]))
          (define (remove-root root lst)
            (cond [(empty? lst) empty]
                  [else (cons (delete-root root (first lst))
                              (remove-root root (rest lst)))]))
          (define (split-root examples root)
            (list (remove-root root (first (split-examples examples root)))
                  (second (split-examples examples root))))]
    (cond [(empty? (first split)) false]
          [(empty? (second split)) true]
          [(empty? attributes)
           (> (length (first split)) (length (second split)))]
          [else (dt-builder examples label)])))


;; i)
;; (train-classifier examples label) Produces a decision tree from the examples
;; using label, and then produce a predicate that consumes a list of attributes
;;  and produces a decision.

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local [(define tree (build-dt examples label))
          (define (pred-help lst tree)
            (cond [(boolean? tree) tree]
                  [(member? (first tree) lst)
                   (pred-help lst (second tree))]
                  [else (pred-help lst (third tree))]))
          (define (pred lst)
            (pred-help lst tree))]
    pred))

;; Tests:
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)


;; Bonus
;; (performance classifier? examples label) Produces a list containing the
;; label along with the sensitivity and specificity expressed as a decimal.

;; performance: ((listof Sym) -> Bool) (listof Example) Sym ->
;; (list Sym Nat Nat)
(define (performance classifier? examples label)
  (local [(define (actual-label examples)
            (cond [(empty? examples) 0]
                  [(symbol=? (first (first examples)) label)
                   (add1 (actual-label (rest examples)))]
                  [else (actual-label (rest examples))]))
          (define actual-label-examples (actual-label examples))
          (define (performance-calculator examples correct-label
                                          correct-not-label)
            (cond [(empty? examples) (list correct-label correct-not-label)]
                  [(and (symbol=? (first (first examples)) label)
                        (classifier? (first examples)))
                   (performance-calculator (rest examples) (add1 correct-label)
                                           correct-not-label)]
                  [(and (not (symbol=? (first (first examples)) label))
                        (not (classifier? (first examples))))
                   (performance-calculator (rest examples) correct-label
                                           (add1 correct-not-label))]
                  [else (performance-calculator (rest examples) correct-label
                                                correct-not-label)]))
          (define number-correct (performance-calculator examples 0 0))]
    (cond [(empty? examples)
           (list label 0 0)]
          [(zero? actual-label-examples)
           (list label 0 (round (* 100 (/ (second number-correct)
                                          (- (length examples)
                                             actual-label-examples)))))]
          [(= actual-label-examples (length examples))
           (list label (round (* 100 (/ (first number-correct)
                                        actual-label-examples))) 0)]
          [else (list label (round (* 100 (/ (first number-correct)
                                             actual-label-examples)))
                      (round (* 100 (/ (second number-correct)
                                       (- (length examples)
                                          actual-label-examples)))))])))