;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =============================================================================
;; data definitions For Q2
;; =============================================================================

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))


;; =============================================================================
;; constants For Q2 examples
;; =============================================================================

(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))


;; (html->string hi) Produces an html string from HTML-Item hi.
;; Examples:
(check-expect (html->string "text") "text")
(check-expect
 (html->string html-example)
 (string-append
  "<html><head><title>CS135</title></head>"
  "<body><h1>Welcome</h1>More text...</body></html>"))

;; html->string: HI -> Str
(define (html->string hi)
  (cond [(empty? hi) ""]
        [(string? hi) hi]
        [else (string-append "<" (symbol->string (first hi)) ">"
                             (tag-recurse (rest hi)) "</" (symbol->string (first hi)) ">")]))

;; Tests:
(check-expect (html->string "") "")
(check-expect (html->string "orange") "orange")
(check-expect (html->string '(title)) "<title></title>")
(check-expect (html->string short-example)
              "<p><h1>Heading</h1>Text</p>")


;; (tag-recurse t) Produces an html string from Tag t.
;; Examples and tests: see wrapper function html->string

;; tag-recurse: Tag -> String
(define (tag-recurse t)
  (cond [(empty? t) ""]
        [else (string-append (html->string (first t)) (tag-recurse (rest t)))]))


;; (remove-tag sym hi) Produces a new HI with all occurences of tag sym removed.
;; Examples:
(check-expect (remove-tag 'b '(p "Hello, " (b "World") "!"))
              '(p "Hello, " "World" "!"))
(check-expect (remove-tag 'p '(p "Hello, " (b "World") "!"))
              '("Hello, " (b "World") "!"))

;; remove-tag: Sym HI -> (anyof HI (listof HI))
(define (remove-tag sym hi)
  (cond [(string? hi) hi]
        [else (remove-lists (remove-recurse sym hi))]))

;; Tests:
(check-expect (remove-tag 'h "hello") "hello")
(check-expect (remove-tag 'h '(h)) empty)
(check-expect (remove-tag 'h '(b)) '(b))
(check-expect (remove-tag 'b html-example) html-example)
(check-expect (remove-tag 'h1 short-example) (list 'p "Heading" "Text"))
(check-expect (remove-tag 'z short-example) short-example)


;; (remove-recurse sym hi) Produces a new HI with all occurences of tag sym removed.
;; Examples and tests: see wrapper function remove-tag

;; remove-recurse: Sym HI -> (anyof HI (listof HI))
(define (remove-recurse sym hi)
  (cond [(empty? hi) empty]
        [(equal? (first hi) sym)
         (remove-recurse sym (rest hi))]
        [(list? (first hi))
         (cons (remove-recurse sym (first hi))
               (remove-recurse sym (rest hi)))]
        [else (cons (first hi)
                    (remove-recurse sym (rest hi)))]))


;; (remove-lists hi) Produces a new HI with all improper tag elements inserted
;; in new HI after removing improper tags.
;; Examples and tests: see wrapper function remove-tag

;; remove-lists: (anyof HI (listof HI)) -> (anyof HI (listof HI))
(define (remove-lists hi)
  (cond [(empty? hi) empty]
        [(list? (first hi))
         (cond [(symbol? (first (first hi)))
                (cons (first hi) (remove-lists (rest hi)))]
               [else (append (first hi) (remove-lists (rest hi)))])]
        [else (cons (first hi) (remove-lists (rest hi)))]))


;; (okay-tags? hi) Determines whether HI hi has followed the rules.
;; Examples:
(check-expect (okay-tags? html-example) true)
(check-expect (okay-tags? '(body (hr "hello"))) false)

;; okay-tags?: HI -> Bool
(define (okay-tags? hi)
  (cond [(empty? hi) true]
        [(string? hi) true]
        [(equal? (first hi) 'hr)
         (cond[(empty? (rest hi)) true]
              [else false])]
        [(equal? (first hi) 'li)
         false]
        [(or (equal? (first hi) 'ol) (equal? (first hi) 'ul))
         (okay-tags-help? (rest hi))]
        [(list? (first hi))
         (and (okay-tags? (first hi)) (okay-tags? (rest hi)))]
        [else (okay-tags? (rest hi))]))

;; Tests:
(check-expect (okay-tags? '(ol (li "hello"))) true)
(check-expect (okay-tags? "hello") true)
(check-expect (okay-tags? '(body (li "Q1") "text")) false)
(check-expect (okay-tags? '("HTML")) true)
(check-expect (okay-tags? '(hr)) true)
(check-expect (okay-tags? '(ol (li "hello") (hr))) true)
(check-expect (okay-tags? '(ol (hr))) true)
(check-expect (okay-tags? '(ul (hr "hello"))) false)
(check-expect (okay-tags? '(ul (a (li "hi") "bye") "hello")) false)
(check-expect (okay-tags? '(ul (li "boomer") (li "your mom"))) true)
(check-expect (okay-tags? '(ul "boomer" (li "your mom"))) true)


;; (okay-tags-help? hi) Determines whether HI hi has followed the rules, given
;; the fact that 'ol or 'ul is the direct parent of HI hi.
;; Examples and tests: see wrapper function okay-tags?

;; okay-tags-help?: HI -> Bool
(define (okay-tags-help? hi)
  (cond [(empty? hi) true]
        [(equal? (first hi) 'li) (okay-tags? (rest hi))]
        [(string? (first hi)) (okay-tags-help? (rest hi))]
        [(list? (first hi))
         (and (okay-tags-help? (first hi)) (okay-tags-help? (rest hi)))]
        [else (okay-tags? hi)]))