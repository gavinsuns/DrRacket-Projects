;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Gavin Sun (20875517)
;; CS 135 Fall 2020
;; Assignment 09, Problem 3
;; **********************************************
;;

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag
;; A Tag is (cons Sym (listof HI))

;; 3a)
;; (tokenize str) Produces a list of strings from str representing the opening
;; tags, closing tags, and strings in the document.
;; Examples:
(check-expect (tokenize "<p><h1>Heading</h1>Text</p>")
'("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "<p>good</p>") '("<p>" "good" "</p>"))

;; tokenize: Str -> (listof Str)
(define (tokenize str)
  (local [;; (first-item lstr) Produces a list containing the letters of the
          ;; first item in lstr.
          ;; first-item: (listof Char) -> (listof Char)
          (define (first-item lstr)
            (cond [(empty? lstr) empty]
                  [(char=? (first lstr) #\>)
                   (cons (first lstr) empty)]
                  [(or (empty? (rest lstr)) (char=? (second lstr) #\<))
                   (cons (first lstr) empty)]
                  [else (cons (first lstr) (first-item (rest lstr)))]))
          ;; (tokenizer lstr) Produces a list of strings from lstr representing
          ;; the opening tags, closing tags, and strings in the document.
          ;; (listof Char) -> (listof Str)
          (define (tokenizer lstr)
            (local [(define first-str (first-item lstr))]
            (cond [(empty? lstr) empty]
                  [else (cons (list->string first-str)
                         (tokenizer (string->list
                                     (substring (list->string lstr)
                                                (length first-str)
                                                (length lstr)))))])))]
    (tokenizer (string->list str))))

;;Tests:
(check-expect (tokenize "") empty)
(check-expect (tokenize "a b c") '("a b c"))
(check-expect (tokenize "<p></p>") '("<p>" "</p>"))
(check-expect (tokenize "<p>good boy</p>") '("<p>" "good boy" "</p>"))


;; 3b)
;; (string->html str) Produces the HTML-Item for str.
;; Examples:
(check-expect (string->html
               "<p><h1>Heading</h1>Text</p>")
              '(p (h1 "Heading") "Text"))
(check-expect (string->html
               "<p>Heading Text</p>")
              '(p "Heading Text"))

;; string->html: Str -> HI
(define (string->html str)
  (local [;; Constant containing the tokenized version of str.
          (define tokenized (tokenize str))
          ;; (tag lst) Produces lst without the angle brackets around it.
          ;; tag: (listof Char) -> (listof Char)
          ;; Requires: lst contains #\< as its first element and #\> as its last
          ;; element.
          (define (tag lst)
            (cond [(char=? (first lst) #\<)
                   (tag (rest lst))]
                  [(char=? (first lst) #\>)
                   empty]
                  [else (cons (first lst) (tag (rest lst)))]))
          ;; (split tok tag) Produces a list containing all elements in tok
          ;; before the appearance of tag.
          ;; split: (listof Str) -> (listof Str)
          ;; Requires: tok contains tag
          (define (split tok tag)
            (local [(define first-str (string->list (first tok)))]
              (cond [(equal? first-str tag) empty]
                    [else (cons (first tok) (split (rest tok) tag))])))
          ;; (rest-tag tok tag) Produces a list containing all elements in tok
          ;; after the appearance of tag.
          ;; rest-tag: (listof Str) -> (listof Str)
          ;; Requires: tok contains tag
          (define (rest-tag tok tag)
            (local [(define first-str (string->list (first tok)))]
              (cond [(equal? first-str tag) (rest tok)]
                    [else (rest-tag (rest tok) tag)])))
          ;; (str->html tok) Produces a list containing the HTML-Item for tok.
          ;; (listof Str) -> (list HI)
          (define (str->html tok)
            (local [;; Constant containing the list version of the first element
                    ;; in tok.
                    (define first-str
                      (cond [(empty? tok) empty]
                            [else (string->list (first tok))]))] 
              (cond [(empty? tok) empty]
                    [(char=? (first first-str) #\<)
                     (cons (append (list (string->symbol
                                    (list->string (tag first-str))))
                           (str->html
                            (split
                             (rest tok)
                             (cons #\<
                                   (cons #\/
                                         (rest first-str))))))
                           (str->html
                            (rest-tag
                             (rest tok)
                             (cons #\<
                                   (cons #\/
                                         (rest first-str))))))]
                    [else (cons (first tok) (str->html (rest tok)))])))]
    (cond [(empty? tokenized) ""]
          [(empty? (rest tokenized)) (first tokenized)]
          [else (first (str->html tokenized))])))

;; Tests:
(check-expect (string->html
               (string-append
                "<html><head><title>CS135</title></head>"
                "<body><h1>Welcome</h1>More text...</body></html>"))
              '(html (head (title "CS135"))
                     (body (h1 "Welcome")
                           "More text...")))
(check-expect (string->html "<title></title>")
              (list 'title))
(check-expect (string->html "") "")
(check-expect (string->html "hello") "hello")