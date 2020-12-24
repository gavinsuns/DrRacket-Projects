;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =============================================================================
;; struct and data definitions For Q1
;; =============================================================================

;; A StudentID is a Nat with at most 8 digits (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have an ID < student's ID
;;           all students in the right subtree have an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN


;; =============================================================================
;; constants used in Q1 examples
;; =============================================================================

(define beth (make-student 12345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define john2 (make-student 20488192 "John" false))

(define sample-roster
  (make-rnode beth 
              (make-rnode jenny empty empty)
              (make-rnode john1 empty empty)))

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))


;; (find-student stid r) Produces the student in Roster r with StudentID stid.
;; Produces false if student ID stid is not in roster r.
;; Examples:
(check-expect (find-student 12345678 sample-roster) beth)
(check-expect (find-student 87654321 sample-roster) false)

;; find-student: StudentID Roster -> (anyof Student false)
(define (find-student stid r)
  (cond [(empty? r) false]
        [(= stid (student-id (rnode-student r))) (rnode-student r)]
        [(> stid (student-id (rnode-student r)))
         (find-student stid (rnode-right r))]
        [else (find-student stid (rnode-left r))]))

;; Tests:
(check-expect (find-student 12345678 empty) false)
(check-expect (find-student 20488192 sample-roster-2) john2)
(check-expect (find-student 99999999 sample-roster) false)


;; (class-average r) Produces the class average of Roster r.
;; Examples:
(check-expect (class-average sample-roster) (+ 90 2/3))
(check-expect (class-average sample-roster-2) (+ 90 2/3))

;; class-average: Roster -> (anyof 'N/A Num)
(define (class-average r)
  (cond [(zero? (students-grades-recurse r)) 'N/A]
        [else (/ (class-gtotal-recurse r) (students-grades-recurse r))]))

;; Tests:
(check-expect (class-average empty) 'N/A)
(check-expect (class-average (make-rnode beth empty empty)) 96)
(check-expect (class-average (make-rnode beth empty (make-rnode jenny empty empty))) 88.5)


;; (class-gtotal-recurse r) Produces the sum of all numeric grades in Roster r.
;; Examples and tests: see wrapper function class-average

;; class-gtotal-recurse: Roster -> Num 
(define (class-gtotal-recurse r)
  (cond [(empty? r) 0]
        [(and (number? (student-grade (rnode-student r))) (rnode? r))
         (+ (student-grade (rnode-student r))
            (class-gtotal-recurse (rnode-left r))
            (class-gtotal-recurse (rnode-right r)))]
        [(rnode? r) (+ (class-gtotal-recurse (rnode-left r))
                       (class-gtotal-recurse (rnode-right r)))]))


;; (students-grades-recurse r) Produces the total number of students with numeric grades.
;; Examples and tests: see wrapper function class-average

;; students-grades-recurse: Roster -> Num
(define (students-grades-recurse r)
  (cond [(empty? r) 0]
        [(and (number? (student-grade (rnode-student r))) (rnode? r))
         (+ 1 (students-grades-recurse (rnode-left r))
            (students-grades-recurse (rnode-right r)))]
        [(rnode? r) (+ (students-grades-recurse (rnode-left r))
                       (students-grades-recurse (rnode-right r)))]))


;; (find-student/name name r) Produces all students in Roster r with name, name.
;; Examples:
(check-expect (find-student/name "Beth" sample-roster) (list beth))
(check-expect (find-student/name "Dan" sample-roster) empty)

;; find-student/name: Str Roster -> (listof Student)
(define (find-student/name name r)
  (cond [(empty? r) empty]
        [(string=? name (student-name (rnode-student r)))
         (append (list (rnode-student r)) (find-student/name name (rnode-left r))
                 (find-student/name name (rnode-right r)))]
        [else (append (find-student/name name (rnode-left r))
                      (find-student/name name (rnode-right r)))]))

;; Tests:
(check-expect (find-student/name "Jenny" sample-roster) (list jenny))
(check-expect (find-student/name "Bob" sample-roster-2) empty)
(check-expect (find-student/name "John" sample-roster-2) (list john1 john2))


;; (add-students lst r) Produces a new Roster with all students in lst
;; either added or adjusted to Roster r.
;; Examples:
(check-expect (add-students
               (list (list 20488192 "John")
                     (list 8675309 "Jen"))
               sample-roster)
              sample-roster-2)

;; add-students: (listof (list StudentID Str)) Roster -> Roster
(define (add-students lst r)
  (cond [(empty? lst) r]
        [else (add-students (rest lst) (do-std r (first lst)))]))

;; Tests:
(check-expect (add-students empty sample-roster)
              sample-roster)
(check-expect (add-students (list (list 13249586 "John") (list 12345678 "Kyle")) empty)
              (make-rnode (make-student 13249586 "John" #false)
                          (make-rnode (make-student 12345678 "Kyle" #false) '() '()) '()))
(check-expect (add-students (list (list 12345678 "Beth")) sample-roster)
              sample-roster)
(check-expect (add-students
               (list (list 30203403 "Bob")
                     (list 12345678 "David"))
               sample-roster)
              (make-rnode
               (make-student 12345678 "David" 96)
               (make-rnode jenny '() '())
               (make-rnode john1
                           (make-rnode (make-student 30203403 "Bob" #false) '() '())
                           '())))


;; (do-std r std) Produces a new roster with student std either added or
;; adjusted in roster r.
;; Examples and tests: see wrapper function add-students

;; do-std: Roster Student -> Roster
(define (do-std r std)
  (cond [(student? (find-student (first std) r))
         (adjust-std std r)]
        [else (add-std std r)]))


;; (add-std std r) Produces a new roster with student std added to roster r.
;; Example:
(check-expect (add-std
               (list 20488192 "John")
               sample-roster)
              (make-rnode
               beth (make-rnode jenny '() '())
               (make-rnode john1 (make-rnode (make-student 20488192 "John" #false) '() '()) '())))

;; add-std: Student Roster -> Roster
(define (add-std std r)
  (cond [(empty? r) (make-rnode (make-student (first std) (second std) false) empty empty)]
        [(< (first std) (student-id (rnode-student r)))
         (make-rnode (rnode-student r) (add-std std (rnode-left r)) (rnode-right r))]
        [else (make-rnode (rnode-student r) (rnode-left r) (add-std std (rnode-right r)))]))


;; (adjust-std std r) Produces a new roster  with student std's name changed in roster r.
;; Example:
(check-expect (adjust-std
               (list 48975311 "Becky")
               sample-roster)
              (make-rnode beth (make-rnode jenny '() '())
                          (make-rnode (make-student 48975311 "Becky" 95) '() '())))

;; adjust-std: Student Roster
(define (adjust-std std r)
  (cond [(= (first std) (student-id (rnode-student r)))
         (make-rnode (make-student (first std) (second std) (student-grade (rnode-student r)))  (rnode-left r) (rnode-right r))]
        [(< (first std) (student-id (rnode-student r)))
         (make-rnode (rnode-student r) (adjust-std std (rnode-left r)) (rnode-right r))]
        [else (make-rnode (rnode-student r) (rnode-left r) (adjust-std std (rnode-right r)))]))