; Author: Frank Dattalo

; The following redefines serve no purpose other than
; for the way that I think about the operations in my first.
; I prefer the semantic names that I have the operations,
; and thus is why I renamed them.
(define (first lst)  (car lst))
(define (rest lst)   (cdr lst))
(define (pair p1 p2) (cons p1 p2))

; filter-list filters a list based off of a passed predicate.
;
; filter-list expects two parameters:
;
; l2f:   a list to filter based off a provided filter function
;
; pf:  a predicate function that when applied to an element of
;                   the list should return #t or #f
(define (filter-list l2f pf)
  (cond 
    ((null? l2f)      l2f)
    ((pf (first l2f)) (pair (first l2f) (filter-list (rest l2f) pf)))
    (#t               (                 filter-list (rest l2f) pf))))

; list-concat concatenates two lists together such as 
; (l1[1] l1[2] . . l1[N-1] l1[N] l2[1] l2[2] . . . l2[N-1] l2[N])
; where l1[X] represents the element X within l1 at position X
;   and l2[Y] represents the element Y within l2 at position Y
;
; list-concat expects two parameters:
;
; l1: the first list
;
; l2: the second list
(define (list-concat l1 l2)
  (cond 
    ((null? l1) l2)
    (#t         (pair (first l1) (list-concat (rest l1) l2)))))

; quicksort sorts a list by recorsively partitioning the list based off of a pivot value
; 
; quicksort expects 1 parameter:
;
; l1: the list to sort
(define (quick-sort l1)
  (cond 
    ((null? l1) l1)
    (#t         
      (list-concat
      (list-concat 
        (quick-sort (filter-list (rest l1) (lambda (i) (<  i (first l1))))) (pair (first l1) '()))
        (quick-sort (filter-list (rest l1) (lambda (i) (>= i (first l1)))))))))


; eliminate takes two lists and produces a list which contains all elements
; from l1 that are not in l2
;
; eliminate expects two parameters:
;
; l1:       the list to remove elements from if they appear in the second list
;
; l2: the list to use when removing elements from the first list
(define (eliminate l1 l2)
  (cond
    ((null? l1) l1)
    ((null? l2) l1)
    ((< (first l1) (first l2)) (pair (first l1) (eliminate (rest l1) l2)))
    ((= (first l1) (first l2)) (                 eliminate (rest l1) l2))
    ((> (first l1) (first l2)) (                 eliminate l1        (rest l2)))))


; eliminate-and-sort takes two lists and produces a sorted list which contains all elements
; from l1 that are not in l2
;
; eliminate-and-sort expects two parameters:
;
; l1: the list to remove elements from if they appear in the second list
;
; l2: the list to use when removing elements from the first list
(define (eliminateNsort l1 l2)
  (cond
    ((null? l1) l1)
    ((null? l2) l1)
    (#t         (eliminate (quick-sort l1) (quick-sort l2)))))