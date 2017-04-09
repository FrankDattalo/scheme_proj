; Author: Frank Dattalo

; The following redefines serve no purpose other than
; for the way that I think about the operations in my head.
; I prefer the semantic names that I have the operations,
; and thus is why I renamed them.
(define (head lst)   (car lst))
(define (tail lst)   (cdr lst))
(define (pair p1 p2) (cons p1 p2))

; filter-list filters a list based off of a passed predicate.
;
; filter-list expects two parameters:
;
; list-to-filter:   a list to filter based off a provided filter function
;
; filter-function:  a predicate function that when applied to an element of
;                   the list should return #t or #f
(define (filter-list list-to-filter filter-function)
  (if (null? list-to-filter) 
    list-to-filter
    (let
        ((list-head (head list-to-filter))
         (list-tail (tail list-to-filter)))
      (if (filter-function list-head)
        (pair list-head (filter-list list-tail filter-function))
        (                filter-list list-tail filter-function)))))

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
  (if (null? l1)
    l2
    (let
        ((list-head (head l1))
         (list-tail (tail l1)))
      (pair list-head (list-concat list-tail l2)))))

; quicksort sorts a list by recorsively partitioning the list based off of a pivot value
; 
; quicksort expects 1 parameter:
;
; list-to-sort: the list to sort
(define (quick-sort list-to-sort)
  (if (null? list-to-sort)
    list-to-sort
    (let
        ((pivot (head list-to-sort))
        (rest  (tail list-to-sort)))
      (let
          ((less-list (filter-list rest (lambda (item) (<  item pivot))))
           (grtr-list (filter-list rest (lambda (item) (>= item pivot)))))
        (list-concat
          (list-concat (quick-sort less-list) (list pivot))
          (quick-sort grtr-list))))))

; eliminate takes two lists and produces a list which contains all elements
; from remove-from-list that are not in using-elements-of-list
;
; eliminate expects two parameters:
;
; remove-from-list:       the list to remove elements from if they appear in the second list
;
; using-elements-of-list: the list to use when removing elements from the first list
(define (eliminate remove-from-list using-elements-of-list)
  (if (or (null? remove-from-list) (null? using-elements-of-list))
      remove-from-list
      (let
          ((remove-from-element (head remove-from-list))
           (tail-remove-list    (tail remove-from-list))
           (using-element       (head using-elements-of-list))
           (tail-using-list     (tail using-elements-of-list)))
        (cond
          ((< remove-from-element using-element) (pair remove-from-element (eliminate tail-remove-list using-elements-of-list)))
          ((= remove-from-element using-element) (                          eliminate tail-remove-list using-elements-of-list))
          ((> remove-from-element using-element) (                          eliminate remove-from-list tail-using-list))))))

; eliminate-and-sort takes two lists and produces a sorted list which contains all elements
; from l1 that are not in l2
;
; eliminate-and-sort expects two parameters:
;
; l1: the list to remove elements from if they appear in the second list
;
; l2: the list to use when removing elements from the first list
(define (eliminateNsort l1 l2)
  (if (or (null? l1) (null? l2))
    l1
    (eliminate (quick-sort l1) (quick-sort l2))))