#lang racket

;--- Chantzi Efthymia
;--- DrRacket, version 6.1


;------------------------------------------->   X2: LISTS AND HIERARHICAL DATA STRUCTURES  <-------------------------------------------------
;---> Detailed Exercise Instructions: https://drive.google.com/file/d/0B4mMtieYoaNDU04tTmZQY0NHanM/view?usp=sharing <---



;defined lists for tests
(define my-list1 (list 1 2 3 4 5))
(define my-list2 (list 5 7 3 4 8 2))


;---Task 1: Basic List Procedures---


;1.
;recursive 'reverse-list-rec'
(define (reverse-list-rec l) 
  (if (null? l)
      '()
      (append (reverse-list-rec (cdr l)) (list(car l)))))
;----------------------------------------------------------


;iterative 'reverse-list-it'
(define (reverse-list-it l)
  
  (define (reverse-list-it-help x y)
    (if (null? x)
        y
        (reverse-list-it-help (cdr x) (cons (car x) y))))
  
  (reverse-list-it-help l '()))
;------------------------------------------------------------------------------------------------------------------------------------------

;2
(define (n-first n l)
   (cond ((< n 0) (display "Error.Number 'n' must be at least equal to 1.")) 
         ((> n (length l)) (display "Error.Number 'n' must be at most the same as the length of the list."))
         ((= n 0) '())
         (else (cons (car l) (n-first  (- n 1) (cdr l)))))) 
;------------------------------------------------------------------------------------------------------------------------------------------

;3
(define (n-last n l)
  
  (define (n-last-help n l check)
    (cond ((<= n 0) (display "Error.Number 'n' must be at least equal to 1."))
          ((> n (length l)) (display "Error.Number 'n' must be at most the same as the length of the list."))
          ((= check (length l)) '())
          (else (cons (nth check l) (n-last-help n l (+ check 1))))))
      
  (n-last-help n l  (- (length l) n)))

;I use the procedure 'nth' in order to access the nth element of the list for the needs of the procedure 'n-last'
(define (nth n l)
  (if (= n 0)
      (car l)
      (nth (- n 1) (cdr l))))
;--------------------------------------------------------------------------------------------------------------------------------------------

;n-last can be also implemented by 'n-last-combined' using 'reverse-list' & 'n-first' from above
(define (n-last-combined n l)
  
  (reverse-list-rec (n-first n (reverse-list-rec l))))  
;-------------------------------------------------------------


;4.
(define (sub-list n_start n_end l)
 
  (if (or (< n_start 0) (> n_start n_end))
      '()
      (cons (nth n_start l) (sub-list (+ n_start 1) n_end l))))
;----------------------------------------------------------------


;5.
(define (position l pred)
  
  (define (position-help l pred counter)
    (cond ((null? l) -1)               ;-1 indicates that there is not any element in the list that satisfies the predicate
          ((pred (car l)) counter)
          (else (position-help (cdr l) pred (+ counter 1)))))
 
  (position-help l pred 0))
;------------------------------------------------------------------


;6.
(define (position-and-element l pred)
  
  (define (position-help l pred counter l_initial)
    (cond ((null? l) -1)
          ((pred (car l)) (cons counter (nth counter l_initial)))      ;use of the procedure 'nth' that has already been defined
          (else (position-help (cdr l) pred (+ counter 1) l_initial))))

  (position-help l pred 0 l))
;----------------------------------------------------------------------


;7.
(define (all-position-and-element l pred)
  
    (define (position-help l pred counter l_initial)
    (cond ((null? l) '())
          ((pred (car l)) (cons (cons counter (nth counter l_initial)) (position-help (cdr l) pred (+ counter 1) l_initial)))
          (else (position-help (cdr l) pred (+ counter 1) l_initial))))

 
  (position-help l pred 0 l))
;----------------------------------------------------------------------------------------------------------------------------------




;---Task 2: Accumulating Lists--- 

(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst) (accumulate op init (cdr lst)))))
;---------------------------------------------------------------      

;---length---
(define (length l)
  (accumulate (lambda (x y) (+ 1 y)) 0  l))
  
;---append---
(define (append l1 l2)
  (accumulate (lambda (x y) (cons x y)) l2 l1))

;---map---
(define (map proc l)
  (accumulate (lambda (x y) (cons (proc x) y)) '() l))

;------------------------------------------------------------




;---Task 3: Fibonacci Numbers--- 

;a.
;recursive procedure 'fibo-rec'
(define (fibo-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibo-rec (- n 1)) (fibo-rec (- n 2))))))


;iterative procedure 'fibo-it'
(define (fibo-it n)
  
  (define (fibo-it-help n a b)
    (if (= n 0) a
        (fibo-it-help (- n 1) b (+ a b))))
  (fibo-it-help n 0 1))


;--------------------------------------------------------------------------

;b.
;By running the two aforementioned implementations with different n's, we realize that their running time differs. Being more specific, this difference is better  
;distinguished as the value of 'n' increases. The bigger it gets, the more time is required by the recursive procedure 'fibo-rec' to perform the evaluation. In other 
;words, the iterative procedure 'fibo-it' is faster than the recursive 'fibo-rec'. Such a behavior is expected, since in the recursive procedure, the evaluation 
;expression gets bigger and bigger with larger inputs and thus, more running time is consumed. On the other hand, the iterative procedure performs the evaluation in  
;one state each time and ends up having a faster overall performance.
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------

;c.
(define (Fibo-series n)
  
  (define (Fibo-series-help n l counter a b)
    (cond ((<= n 0) '())
          ((= counter n) l)
          (else (Fibo-series-help n (append l (cons  b  '())) (+ counter 1) b (+ b a)))))
           
  (Fibo-series-help n '(0) 1 0 1))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------



  
;---Task 4: Tree Procedures---

;checks if a node is a leaf
(define (leaf? x)  
(not (pair? x)))

;estimates the square
(define (square x) (* x x))

;a.
(define (square-tree tree)
  (cond ((null? tree) '())
        ((leaf? tree) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
;---------------------------------------------------------------------------

;b.
(define (depth tree)
  (if (or (null? tree) (leaf? tree))
      0
      (max (+ 1 (depth (car tree))) (depth (cdr tree)))))

;---------------------------------------------------------------------------------------------------------------------------------------------------------------




;---Task 5: Binary Search Trees---   

;a
(define (left tree) (cadr tree)) ;procedure for left subtree

(define (right tree) (caddr tree)) ;procedure for right subtree

(define (binary-tree val left right) (list val left right)) ;procedure for the structure of binary tree



(define (list-to-bst l)
  
  (define (list-to-bst-help l tree)
    (if (null? l)
        tree
        (list-to-bst-help (cdr l) (insert (car l) tree))))
  
  (list-to-bst-help l '()))

  
;procedure for inserting a node into the binary tree
(define (insert val tree)
  
  (cond ((null? tree) (list val '() '()))
        ((= val (car tree)) tree)
        ((< val (car tree)) (binary-tree (car tree) (insert val (left tree))  (right tree)))
        (else (binary-tree (car tree) (left tree)  (insert val (right tree))))))

;------------------------------------------------------------------------------------------------

;b.
;procedure 'map-min-max-bst' finds the min and max elements of a bst with the appropriate 'proc' argument
(define (map-min-max-bst res proc l res_false) 
    (if (null? l)      
        res    
        (map-min-max-bst (car l) proc (proc l) (car l))))


;proc --> left for 'min-bst'
(define (min-bst l)
  
  (map-min-max-bst 0 left l 0))
;--------------------------------------------------------------------
  
;proc --> right for 'max-bst'
(define (max-bst l)
  
  (map-min-max-bst 0 right l 0))
;--------------------------------------------------------------------

;c.
(define (search-bst l n)
  (if(null? l)
     #f
     (cond ((not(number? n)) (display "Invalid form of n."))
           ((< n (car l)) (search-bst (left l) n))
           ((> n (car l)) (search-bst (right l) n))
           (else #t))))     
;---------------------------------------------------------------------------------------------------------------------------------------
