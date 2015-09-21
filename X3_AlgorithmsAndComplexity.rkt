#lang racket

;--- Chantzi Efthymia
;--- DrRacket, version 6.1


;------------------------------------------->   X4: ALGORITHMS AND COMPLEXITY  <-------------------------------------------------
;---> Detailed Exercise Instructions: https://drive.google.com/file/d/0B4mMtieYoaNDM1JiSHpiaGt6OVU/view?usp=sharing <---


;--------------- Task 1 ---------------

;a.
;The recurrence model of the realistic Fibonacci's model rabbit of expansion is:

;                  F_(n) = F_(n-2) + F_(n-3)    , n > 3



;  Year| New Born Pairs | Newly Mature Pairs | Really Mature Pairs | Total # of Pairs |
;  ------------------------------------------------------------------------------------
;    1 |      1         |       0            |        0            |          1       |
;  ------------------------------------------------------------------------------------  
;    2 |      0         |       1            |        0            |          1       |
;  ------------------------------------------------------------------------------------  
;    3 |      1         |       0            |        1            |          2       |
;  ------------------------------------------------------------------------------------  
;    4 |      1         |       1            |        0            |          2       |
;  ------------------------------------------------------------------------------------  
;    5 |      1         |       1            |        1            |          3       |
;  ------------------------------------------------------------------------------------  
;    6 |      2         |       1            |        1            |          4       |
;  ------------------------------------------------------------------------------------  
;    7 |      2         |       2            |        1            |          5       |
;  ------------------------------------------------------------------------------------  
;    8 |      3         |       2            |        2            |          7       |
;  ------------------------------------------------------------------------------------  
;    9 |      4         |       3            |        2            |          9       |
;  ------------------------------------------------------------------------------------  
;   10 |      5         |       4            |        3            |         12       |
;  ------------------------------------------------------------------------------------  
;   11 |      7         |       5            |        4            |         16       |
;  ------------------------------------------------------------------------------------  
;   12 |      9         |       7            |        5            |         21       |
;  ------------------------------------------------------------------------------------
;             ......the expansion continues like this indefinitely......
;---------------------------------------------------------------------------------------


;b.
;recursive procedure used in 'fibonacci-bounded-rec'
(define (fibo-rec n)
  (cond ((< n 0) (display "Invalid Input."))
        ((= n 0) 1) 
        ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (fibo-rec (- n 2)) (fibo-rec (- n 3))))))
;----------------------------------------------------------

;recursive procedure used in 'fibonacci-bounded-it'
(define (fibo-it n)
  
  (define (fibo-it-help n baby adults elderly counter)
    
    (cond ((< n 0) (display "Invalid Input."))
          ((= counter n) (+ (+ baby adults) elderly))
          (else (fibo-it-help n (+ adults elderly) baby adults (+ counter 1)))))
  
  (fibo-it-help n 1 0 0 0))
;---------------------------------------------------------------------------------

;a map function for the procedure 'fibonacci-bounded' ('fibonacci-bounded-rec' and 'fibonacci-bounded-it')
(define (map-fibonacci-bounded n l proc counter)
    (cond ((<= n 3) (display "No rabbit pairs."))
          ((= counter  n) (reverse l))
          (else (map-fibonacci-bounded n (cons (proc counter) l) proc (+ counter 1)))))
  
  
;'fibonacci-bounded-rec'
(define (fibonacci-bounded-rec n)
  (map-fibonacci-bounded n '() fibo-rec 3))
  
;'fibonacci-bounded-it'
(define (fibonacci-bounded-it n)
  (map-fibonacci-bounded n '() fibo-it 3))
;--------------------------------------------------------------------------------------------------------------

;c.
;Given the assumptions of Task 1, there is no upper bound for the algorithm of the realistic model of rabbit expansion.
;There are always rabbits that give birth to new pairs of rabbits and this model of expansion goes on and on indefinitely, even if the life 
;span of rabbits is limited by 2.999 years.
;-------------------------------------------------------------------------------------------------------------------------------------


;-------------- Task 2 -----------------

; log(n) = O(x)? ---> True

; log(n) = Ω(x)? ---> False, but  log(n) = Ω(1) ---> True

; log(n) = Θ(x)? ---> False, but  log(n) = Θ(log(n)) ---> True

;--------------------------------------------------------------------------------------------------------------------------------------



;-------------- Task 3 -----------------

;------------------ start of functions for handling lists ----------------------------------------------------------
;-----------------------------
(define (replace-nth l n elem)
  (cond ((null? l) '())
        ((= n 0) (cons elem (cdr l)))
        (else (cons (car l) (replace-nth (cdr l) (- n 1) elem)))))

;----------------------------------
(define (check-lists l1 l2 position)
  (if (= (list-ref l1 position) (list-ref l2 position))
      #t
      #f))

;---------------------------
(define (create-zeroList l)  
  
  (define (create-zeroList-help len)
    (if (= len 0)
        '()
        (cons 0 (create-zeroList-help (- len 1)))))
  
  (create-zeroList-help (length l)))

;------------------------------
(define (flatten list-of-lists)
(cond ((null? list-of-lists) '())
      (else (append (first list-of-lists) (flatten (rest list-of-lists))))))

(define (delete-from-list l elem)
  (remove* (list elem) l))

;---------------------------
(define (make-sublist n1 n2)
  
  (define (make-sublist-help n1 n2 counter)
    (if (> counter n2) 
        '()
        (cons counter (make-sublist-help n1 n2 (+ counter 1)))))
  
  (make-sublist-help n1 n2 (+ n1 1)))
;------------------ end of functions for handling lists ----------------------------------------------------------


;------------------ start of functions creating the lists of indexes ---------------------------------------------
;-----------------------------------------
(define (create-start-comb l1 l2 position)
  
  (define (start-comb-help l1 l2 position limit)
    (cond ((null? limit) '())
          (else (cons (replace-nth l1  position (car limit)) (start-comb-help l1 l2 position (cdr limit))))))
    
  (start-comb-help l1 l2 position (make-sublist (list-ref l1 position) (list-ref l2 position))))
  
;--------------------------------
(define (base-combinations l1 l2)
  
  (define (base-help l1 l2 position)
    (if  (< position 0) 
          '()
         (cons (create-start-comb l1 l2 position) (base-help l1 l2 (- position 1)))))
  
  (base-help l1 l2 (- (length l1) 1)))

;-----------------------
(define (mapping l1 l2)
  
  (define (mapping-help l1 l2 counter)
    
    (cond ((= counter (length l1)) '())
          ((= counter 0) (cons l2 (cons (map + (list-ref l1 counter) l2) (mapping-help l1 l2 (+ counter 1)))))
          (else (cons (map + (list-ref l1 counter) l2) (mapping-help l1 l2 (+ counter 1))))))
  
  (mapping-help l1 l2 0))

;---------------------------
(define (mapping-lists l1 l2)
  
  (define (lists-help l1 l2 counter)
    (if (= counter (length l2))
        '()
        (cons (mapping l1 (list-ref l2 counter)) (lists-help l1 l2 (+ counter 1)))))
  
  (lists-help l1 l2 0))

;----------------------
(define (for-index l2)
  
  (define (for-index-help l1 l2 counter)
    (cond ((= counter (length l2)) (cons l1 '()))
          (else  (for-index-help (flatten (cons l1 (mapping-lists l1 (list-ref l2 counter)))) l2 (+ counter 1)))))
  
  (for-index-help (list-ref l2 0) l2 1))
;------------------ end of functions creating the lists of indexes -----------------------------------------------


;------------------ function 'index' ---------------------------------------------
(define (index l)  
  (if (equal? l (create-zeroList l))
      l
      (cons (create-zeroList l) (flatten (for-index (delete-from-list (base-combinations (create-zeroList l) l) '()) )))))

;-------------------------------------------------------------------------------------------------------------------------


;In this task, a "brute-force" algorithm is implemented, since every possible combination is examined until the solution is reached. 
;A possible application of this algorithm could be within the field of cryptography, where all possible keys are systematically checked until
;the correct one is found.
