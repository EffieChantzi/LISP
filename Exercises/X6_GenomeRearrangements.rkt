
#lang racket

;--- Chantzi Efthymia
;--- DrRacket, version 6.1


;------------------------------------------->   X6: GENOME REARRANGEMENTS  <-------------------------------------------------
;---> Detailed Exercise Instructions: https://drive.google.com/file/d/0B4mMtieYoaNDMGctY3l3RjliUm8/view?usp=sharing <---


;--------------- Task 1 ---------------


;Line 1
;'Breakpoints' is implemented by a help function 'Break-help'. Three cases: a) it returns the total number of breakpoints when it reaches
;the end of list, b) if the difference between adjacent elements is greater than 1, it counts a breakpoint and adds it to the previous 
;ones if there exist any, c) there is no breakpoint(the elements are consecutive), so the execution continues with the next element.
(define (Breakpoints l)
  
  (define (Break-help l b counter)
    (cond ((= counter (- (length l) 1)) b)
          ((> (abs (- (list-ref l counter) (list-ref l (+ counter 1)))) 1) (Break-help l (+ b 1) (+ counter 1)))
          (else (Break-help l b (+ counter 1)))))
  
  (Break-help l 0 0))

;2 running examples
(display "---Line 1: Breakpoints--- \n")
(Breakpoints '(0 3 4 6 5 8 1 7 2 9)) ;7
(Breakpoints '(1 4 6 5 7 8 3 2)) ;4
(display "\n")
;------------------------------------------------------------------------------------------------------------------------------------------

;Line 6

;'sub-list' (from X2) extracts a sublist of a list specified by the start and end indices. 
(define (sub-list n_start n_end l)
  
  (define (help a b l counter count)
    (cond ((or (< a 0) (> b (- (length l) 1)) (> count (abs (- a b)))) '())
          ((> a b) (help b a l b count))
          (else (cons (list-ref l counter) (help a b l (+ counter 1) (+ count 1))))))
  (help n_start n_end l n_start 0))

;------------------------------------------------------------------------------------------

;'replace-nth' (from X2) replaces the value n of list l with the value of elem
(define (replace-nth l n elem)
  (cond ((null? l) '())
        ((= n 0) (cons elem (cdr l)))
        (else (cons (car l) (replace-nth (cdr l) (- n 1) elem)))))
;-------------------------------------------------------------------------------------------


;'Reverse' is implemented by 'help-Reverse', which makes a sublist of start and end indices, reverses it and then it replaces
;the unreversed sublist of the initial list with the reversed one. Procedures 'sub-list' and 'replace-nth' are used in its implementation.
(define (Reverse l pos_start_rev pos_end_rev)
  
  (define (help-Reverse l start end counter l_reverse)
    (cond ((null? l_reverse) (display "Indices out of bounds."))
          ((> start end) (help-Reverse l end start counter l_reverse))
          ((= counter (length l_reverse)) l)
          (else (help-Reverse (replace-nth l (- start 1) (list-ref l_reverse counter)) (+ start 1) end (+ counter 1) l_reverse))))
           
  (help-Reverse l pos_start_rev pos_end_rev 0 (reverse (sub-list (- pos_start_rev 1) (- pos_end_rev 1) l))))


;2 running examples
(display "---Line 6: Reverse--- \n")
(Reverse '(3 4 5 1 2 6) 1 4) ;'(1 5 4 3 2 6)
(Reverse '(1 2 3 4) 2 3)   ;'(1 3 2 4)
(display "\n")
;------------------------------------------------------------------------------------------------------------

;Line 2 

;'position-Breakpoints' returns a list that contains the position of all breakpoints in a list. The counting starts from 0, not from 1.
(define (position-Breakpoints l)
  
  (define (help-position l counter)
    (cond ((or (= counter (-  (length l) 1)) (null? l))  '())
          ((> (abs (- (list-ref l counter) (list-ref l (+ counter 1)))) 1)  (cons counter (help-position l (+ counter 1))))
          (else (help-position l (+ counter 1)))))
  
  (help-position l 0))
;-----------------------------

;'refine' contributes to the function of 'position-Breakpoints' at special cases (e.g when the list of of positions of breakpoints
;has only one element)
(define (refine l)
  
  (define (help-refine l l_break)
    (if (= (length l_break) 1)
        (if (= (length l) 2)
            (append (replace-nth l_break (car l_break) -1) (cons (- (length l) 1) '()))
            (append l_break (cons (- (length l) 1) '())))       
        l_break))
  
  (help-refine l (position-Breakpoints l)))
;------------------------------------------

;'decrOrder' returns true if a list contains the elements in decreasing order 
(define (decrOrder l)
  
  (if (and (equal? l (sort l >)) (= (length l) (length (sort l >))))
      #t
      #f))
;-----------------------------------------

;'HasDecrStrip' returns true if the list l includes at least one decreasing strip. It uses the two aforementioned functions ('refine' and
;'decrOrder').
(define (HasDecrStrip l)
  
  (define (help-DecrStrip l l_pos counter)
    
    (if (< counter (- (length l_pos) 1))
        (if (decrOrder (sub-list (+ (list-ref l_pos counter) 1) (list-ref l_pos (+ counter 1)) l))
             #t
             (help-DecrStrip l l_pos (+ counter 1)))
        #f))
    
  (help-DecrStrip l (refine l) 0))

;9 running examples
(display "---Line 2: HasDecrStrip--- \n")
(HasDecrStrip '(1 2 3 6 5)) ;#t
(HasDecrStrip '(1 2 3 5 6)) ;#f
(HasDecrStrip '(1 2 3 6 5 7 9)) ;#t
(HasDecrStrip '(1 2 3 7 9 6 5)) ;#t
(HasDecrStrip '()) ;#f
(HasDecrStrip '(2)) ;#f
(HasDecrStrip '(8 2)) ;#t
(HasDecrStrip '(2 8)) ;#f
(HasDecrStrip '(8 8)) ;#f
(display "\n")
;----------------------------------

;Line 5
;'FlipIncreasingStrip' is implemented by the help function 'help-Flip'. It takes as arguments a list l, an other list l_pos, which
;includes the positions of the breakpoints of l and a counter that checks if we have gone through the whole list.
(define (FlipIncreasingStrip l)
  
  (define (help-Flip l l_pos counter)
    
    (if (< counter (- (length l_pos) 1))
        (if (not(decrOrder (sub-list (+ (list-ref l_pos counter) 1) (list-ref l_pos (+ counter 1)) l)))
             (cons (+ (list-ref l_pos counter) 2) (+ (list-ref l_pos (+ counter 1)) 1))
             (help-Flip l l_pos (+ counter 1)))
        '()))
    
  (help-Flip l (refine l) 0))

;5 running examples
(display "---Line 5: FlipIncreasingStrip---\n")
(FlipIncreasingStrip '(0 2 3 1 4 5)) ;'(2 . 3)
(FlipIncreasingStrip '(0 3 4 5 1 2 6)) ;'(2 . 4)
(FlipIncreasingStrip '(0 2)) ;'(1 . 2)
(FlipIncreasingStrip '(1 2)) ;'(), There are no breakpoints. Thus, there are not any strips, either increasing or decreasing.
(FlipIncreasingStrip '(0)) ;'(), since there is only one element in the permutation π
(display "\n")
;-------------------------------------------------------------------------------------------------------------------------------


;--------------- Task 2 ---------------

;a.

; FindBestReversal
; Input: A list of numbers, representing a permutation of genes
; Output: A pair of numbers (i . j), the best reversal is between i and j
(define (FindBestReversal pi)
  ; Help procedure that goes through all combinations of i and j to test for the lowest # of breakpoints
  ; pi = permutation, i = index i, besti = index of i in best permutation
  ; j = index j, bestj = index of j in best permutation, b = number of breakpoints in pi
  (define (FindBestReversal-help pi i besti j bestj b)
    (if (< i (length pi)) ; Check that end isn't reached
        (let ((newb (Breakpoints (Reverse pi i j))) ; Number of breakpoints after current reversal
              (nexti (+ i (quotient j (- (length pi) 1)))) ; Calculate next i
              (nextj (+ 1 (remainder j (- (length pi) 1))))) ; Calculate next j
          (if (< newb b) ; Check if newb is lower than currently lowest b
              (FindBestReversal-help pi nexti i nextj j newb) ; Recursive call, saving the newly found best b
              (FindBestReversal-help pi nexti besti nextj bestj b))) ; Recursive call
        (cons besti bestj))) ; Return the best reversal
  (FindBestReversal-help pi 2 2 2 2 (Breakpoints pi)))

(display "---Task 2: Example of FindBestReversal--- \n")
(FindBestReversal '(0 3 4 1 2 5)) ;'(2 . 4)
(display "\n")
;----------------------------------------------------------

(define (AddEnds pi) ; Adds 0 and max+1 to the list.
   (cons 0 (append pi (list (+ 1 (length pi))))))
;-----------------------------------------------------


;'ImprovedBreakpointReversalSort' uses the help function 'ImprovedBreakpointReversalSort-help', which implements the pseudocode from Task 1
;making use of the procedures that are already mentioned and constructed.
(define (ImprovedBreakpointReversalSort pi)
  (display "\n")
  (define (ImprovedBreakpointReversalSort-help pi flag)
    
    (if (= flag 1) 
        (display pi)
        (display "Starting..."))
    
    (display "\n")
    (if (> (Breakpoints pi) 0) 
        
        (cond ((HasDecrStrip pi) (ImprovedBreakpointReversalSort-help (Reverse pi (car (FindBestReversal pi)) (cdr (FindBestReversal pi))) 1)) 
              (else (ImprovedBreakpointReversalSort-help (Reverse pi (car (FlipIncreasingStrip pi)) (cdr (FlipIncreasingStrip pi))) 1)))
        
        (display "Done. \n")))
  
  (ImprovedBreakpointReversalSort-help (AddEnds pi) 0))

;Running example provided by exercise
(display "---Task 2: Example Question a---\n")
(ImprovedBreakpointReversalSort '(6 1 2 3 4 5))     ; (0 5 4 3 2 1 6 7) ; (0 1 2 3 4 5 6 7) ; Done.
(display "\n")
  
;b.
(display "---Task 2: Question b---\n")
(ImprovedBreakpointReversalSort '(3 4 6 5 8 1 7 2))
(display "\n")

;The output of the running example of question b is:

;Starting...
;(0 3 4 6 5 8 7 1 2 9)
;(0 1 7 8 5 6 4 3 2 9)
;(0 1 2 3 4 6 5 8 7 9)
;(0 1 2 3 4 5 6 8 7 9)
;(0 1 2 3 4 5 6 7 8 9)
;Done.
;-------------------------------------------------------------------------------------------------------------------------------------------

;c.
;A permutation σ with no decreasing strips and no reversal such that reduces the number of breakpoints is: '(1 4 5 2 3 6). In this case, the  
;if-test of line 2 is absolutely needed, because there are no decreasing strips but only increasing. As a result, the 'if' condition is not 
;fulfilled, which means that the algorithm enters the 'else' condition. If it had not been for it, the algorithm would get stuck, but now it is 
;able to continue the execution by choosing a reversal that flips an increasing strip.
;---------------------------------------------------------------------------------------------------------------------------------------------

;d.
;Let us take the permutation pi =  1 4 5 2 3 6 again. The approximation algorithm with this pi as input, gives:
(display "---Task 2: Question d---\n")
(ImprovedBreakpointReversalSort '(1 4 5 2 3 6))
(display "\n")
;Starting...
;(0 1 5 4 2 3 6 7)
;(0 1 3 2 4 5 6 7)
;(0 1 2 3 4 5 6 7)
;Done.

;We see that 3 reversals are required in order to acquire the above final sorted list. Actually, this permutation could have a shorter path,
;if the reversals were made this way:
;ρ(3, 5) ---> reversal between the third and fifth element results in: (0 1 2 5 4 3 6 7)
;ρ(4, 6) ---> reversal between the forth and sixth element results in: (0 1 2 3 4 5 6 7)
 
;So, only 2 reversals are required, whereas the 'ImprovedBreakpointReversalSort' executes 3 reversals for the same permutation. 
;---------------------------------------------------------------------------------------------------------------------------------------------
