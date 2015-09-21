#lang racket

;--- Chantzi Efthymia
;--- DrRacket, version 6.1


;------------------------------------------->   X1: SCHEME BASICS   <-------------------------------------------------
;-----> Detailed Exercise Instructions: https://drive.google.com/file/d/0B4mMtieYoaNDSTB1NzdzV1hkelk/view?usp=sharing <----


;---Task 1: Introduction---

;Expressions              ;Results
;1.
2                         ;2

;2.
12.1234                   ;12.1234

;3.
(+ 2 1)                   ;3

;4.
(+ 2 1.0)                 ;3.0

;5.
(+ (* 2 3) 4)             ;10

;6.
(define age-of-adam 23)   ;undefined, no return value

;7.
(define age-of-eva 24)    ;undefined, no return value

;8.
(> age-of-adam age-of-eva) ;#f

;9.
(define x 1.4142)          ;undefined, no return value

;10.
(define y (* x x))         ;undefined, no return value

;11.
y                          ;1.9999616399999998

;12.
(+ x y)                    ;3.4141616399999997

;13.
+                          ;#<procedure:+>

;14.
(+)                        ;0

;15.
(lambda (a b) (+ (* a a) (* b b))) ;procedure

;16.
((lambda (a) (+ a a)) 5)    ;10

;17.
((lambda (x) (* 2 x)) x)    ;2.8284

;18.
(define (foo arg) (+ arg 3))  ;undefined, no return value

;19.
;(foo)                        ;Error--> "foo: arity mismatch; the expected number of arguments does not match the given number expected: 1 given: 0"

;20.
(define (fee) (+ x 5) x)      ;undefined, no return value

;21.
(fee)                         ;1.4142

;22.
(define (fi arg) (* 2 arg) (+ 3 arg)) ;undefined, no return value

;23
;(fi)                               ;Error---> "fi: arity mismatch; the expected number of arguments does not match the given number expected: 1 given: 0"

;I have put expressions: 19.(foo) and 23.(fi) in comments, so as not to disturb the execution of the programme since they contain errors. 
;------------------------------------------------------------------------------------------------------------------------------------------------



;---Task 2: Lambda Expressions---


;1
(lambda (x) x) ;it just returns the argument that takes as an input
;----------------------------------------------------------------------------------

;2
(lambda () 2) ;no arguments, it just returns the constant number "2"
;----------------------------------------------------------------------------------

;3
(lambda (a b) (+ a b)) ;it takes two arguments, a and b, and it returns the summation of these two '(a + b)'
;-----------------------------------------------------------------------------------------------------------------

;4
(define math (lambda (a b) ((lambda (a) (+ b a)) (+ a 1)))) ;This is a compound procedure, which takes two arguments: 'a' and 'b'. As a result, during the evaluation
;of its body, each formal parameter is replaced by the actual argument value. Firstly, the inner expression-summation '(a + 1)' is evaluated, which then becomes the  
;value of the argument 'a' for the inner lambda expression. In this way, the summation '(b + a)' is estimated and the outer lambda formula terminates, after having   
;executed its inner actions. 
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------

;I have used 'define' in these lambda formulae, in order to be able to execute them directly
;1
(define mathform1 (lambda (x) (sqrt(* x x))))
;----------------------------------------------

;2
(define mathform2 (lambda (b h) (/ (* b h) 2)))
;----------------------------------------------

;3
(define mathform3 (lambda (a b) (sqrt(+ (* a a) (* b b)))))
;----------------------------------------------------------

;4
(define mathform4 (lambda (celcius) (+ (* celcius 1.8) 32)))
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------



;---Task 3: Fahrenheit---

(define  (fahr-to-celcius temp)
  (/ (- temp 32) 1.8))
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------



;---Task 4:How many pins?---

;1
(define (number-of-pins-rec rows)
  (cond ((< rows 0) (display "Invalid input"))
        ((= rows 1) rows)
        ((= rows 0) 0)
        (else (+ rows (number-of-pins-rec(- rows 1))))))
;------------------------------------------------------------

;2
(define (number-of-pins-it rows)
  
  (define (number-of-pins-it-help total product counter)          
    (cond ((= counter 0) product)
        ((< counter 0) (display "Invalid input"))
        (else (number-of-pins-it-help total (+ product counter) (- counter 1)))))

  (number-of-pins-it-help rows 0 rows))
;-----------------------------------------------------------------------------------

;3. ---Substitution Model---

;The substitution model for (number-of-pins-of-rec 4) is:

;step 1: (number-of-pins-of-rec 4)
;step 2: (4 + (number-of-pins-of-rec 3))
;step 3: (4 + (3 + (number-of-pins-of-rec 2)))
;step 4: (4 + (3 + (2 + (number-of-pins-of-rec 1))))
;step 5: (4 + (3 + (2 + 1)))
;step 6: (4 + (3 + 3))
;step 7: (4 + 6)
;step 6: 10
;------------------------------------------------------------

;The substitution model for (number-of-pins-of-it 4) is:

;step 1: (number-of-pins-of-it 4)
;        (4 0 4)
;step 2: (number-of-pins-of-it 3)
;        (4 4 3)
;step 3: (number-of-pins-of-it 2)
;        (4 7 2)
;step 4: (number-of-pins-of-it 1)
;        (4 9 1)
;step 5: (number-of-pins-of-it 0)
;        (4 10 0)

;         10
;------------------------------------------------------------


;---Task 5:Exponentiation---

(define (my-expt base power)
  (cond ((= power 0) 1)
        ((< power 0) (my-expt (/ 1 base) (abs power)))
        (else(* base (my-expt base (- power 1))))))
;------------------------------------------------------------


(define (square x) (* x x))  

(define (fast-expt base power)
  (if (even? power)
      (my-expt (square base) (/ power 2))
      (* base (my-expt (square base)  (/ (- power 1)  2)))))
;----------------------------------------------------------------------------------------------------------------------------------------------------


;---Task 6: testing for primality---


(define (prime number)
  
  (define (prime-help number divisor)
    (cond ((= divisor 1) (display "Prime number"))
          ((< divisor 1) (display "Non-prime by definition. Number must be greater than 1."))
          ((= (modulo number divisor) 0) (display "Non-prime. Right after itself, ") (display number) (display "  is divided by:  ") (display  divisor))
          (else (prime-help number (- divisor 1)))))

      (prime-help number (- number 1)))
;----------------------------------------------------------------------------------------------------------------------------------------------------------

;Fermat test procedure
(define (fermat-help n a t)
  (if (> t 0) 
      (if (not(= (remainder (my-expt a n) n) a)) 
          (display "Non-prime by Fermat test")
          (fermat-help n (+ 1 (random(- n 1))) (- t 1)))
      (display "Prime by Fermat test")))

(define (fermat n t) 
  (cond ((or (<= t 0) (<= n 1))(display "Invalid argument(s)"))
       (else(fermat-help n (+ 1 (random(- n 1))) t))))

;-----------------------------------------------------------------------------------------------------------------------------------------------------------



