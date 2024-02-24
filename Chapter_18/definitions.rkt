#lang racket
(require rnrs/mutable-pairs-6)
; ------------------------------
; The Seasoned Schemer
; Chapter 18: We Change, Therefore We Are the Same! 

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define counter 0)
(define set-counter 0)
(define consC
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
; ------------------------------

; ------------------------------
(define lots                  ; they use "kons" instead of "cons"
  (lambda (m)
    (cond
      ((zero? m) '())
      (else (cons 'egg
                  (lots (sub1 m)))))))
; ------------------------------

; ------------------------------
(define lenkth                ; they use "kdr" instead of "cdr"
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (cdr l)))))))
; ------------------------------

; ------------------------------
(define add-at-end            ; they use "kar/kdr/kons"
  (lambda (l)                 ; instead of "car/cdr/cons"
    (cond
      ((null? (cdr l)) (consC (car l)
                              (cons 'egg '())))
      (else (consC (car l)
                   (add-at-end (cdr l)))))))
; ------------------------------

; set-cdr! is not defined in core Racket
; https://stackoverflow.com/questions/9475366/set-car-set-cdr-unbound-in-racket
; adding (require rnrs/mutable-pairs-6) seems to work
; ------------------------------
(define add-at-end-too        ; they use "kdr/kons"
  (lambda (l)                 ; instead of "cdr/cons"
    (letrec                   ; and "set-kdr" instead of "set-cdr!"
        ((A (lambda (ls)
              (cond
                ((null? (cdr ls)) (set-cdr! ls (cons 'egg '())))
                (else (A (cdr ls)))))))
    (A l)
    l)))
; ------------------------------

; getting contract violation from set-mcdr! with previous definition
; need to debug

;(add-at-end-too (lots 1))
;(add-at-end-too '(egg))
;(A '(egg))
;  (null? (cdr '(egg)))
;  (null? '())  #t
;    (set-cdr! '(egg) (cons 'egg '()))
;    (set-cdr! '(egg) '(egg))   <--- contract violation
;                                      expected mpair?

(mpair? (lots 1))   ; will return false

; from the documentation, mpair is a mutable pair
; which is not the same thing as a pair or list
; text seems to dodge this point

; maybe we need a version of lots that returns mpair? try it
; (and is this secret behind kar/kdr/kons ?)

; ------------------------------
(define lotsM                  
  (lambda (m)
    (cond
      ((zero? m) '())
      (else (mcons 'egg
                  (lotsM (sub1 m)))))))
; ------------------------------

;(add-at-end-too (lotsM 1))  <-- will also fail because we are using cons
;                                need to rewrite add-at-end-too as well

; ------------------------------
(define add-at-end-tooM
  (lambda (l)         
    (letrec           
        ((A (lambda (ls)
              (cond
                ((null? (mcdr ls)) (set-mcdr! ls (mcons 'egg '())))
                (else (A (mcdr ls)))))))
    (A l)
    l)))
; ------------------------------

; this now works:
(add-at-end-tooM (lotsM 1))

(define list1
  (list 'apple 'pear 'peach 'watermelon 'orange))


