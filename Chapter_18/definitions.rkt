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
(add-at-end-tooM (lotsM 3))

; ------------------------------
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))
; ------------------------------

; ------------------------------
(define kar
  (lambda (c)
    (c (lambda (a d) a))))
; ------------------------------

; ------------------------------
(define kdr
  (lambda (c)
    (c (lambda (a d) d))))
; ------------------------------

; ------------------------------
(define bons
  (lambda (kar2)
    (let ((kdr2 '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr2 x))
         (kar2
          kdr2))))))
; ------------------------------

; ------------------------------
(define kar2
  (lambda (c)
    (c (lambda (s a d) a))))
; ------------------------------

; ------------------------------
(define kdr2
  (lambda (c)
    (c (lambda (s a d) d))))
; ------------------------------

; ------------------------------
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))
; ------------------------------

; ------------------------------
(define kons2
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))
; ------------------------------

; ------------------------------
(define lots2
  (lambda (m)
    (cond
      ((zero? m) '())
      (else (kons2 'egg
                  (lots2 (sub1 m)))))))
; ------------------------------

; ------------------------------
;(define dozen (lots2 12))
; ------------------------------

; ------------------------------
(define add-at-end2           ; they use "kar/kdr/kons"
  (lambda (l)                 ; instead of "car/cdr/cons"
    (cond
      ((null? (kdr l)) (consC (kar l)
                              (kons 'egg '())))
      (else (consC (kar l)
                   (add-at-end2 (kdr l)))))))
; ------------------------------

; evaluating the following results in an error:
; cdr: contract violation
; expected: pair?
; from add-at-end
; is this a pair?/mpair? issue again
; fixed by creating add-at-end2 which uses kar/kdr/kons
; ------------------------------
;(define bakers-dozen (add-at-end2 dozen))
; ------------------------------

; applying these is getting odd results, not matching text
; I probably have something crossed up here - needs debugging

; ------------------------------
;(define bakers-dozen-too
;  (add-at-end-tooM dozen))
; ------------------------------

; ------------------------------
;(define bakers-dozen-again
;  (add-at-end dozen))
; ------------------------------

; ------------------------------
(define eklist?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) (null? ls2))
      ((null? ls2) #f)
      (else
       (and (eq? (kar2 ls1) (kar2 ls2))
            (eklist? (kdr2 ls1) (kdr2 ls2)))))))
; ------------------------------

(define list1
  (list 'apple 'pear 'peach 'watermelon 'orange))


