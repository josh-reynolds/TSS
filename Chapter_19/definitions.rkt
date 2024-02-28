#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 19: Absconding with the Jewels

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else
       (cons (deep (sub1 m)) '())))))
; ------------------------------

; ------------------------------
(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons p '())
         '())
        '())
       '())
      '())
     '())))
; ------------------------------

; ------------------------------
(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())))
; ------------------------------

; ------------------------------
(define toppings 0)           ; text just uses (define toppings)
                              ; but this gives a bad syntax error
; ------------------------------

; ------------------------------
(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (let/cc jump
         (set! toppings jump)
         'pizza))
      (else (cons (deepB (sub1 m)) '())))))
; ------------------------------