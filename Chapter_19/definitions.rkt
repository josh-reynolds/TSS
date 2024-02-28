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
(define deepB
  (lambda (m)
    (cond
;      ((zero? m)
;       ... (set! toppings ...) ...)
      (else (cons (deepB (sub1 m)) '())))))
; ------------------------------