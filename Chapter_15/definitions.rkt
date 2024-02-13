#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 15: The Difference Between Men and Boys...

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define x
  (cons 'chicago
        (cons 'pizza
              '())))
; ------------------------------

; ------------------------------
(set! x 'gone)
; ------------------------------

; ------------------------------
(set! x 'skins)
; ------------------------------

; ------------------------------
(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))
; ------------------------------

; ------------------------------
(set! x 'rings)
; ------------------------------

; ------------------------------
(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))
; ------------------------------