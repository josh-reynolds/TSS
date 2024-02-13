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
