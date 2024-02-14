#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 16: Ready, Set, Bang!

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake '()))))
; ------------------------------

; ------------------------------
(define last
  'angelfood)
; ------------------------------

; ------------------------------
(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons 'cake '()))))
; ------------------------------

; ------------------------------
(define ingredients
  '())
; ------------------------------

; ------------------------------
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons 'cake '()))))
; ------------------------------