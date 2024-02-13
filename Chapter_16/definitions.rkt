#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 16: Ready, Set, Bang!

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
