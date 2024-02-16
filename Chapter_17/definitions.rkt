#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 17: We Change, Therefore We Are!

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
