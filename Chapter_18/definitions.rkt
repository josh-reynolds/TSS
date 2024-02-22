#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 18: We Change, Therefore We Are the Same! 

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

