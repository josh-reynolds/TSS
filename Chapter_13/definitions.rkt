#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 13: Hop, Skip, and Jump

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
