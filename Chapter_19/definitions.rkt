#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 19: Absconding with the Jewels

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
