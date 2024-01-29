#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 11: Welcome Back to the Show

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------