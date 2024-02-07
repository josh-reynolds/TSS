#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 14: Let There Be Names

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
