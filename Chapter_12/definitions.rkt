#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 12: Take Cover

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
