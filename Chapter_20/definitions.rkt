#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 20: What's in Store? 
; ------------------------------

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------
