#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 18: We Change, Therefore We Are the Same! 

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define lots                  ; they use "kons" instead of "cons"
  (lambda (m)
    (cond
      ((zero? m) '())
      (else (cons 'egg
                  (lots (sub1 m)))))))
; ------------------------------

; ------------------------------
(define lenkth                ; they use "kdr" instead of "cdr"
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (cdr l)))))))
; ------------------------------
