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
(define counter 0)
(define set-counter 0)
(define consC
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
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

; ------------------------------
(define add-at-end            ; they use "kar/kdr/kons"
  (lambda (l)                 ; instead of "car/cdr/cons"
    (cond
      ((null? (cdr l)) (consC (car l)
                              (cons 'egg '())))
      (else (consC (car l)
                   (add-at-end (cdr l)))))))
; ------------------------------
