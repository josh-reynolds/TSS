#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 12: Take Cover

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
;(define multirember
;  (lambda (a lat)
;    ((Y (lambda (mr)
;          (lambda (lat)
;            (cond
;              ((null? lat) '())
;              ((eq? a (car lat)) (mr (cdr lat)))
;              (else (cons (car lat) (mr (cdr lat))))))))
;     lat)))   
; ------------------------------

; ------------------------------
;(define ???
;  ((lambda (le)
;     ((lambda (f) (f f))
;        (lambda (f)
;          (le (lambda (x) ((f f) x)))))
;     (lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l))))))))))
; ------------------------------

; ------------------------------
;(define length
;  (Y (lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l)))))))))
; ------------------------------