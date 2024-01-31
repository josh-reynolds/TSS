#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 11: Welcome Back to the Show

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))
; ------------------------------

; ------------------------------
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? (cdr lat)) #f)
      ((eq? (car lat) (car (cdr lat))) #t)
      (else (two-in-a-row? (cdr lat))))))
; ------------------------------

; ------------------------------
(define 2-in-a-row?           ; definition from the text
  (lambda (lat)               ; they took a different approach with a helper function
    (cond
      ((null? lat) #f)
      (else
       (or (is-first? (car lat) (cdr lat))
           (two-in-a-row? (cdr lat)))))))
; ------------------------------

; ------------------------------
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))
; ------------------------------

; ------------------------------
(define too-in-a-row?         ; revised version, with diff. helper
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (is-first-b? (car lat) (cdr lat))))))
; ------------------------------

; ------------------------------
(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (too-in-a-row? lat))))))
; ------------------------------

; ------------------------------
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? preceding (car lat))
                (two-in-a-row-b? (car lat) (cdr lat)))))))
; ------------------------------

; ------------------------------
(define 2-in-a-row-b?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))
; ------------------------------

(define list1
  (list 'Italian 'sardines 'spaghetti 'parsley))

(define list2
  (list 'Italian 'sardines 'sardines 'spaghetti 'parsley))

(define list3
  (list 'Italian 'sardines 'more 'sardines 'spaghetti 'parsley))