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



(define list1
  (list 'Italian 'sardines 'spaghetti 'parsley))

(define list2
  (list 'Italian 'sardines 'sardines 'spaghetti 'parsley))

(define list3
  (list 'Italian 'sardines 'more 'sardines 'spaghetti 'parsley))