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
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
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

; ------ working through example
;(2-in-a-row-b? list4)
;  (null? list4) #f
;  (else (two-in-a-row-b? (car list4) (cdr list4)))
;  (else (two-in-a-row-b? 'b (list 'd 'e 'i 'i 'a 'g)))
;    (null? (list 'd 'e 'i 'i 'a 'g)) #f
;    (else (or (eq? 'b (car (list 'd 'e 'i 'i 'a 'g)) (two-in-a-row-b? (car ...) (cdr ...)))))
;    (else (or (eq? 'b 'd) (two-in-a-row-b? 'd (list 'e 'i 'i 'a 'g))))
;    (else (or #f (two-in-a-row-b? 'd (list 'e 'i 'i 'a 'g))))
;                   (null? (list 'e 'i 'i 'a 'g)) #f
;                   (else (or (eq? 'd (car (list 'e 'i 'i 'a 'g))) (two-in-a-row-b? (car ...) (cdr ...))))
;                   (else (or (eq? 'd 'e) (two-in-a-row-b? 'e (list 'i 'i 'a 'g))))
;                   (else #f (two-in-a-row-b? 'e (list 'i 'i 'a 'g)))
;                              (null? (list 'i 'i 'a 'g)) #f
;                              (else (or (eq? 'e (car (list 'i 'i 'a 'g))) (two-in-a-row-b? (car ...) (cdr ...))))
;                              (else (or (eq? 'e 'i) (two-in-a-row-b? 'i (list 'i 'i 'a 'g))))
;                              (else (or #f (two-in-a-row-b? 'i (list 'i 'i 'a 'g))))
;                                             (null? (list 'i 'i 'a 'g)) #f
;                                             (else (or (eq? 'i (car (list 'i 'i 'a 'g))) (two-in-a-row-b? (car ...) (cdr ...))))
;                                             (else (or (eq? 'i 'i) (two-in-a-row-b? (car ...) (cdr ...))))
;                                             (else (or #t (two-in-a-row-b? (car ...) (cdr ...))))  ; short-circuit here?
;                                             #t
;                              (else (or #f #t))
;                              #t
;                   (else #f #t)
;                   #t
;    (else (or #f #t))
;    #t
; #t
; ------------------------------

; ------------------------------
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))
; ------------------------------

; ------------------------------
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else (cons (o+ sonssf (car tup))
                  (sum-of-prefixes-b (o+ sonssf (car tup)) (cdr tup)))))))
; ------------------------------


(define list1
  (list 'Italian 'sardines 'spaghetti 'parsley))

(define list2
  (list 'Italian 'sardines 'sardines 'spaghetti 'parsley))

(define list3
  (list 'Italian 'sardines 'more 'sardines 'spaghetti 'parsley))

(define list4
  (list 'b 'd 'e 'i 'i 'a 'g))

(define tup1
  (list 2 1 9 17 0))

(define tup2
  (list 1 1 1 1 1))