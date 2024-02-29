#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 19: Absconding with the Jewels

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else
       (cons (deep (sub1 m)) '())))))
; ------------------------------

; ------------------------------
(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons p '())
         '())
        '())
       '())
      '())
     '())))
; ------------------------------

; ------------------------------
(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p '())
       '())
      '())
     '())))
; ------------------------------

; ------------------------------
(define toppings 0)           ; text just uses (define toppings)
                              ; but this gives a bad syntax error
; ------------------------------

; ------------------------------
(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (let/cc jump
         (set! toppings jump)
         'pizza))
      (else (cons (deepB (sub1 m)) '())))))
; ------------------------------

; ------------------------------
(define deep&co
  (lambda (m k)
    (cond
      ((zero? m) (k 'pizza))
      (else
       (deep&co (sub1 m)
                (lambda (x)
                  (k (cons x '()))))))))
; ------------------------------

; ------------------------------
(define two-layers
  (lambda (p)
    (cons
     (cons p '())
     '())))
; ------------------------------

; ------------------------------
(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m) (let ()
                   (set! toppings k)
                   (k 'pizza)))
      (else (deep&coB (sub1 m) (lambda (x)
                                 (k (cons x '()))))))))
; ------------------------------

; ------------------------------
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))
; ------------------------------

; ------------------------------
(define two-in-a-row-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (two-in-a-row-b? (car lat) (cdr lat)))))))
; ------------------------------

; ------------------------------
(define two-in-a-row2?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else
               (let ((nxt (car lat)))
                 (or (eq? nxt a)
                     (W nxt (cdr lat)))))))))
    (lambda (lat)
             (cond
               ((null? lat) #f)
               (else (W (car lat) (cdr lat)))))))
; ------------------------------

(define list1
  (list 'a 'a 'b 'c))

(define list2
  (list 'a 'b 'c))

(define list3
  (list 'a 'b 'b 'c))
