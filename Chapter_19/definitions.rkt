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

; ------------------------------
(define leave 0)              ; text just uses (define leave)
                              ; but this gives a bad syntax error
; ------------------------------

; ------------------------------
(define walk
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (leave (car l)))
      (else
       (let ()
         (walk (car l))
         (walk (cdr l)))))))
; ------------------------------

; ------------------------------
(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else
                    (let ()
                      (lm (car l))
                      (lm (cdr l))))))))
        (lm l)))))
; ------------------------------

; ------------------------------
(define start-it
  (lambda (l)
    (let/cc here
      (set! leave here)
      (walk l))))
; ------------------------------

; ------------------------------
(define fill 0)               ; text just uses (define fill)
                              ; but this gives a bad syntax error
; ------------------------------

; ------------------------------
(define waddle
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (let ()
                         (let/cc rest
                           (set! fill rest)
                           (leave (car l)))
                         (waddle (cdr l))))
      (else (let ()
              (waddle (car l))
              (waddle (cdr l)))))))
; ------------------------------

; ------------------------------
(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))
; ------------------------------

(define list1
  (list 'a 'a 'b 'c))

(define list2
  (list 'a 'b 'c))

(define list3
  (list 'a 'b 'b 'c))

(define list4
  (list (list 'potato)
        (list 'chips
              (list 'chips
                    (list 'with)))
        'fish))

(define list5
  (list (list 'donuts)
        (list 'cheerios
              (list 'cheerios
                    (list 'spaghettios)))
        'donuts))