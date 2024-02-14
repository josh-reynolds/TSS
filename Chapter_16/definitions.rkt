#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 16: Ready, Set, Bang!

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
(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake '()))))
; ------------------------------

; ------------------------------
(define last
  'angelfood)
; ------------------------------

; ------------------------------
(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons 'cake '()))))
; ------------------------------

; ------------------------------
(define ingredients
  '())
; ------------------------------

; ------------------------------
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
          (cons 'cake '()))))
; ------------------------------

; ------------------------------
(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deep (sub1 m)) '())))))
; ------------------------------

; ------------------------------
(define Ns '())
; ------------------------------

; ------------------------------
(define Rs '())
; ------------------------------

; ------------------------------
(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      result)))
; ------------------------------

; ------------------------------
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((= (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
         (A Ns Rs))))
; ------------------------------

; ------------------------------
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))
; ------------------------------