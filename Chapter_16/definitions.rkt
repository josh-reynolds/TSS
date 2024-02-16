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

; ------------------------------
(define deepM2
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result))))
; ------------------------------

; ------------------------------
(define deep2
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deepM3 (sub1 m)) '())))))
; ------------------------------

; ------------------------------
(define deepM3
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep2 n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result))))
; ------------------------------

; ------------------------------
(define deep3
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deepM4 (sub1 m)) '())))))
; ------------------------------

; ------------------------------
(define deepM4
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (if (member? n Ns)
          (find2 n Ns Rs)
          (let ((result (deep3 n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons result Rs))
            result)))))
; ------------------------------

; ------------------------------
(define find2
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((= (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
         (A Ns Rs))))
; ------------------------------

; ------------------------------
(define deepM5
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find2 n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep3 n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))
; ------------------------------

; ------------------------------
(define length
  (lambda (l)
    0))
; ------------------------------

; ------------------------------
(set! length
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
; ------------------------------

; ------------------------------
(define length2
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (h (cdr l)))))))
    h))
; ------------------------------

; ------------------------------
(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))
; ------------------------------

; ------------------------------
(define length3
  (let ((h (lambda (l) 0)))
    (set! h
          (L (lambda (arg) (h arg))))
    h))
; ------------------------------

; ------------------------------
; ------------- working examples
;(lambda (l)
;      (cond
;        ((null? l) 0)
;        (else (add1
;               ((lambda (arg) (h arg))
;                (cdr l))))))
; ------------------------------