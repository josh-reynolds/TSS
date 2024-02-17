#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 17: We Change, Therefore We Are!

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m))
              '()))))
; ------------------------------

; ------------------------------
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((= (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
         (A Ns Rs))))
; ------------------------------

; typo on p. 127? they have 'RS' in the exists/find line
; instead of 'Rs' - correcting here
; ------------------------------
(define deepM
  (let ((Rs '())
        (Ns '()))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m))
                        '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))
; ------------------------------

; ------------------------------
(define deepM2
  (let ((Rs '())
        (Ns '()))
    (let
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m))
                        '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))
; ------------------------------

; ------------------------------
(define deepM3
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
; ------------------------------
