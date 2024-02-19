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

; ------------------------------
(define deepM4
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result ((lambda (m)
                             (if (zero? m)
                                 'pizza
                                 (cons (deepM (sub1 m)) '())))
                          n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
; ------------------------------

; ------------------------------
(define deepM5
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (let ((m n))
                     (if (zero? m)
                         'pizza
                         (cons (deepM (sub1 m)) '())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns ))
              result)
            exists)))))
; ------------------------------

; ------------------------------
(define deepM6
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       'pizza
                       (cons (deepM (sub1 n)) '()))
                   ))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
; ------------------------------

; ------------------------------
(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
; ------------------------------

; ------------------------------
(define deep2
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep2 (sub1 m))
               '()))))
; ------------------------------

; text just has (define counter)
; but that results in a syntax error
; ------------------------------
(define counter 0)
; ------------------------------

; ------------------------------
(define consC2
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
; ------------------------------

; ------------------------------
(define deep3
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC2 (deep3 (sub1 m))
               '()))))
; ------------------------------

; ------------------------------
(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000))))
; ------------------------------

; ------------------------------
(define supercounter2
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))
; ------------------------------
