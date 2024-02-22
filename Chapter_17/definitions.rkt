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

; text just has (define set-counter)
; but that results in a syntax error
; ------------------------------
(define set-counter 0)
; ------------------------------

; ------------------------------
(define consC3
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
; ------------------------------

; ------------------------------
(define deep4
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC3 (deep4 (sub1 m))
               '()))))
; ------------------------------

; ------------------------------
(define deepM7
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       'pizza
                       (consC3 (deepM7 (sub1 n)) '()))
                   ))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))
; ------------------------------

; ------------------------------
(define rember1*
  (lambda (a l)
    (letrec ((R (lambda (l oh)
                  (cond
                    ((null? l) (oh 'no))
                    ((atom? (car l)) (if (eq? a (car l))
                                         (cdr l)
                                         (cons (car l) (R (cdr l) oh))))
                    (else
                     (let ((new-car
                            (let/cc oh
                              (R (car l) oh))))
                       (if (atom? new-car)
                           (cons (car l) (R (cdr l) oh))
                           (cons (new-car (cdr l))))))))))
             (let ((new-l (let/cc oh (R l oh))))
               (if (atom? new-l)
                   l
                   new-l)))))
; ------------------------------

; ------------------------------
(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
                ((null? l) (oh 'no))
                ((atom? (car l)) (if (eq? (car l) a)
                                    (cdr l)
                                    (consC3 (car l) (R (cdr l) oh))))
                (else
                 (let ((new-car (let/cc oh (R (car l) oh))))
                   (if (atom? new-car)
                       (consC3 (car l) (R (cdr l) oh))
                       (consC3 new-car (cdr l)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))
; ------------------------------

; ------------------------------
(define rember1*2            ; actually the first version...
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l)) (if (eq? (car l) a)
                                     (cdr l)
                                     (cons (car l) (R (cdr l)))))
                (else
                 (let ((av (R (car l))))
                   (if (eqlist? (car l) av)
                       (cons (car l)
                             (R (cdr l)))
                       (cons av (cdr l)))))))))
      (R l))))
; ------------------------------

; ------------------------------
(define o-equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)(atom? s2)) (equan? s1 s2))
      ((or (atom? s1)(atom? s2)) #f)
      (else (eqlist? s1 s2)))))
; ------------------------------

; ------------------------------
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2)) #t)
      ((or (null? l1)(null? l2)) #f)
      (else (and (o-equal? (car l1)(car l2))
                 (o-equal? (cdr l1)(cdr l2)))))))
; ------------------------------

; ------------------------------
(define equan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2))(= a1 a2))
      ((or (number? a1)(number? a2)) #f)
      (else (eq? a1 a2)))))
; ------------------------------

; ------------------------------
(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l)) (if (eq? (car l) a)
                                     (cdr l)
                                     (consC3 (car l) (R (cdr l)))))
                (else
                 (let ((av (R (car l))))
                   (if (eqlist? (car l) av)
                       (consC3 (car l) (R (cdr l)))
                       (consC3 av (cdr l)))))))))
      (R l))))
; ------------------------------


(define list1
  (list (list 'food)
        'more
        (list 'food)))
