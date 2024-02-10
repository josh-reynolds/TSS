#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 14: Let There Be Names

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
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
(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
; ------------------------------

; ------------------------------
(define one?
  (lambda (n)
    (= n 1)))
; ------------------------------

; ------------------------------
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
; ------------------------------

; ------------------------------
(define leftmost2
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
       (cond
         ((atom? (leftmost2 (car l))) (leftmost2 (car l)))
         (else (leftmost2 (cdr l))))))))
; ------------------------------

; ------------------------------
(define leftmost3
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost3 (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost3 (cdr l)))))))))
; ------------------------------

; ------------------------------
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (cdr l))
         (else (cons (car l) (rember1* a (cdr l))))))
      (else
       (cond
         ((eqlist? (rember1* a (car l)) (car l))
          (cons (car l) (rember1* a (cdr l))))
         (else (cons (rember1* a (car l)) (cdr l))))))))
; ------------------------------

; ------------------------------
(define rember1*2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? a (car l)) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (cond
                   ((eqlist? (R (car l)) (car l))
                    (cons (car l) (R (cdr l))))
                   (else (cons (R (car l)) (cdr l)))))))))
         (R l))))
; ------------------------------

; ------------------------------
(define rember1*3
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? a (car l)) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? av (car l))
                      (cons (car l) (R (cdr l))))
                     (else (cons av (cdr l))))))))))
         (R l))))
; ------------------------------

; ------------------------------
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
       (cond
         ((> (depth* (cdr l)) (add1 (depth* (car l))))
          (depth* (cdr l)))
         (else (add1 (depth* (car l)))))))))
; ------------------------------

; ------------------------------
(define depth*2               ; will fail for list9 during let definition
  (lambda (l)
    (let ((a (add1 (depth*2 (car l))))
          (d (depth*2 (cdr l))))
      (cond
        ((null? l) 1)
        ((atom? (car l)) d)
        (else
         (cond
           ((> d a) d)
           (else a)))))))
; ------------------------------

; ------------------------------
(define depth*3
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*3 (cdr l)))
      (else
       (let ((a (add1 (depth*3 (car l))))
             (d (depth*3 (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))
; ------------------------------

; ------------------------------
(define depth*4
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth*4 (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (let ((a (add1 (depth*4 (car l)))))
              (cond
                ((> d a) d)
                (else a))))))))))
; ------------------------------

; ------------------------------
(define depth*5
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*5 (cdr l)))
      (else
       (let ((a (add1 (depth*5 (car l))))
             (d (depth*5 (cdr l))))
         (if (> d a) d a))))))
; ------------------------------

; ------------------------------
(define depth*6
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*6 (cdr l)))
      (else
       (let ((a (add1 (depth*6 (car l))))
             (d (depth*6 (cdr l))))
         (max a d))))))
; ------------------------------

; ------------------------------
(define depth*7
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*7 (cdr l)))
      (else
       (max
        (add1 (depth*7 (car l)))
        (depth*7 (cdr l)))))))
; ------------------------------

; ------------------------------
(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) '())
                (else
                 (let ((rp (cons (car tup) rp)))
                   (cons (pick (car tup) rp)
                         (P (cdr tup) rp))))))))
      (P tup '()))))
; ------------------------------

; ------------------------------
(define leftmost4
  (lambda (l)
    (let/cc skip
      (lm l skip))))
; ------------------------------

; ------------------------------
(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l)) (out (car l)))
      (else
       (let ()                ; could also use begin
         (lm (car l) out)
         (lm (cdr l) out))))))
; ------------------------------

(define list1
  (list (list (list 'a)
              'b)
        (list 'c
              'd)))

(define list2
  (list (list (list 'a)
              '())
        '()
        (list 'e)))

(define list3
  (list (list (list '()
                    'a)
              '())))

(define list4
  (list (list 'Swedish
              'rye)
        (list 'French
              (list 'mustard
                    'salad
                    'turkey))
        'salad))

(define list5
  (list (list 'pasta
              'meat)
        'pasta
        (list 'noodles
              'meat
              'sauce)
        'meat
        'tomatoes))

(define list6
  (list (list 'pickled)
        'peppers
        (list 'peppers
              'pickled)))

(define list7
  (list 'margarine
        (list (list 'bitter
                    'butter)
              (list 'makes)
              (list 'batter
                    (list 'bitter)))
        'butter))

(define list8
  (list 'c
        (list 'b
              (list 'a 'b)
              'a)
        'a))

(define list9
  (list '()
        (list (list 'bitter
                    'butter)
              (list 'makes)
              (list 'batter
                    (list 'bitter)))
        'butter))

(define list10
  (list (list (list 'a))
        'b
        (list 'c)))