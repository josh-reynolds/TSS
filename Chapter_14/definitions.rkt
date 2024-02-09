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

(define list1
  (list (list (list 'a)
              'b)
        (list 'c 'd)))

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
  (list (list 'Swedish 'rye)
        (list 'French (list 'mustard 'salad 'turkey))
        'salad))

(define list5
  (list (list 'pasta 'meat)
        'pasta
        (list 'noodles 'meat 'sauce)
        'meat
        'tomatoes))