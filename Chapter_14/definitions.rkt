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