#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 13: Hop, Skip, and Jump

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
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
; ------------------------------

; ------------------------------
(define intersect2            ; using letrec
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
            (cond
              ((null? set) '())
              ((member? (car set) set2) (cons (car set) (I (cdr set))))
              (else (I (cdr set)))))))
         (I set1))))
; ------------------------------





(define set1
  (list 'tomatoes 'and 'macaroni))

(define set2
  (list 'macaroni 'and 'cheese))