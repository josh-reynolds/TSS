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

; ------------------------------
(define intersectall          ; assumes lset is not empty
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset) (intersectall (cdr lset)))))))
; ------------------------------

; ------------------------------
(define intersectall2
  (lambda (lset)
    (cond
      ((null? lset) '())
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset) (intersectall (cdr lset)))))))
; ------------------------------

; ------------------------------
(define intersectall3
  (lambda (lset)
    (letrec
        ((intersectall (lambda (lset)
                        (cond
                          ((null? (cdr lset)) (car lset))
                          (else (intersect (car lset) (intersectall (cdr lset))))))))
         (cond
           ((null? lset) '())
           (else (intersectall lset))))))   
; ------------------------------

; ------------------------------
(define intersectall4
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
                        (cond
                          ((null? (cdr lset)) (car lset))
                          (else (intersect (car lset) (A (cdr lset))))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))   
; ------------------------------

(define set1
  (list 'tomatoes 'and 'macaroni))

(define set2
  (list 'macaroni 'and 'cheese))

(define set3
  (list 'and 'foo 'bar))