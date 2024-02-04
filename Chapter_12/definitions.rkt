#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 12: Take Cover

; ------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
;(define multirember
;  (lambda (a lat)
;    ((Y (lambda (mr)
;          (lambda (lat)
;            (cond
;              ((null? lat) '())
;              ((eq? a (car lat)) (mr (cdr lat)))
;              (else (cons (car lat) (mr (cdr lat))))))))
;     lat)))   
; ------------------------------

; ------------------------------
;(define ???
;  ((lambda (le)
;     ((lambda (f) (f f))
;        (lambda (f)
;          (le (lambda (x) ((f f) x)))))
;     (lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l))))))))))
; ------------------------------

; ------------------------------
;(define length
;  (Y (lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l)))))))))
; ------------------------------

; ------------------------------
(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? a (car lat)) (mr (cdr lat)))
                  (else (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))
; ------------------------------

; following version will not work - 'a' not defined in mr
; ------------------------------
;(define multirember
;  (lambda (a lat)
;    (mr lat)))
; ------------------------------

; ------------------------------
;(define mr
;  (lambda (lat)
;    (cond
;      ((null? lat) '())
;      ((eq? a (car lat)) (mr (cdr lat)))
;      (else (cons (car lat) (mr (cdr lat)))))))
; ------------------------------

; ------------------------------
(define multirember2
  (lambda (a lat)
    (letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? a (car lat)) (mr (cdr lat)))
                  (else (cons (car lat) (mr (cdr lat))))))))
          (mr lat))))
; ------------------------------

; ------------------------------
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))
; ------------------------------

; ------------------------------
(define rember-eq? (rember-f eq?))
; ------------------------------

; ------------------------------
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))
; ------------------------------

; ------------------------------
(define multirember-f2
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) '())
              ((test? a (car lat)) (m-f a (cdr lat)))
              (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))
; ------------------------------

; ------------------------------
(define multirember3
  (letrec
      ((mr
        (lambda (a lat)
          (cond
            ((null? lat) '())
            ((eq? a (car lat)) (mr a (cdr lat)))
            (else (cons (car lat) (mr a (cdr lat))))))))
    mr))
; ------------------------------

; ------------------------------
(define multirember4
  (letrec
      ((multirember
        (lambda (a lat)
          (cond
            ((null? lat) '())
            ((eq? a (car lat)) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat))))))))
    multirember))
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
(define member2?              ; my version - book works out differently
  (letrec                     ; see below
      ((m?
        (lambda (a lat)
          (cond
            ((null? lat) #f)
            ((eq? a (car lat)) #t)
            (else (m? a (cdr lat)))))))
    m?))
; ------------------------------

; ------------------------------
(define member3?              ; first example on p. 27
  (lambda (a lat)
    ((letrec
        ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? a (car l)) #t)
                   (else (yes? (cdr l)))))))
       yes?)
     lat)))
; ------------------------------

; ------------------------------
(define member4?              ; second example on p. 27
  (lambda (a lat)
    (letrec
        ((yes?
          (lambda (l)
            (cond
              ((null? l) #f)
              ((eq? a (car l)) #t)
              (else (yes? (cdr l)))))))
      (yes? lat))))
; ------------------------------

; ------------------------------
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
; ------------------------------

; ------------------------------
(define union2
  (lambda (set1 set2)
    (letrec
        ((U
          (lambda (s)
            (cond
              ((null? s) set2)
              ((member? (car s) set2) (U (cdr s)))
              (else (cons (car s) (U (cdr s))))))))
      (U set1))))
; ------------------------------

; ------------------------------
(define union3
  (lambda (set1 set2)
    (letrec
        ((A
          (lambda (set)
            (cond
              ((null? set) set2)
              ((member? (car set) set2) (A (cdr set)))
              (else (cons (car set) (A (cdr set))))))))
      (A set1))))
; ------------------------------

; ------------------------------
(define member5?
  (lambda (lat a)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member5? (cdr lat) a)))))
; ------------------------------

; ------------------------------
(define union4
  (lambda (set1 set2)
    (letrec
        ((U
          (lambda (set)
            (cond
              ((null? set) set2)
              ((member? (car set) set2) (U (cdr set)))
              (else (cons (car set) (U (cdr set)))))))
         (member?
          (lambda (a lat)
            (cond
              ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else (member? a (cdr lat)))))))
      (U set1))))
; ------------------------------

; ------------------------------
(define union5
  (lambda (set1 set2)
    (letrec
        ((U
          (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2) (U (cdr set)))
              (else (cons (car set) (U (cdr set)))))))
         (M?
          (lambda (a lat)
            (cond
              ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else (M? a (cdr lat)))))))
      (U set1))))
; ------------------------------

; ------------------------------
(define union6
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) set2)
                ((M? (car set) set2) (U (cdr set)))
                (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            ((eq? a (car lat)) #t)
                            (else (N? (cdr lat)))))))
                 (N? lat)))))
             (U set1))))
; ------------------------------

(define list1
  (list 'apple 'custard 'pie 'linzer 'pie 'torte))

(define list2
  (list 'salad 'greens 'with 'pears 'brie 'cheese 'frozen 'yogurt))

(define set1
  (list 'tomatoes 'and 'macaroni 'casserole))

(define set2
  (list 'macaroni 'and 'cheese))