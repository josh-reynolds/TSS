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

; ------------------------------
(define intersectall5         ; text has letcc, but Racket/Scheme
  (lambda (lset)              ; appear to have let/cc instead
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (intersect (car lset) (A (cdr lset))))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))
; ------------------------------

; ------------------------------
(define intersectall6
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset)) (hop '()))
                   ((null? (cdr lset)) (car lset))
                   (else (intersect (car lset) (A (cdr lset))))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))))
; ------------------------------

; ------------------------------
(define intersect3
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set1)
              (cond
                ((null? set1) '())
                ((member? (car set1) set2) (cons (car set1) (I (cdr set1))))
                (else (I (cdr set1)))))))
      (cond
        ((null? set2) '())
        (else (I set1))))))
; ------------------------------

; ------------------------------
(define intersectall7         ; text reversed the member?/else clauses
  (lambda (lset)              ; in J which is different from intersect3 above
    (let/cc hop               ; and seems like a bug in the book...
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (I (car lset) (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s1)
                         (cond
                           ((null? s1) '())
                           ((member? (car s1) s2) (cons (car s1) (J (cdr s1))))
                           (else (J (cdr s1)))))))
                  (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))
; ------------------------------

; ------------------------------
(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((eq? a (car l)) (cdr l))
                (else (cons (car l) (R (cdr l))))))))
      (R lat))))
; ------------------------------

; ------------------------------
(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((eq? a (car l)) '())
                (else (cons (car l) (R (cdr l))))))))
      (R lat))))
; ------------------------------

; ------------------------------
(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (l)
                (cond
                  ((null? l) '())
                  ((eq? a (car l)) (skip (R (cdr l))))
                  (else (cons (car l) (R (cdr l))))))))
        (R lat)))))
; ------------------------------

(define set1
  (list 'tomatoes 'and 'macaroni))

(define set2
  (list 'macaroni 'and 'cheese))

(define set3
  (list 'and 'foo 'bar))

(define lset1
  (list (list 3 'mangos 'and)
        (list 3 'kiwis 'and)
        (list 3 'hamburgers)))

(define lset2
  (list (list 3 'steaks 'and)
        (list 'no 'food 'and)
        (list 'three 'baked 'potatoes)
        (list '3 'diet 'hamburgers)))

(define lset3
  (list (list 3 'mangos 'and)
        '()
        (list 3 'diet 'hamburgers)))

(define list1
  (list 'noodles 'spaghetti 'spätzle 'bean-thread 'roots 'potatoes 'yam 'others 'rice))

(define list2
  (list 'cookies 'chocolate 'mints 'caramel 'delight 'ginger 'snaps
        'desserts 'chocolate 'mousse 'vanilla 'ice 'cream 'German
        'chocolate 'cake 'more 'desserts 'gingerbreadman 'chocolate
        'chip 'brownies))

(define list3
  (list 'cookies 'chocolate 'mints 'caramel 'delight 'ginger 'snaps
        'desserts 'chocolate 'mousse 'vanilla 'ice 'cream 'German
        'chocolate 'cake 'more 'cookies 'gingerbreadman 'chocolate
        'chip 'brownies))