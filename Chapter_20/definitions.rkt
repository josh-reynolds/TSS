#lang racket
; ------------------------------
; The Seasoned Schemer
; Chapter 20: What's in Store? 
; ------------------------------

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
; ------------------------------

; ------------------------------
(define the-empty-table       ; text uses ... which causes an error
  (lambda (name)              ; substituting 0 here
    0))
; ------------------------------

; ------------------------------
(define lookup
  (lambda (table name)
    (table name)))
; ------------------------------

; ------------------------------
(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        ((eq? name2 name1) value)
        (else (table name2))))))
; ------------------------------

; ------------------------------
(define value                 ; again substituting 0 for ...
  (lambda (e)
    0
    (cond
      ((define? e) (*define e))
      (else (the-meaning e))) 0 ))
; ------------------------------

; ------------------------------
(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e)) (eq? (car e) 'define))
      (else #f))))
; ------------------------------

; ------------------------------
(define the-meaning 0)            ; placeholders to allow evaluation
(define name-of 0)
(define right-side-of 0)
; ------------------------------

; ------------------------------
(define global-table
;  0 the-empty-table 0)
  0)
; ------------------------------

; ------------------------------
(define *define
  (lambda (e)
    (set! global-table
          (extend
           (name-of e)
           (box
            (the-meaning
             (right-side-of e)))
           global-table))))
; ------------------------------

; ------------------------------
(define boxx                  ; racket already supplies 'box'
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))
; ------------------------------