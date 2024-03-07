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
(define the-empty-table
  (lambda (name)
    (abort
     (cons 'no-answer
           (cons name '())))))
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
(define value
  (lambda (e)
    (let/cc the-end
      (set! abort the-end)
      (cond
        ((define? e) (*define e))
        (else (the-meaning e))) 0 )))
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
           (boxx
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

; ------------------------------
(define setboxx
  (lambda (boxx new)
    (boxx (lambda (it set) (set new)))))
; ------------------------------

; ------------------------------
(define unboxx
  (lambda (boxx)
    (boxx (lambda (it set) it))))
; ------------------------------

; ------------------------------
(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))
; ------------------------------

; ------------------------------
(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))
; ------------------------------

; ------------------------------
(define meaning
  (lambda (e table)
    ((expression-to-action e)
     e table)))
; ------------------------------

; ------------------------------
(define *quote
  (lambda (e table)
    (text-of e)))
; ------------------------------

; ------------------------------
(define *identifier
  (lambda (e table)
    (unboxx (lookup table e))))
; ------------------------------

; ------------------------------
(define *set
  (lambda (e table)
    (setbox
     (lookup table (name-of e))
     (meaning (right-side-of e) table))))
; ------------------------------

; ------------------------------
(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend
               (formals-of e)
               (box-all args)
               table)))))
; ------------------------------

; ------------------------------
(define beglis
  (lambda (es table)
    (cond
      ((null? (cdr es)) (meaning (car es) table))
      (else ((lambda (val)
               (beglis (cdr es) table))
             (meaning (car es) table))))))
; ------------------------------

; ------------------------------
(define box-all
  (lambda (vals)
    (cond
      ((null? vals) '())
      (else (cons (box (car vals))
                  (box-all (cdr vals)))))))
; ------------------------------

; ------------------------------
(define multi-extend
  (lambda (names values table)
    (cond
      ((null? names) table)
      (else
       (extend (car names) (car values)
               (multi-extend
                (cdr names)
                (cdr values)
                table))))))
; ------------------------------

; ------------------------------
(define odd?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (even? (sub1 n))))))
; ------------------------------

; ------------------------------
(define even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (odd? (sub1 n))))))
; ------------------------------

; ------------------------------
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))
; ------------------------------

; ------------------------------
(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       ((lambda (val)
          (cons val (evlis (cdr args) table)))
          (meaning (car args) table))))))
; ------------------------------

; ------------------------------
(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))
; ------------------------------

; ------------------------------
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))
; ------------------------------

; ------------------------------
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))
; ------------------------------

; ------------------------------
(define *const_old            ; first version from p. 192
  (lambda (e table)
    (cond
      ((number? e) 2)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      ((eq? e 'cons) (b-prim cons))
      ((eq? e 'car) (a-prim car))
      ((eq? e 'cdr) (a-prim cdr))
      ((eq? e 'eq?) (b-prim eq?))
      ((eq? e 'atom?) (a-prim atom?))
      ((eq? e 'null?) (a-prim null?))
      ((eq? e 'zero?) (a-prim zero?))
      ((eq? e 'add1) (a-prim add1))
      ((eq? e 'sub1) (a-prim sub1))
      ((eq? e 'number?) (a-prim number?)))))
; ------------------------------

; ------------------------------
(define *const                ; text has number? listed twice in let...
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:null? (a-prim null?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))        
        (:number? (a-prim number?)))
    (lambda (e table)
      (cond
        ((number? e) e)
        ((eq? e #t) #t)
        ((eq? e #f) #f)
        ((eq? e 'cons) :cons)
        ((eq? e 'car) :car)
        ((eq? e 'cdr) :cdr)
        ((eq? e 'eq?) :eq?)
        ((eq? e 'atom?) :atom?)
        ((eq? e 'null?) :null?)
        ((eq? e 'zero?) :zero?)
        ((eq? e 'add1) :add1)
        ((eq? e 'sub1) :sub1)
        ((eq? e 'number?) :number?)))))
; ------------------------------

; ------------------------------
(define *const2               ; second version, without let
  ((lambda (:cons :car   :cdr   :null?
            :eq?  :atom? :zero? :add1
            :sub1 :number?)
     (lambda (e table)
       (cond
         ((number? e) e)
         ((eq? e #t) #t)
         ((eq? e #f) #f)
         ((eq? e 'cons) :cons)
         ((eq? e 'car) :car)
         ((eq? e 'cdr) :cdr)
         ((eq? e 'eq?) :eq?)
         ((eq? e 'atom?) :atom?)
         ((eq? e 'null?) :null?)
         ((eq? e 'zero?) :zero?)
         ((eq? e 'add1) :add1)
         ((eq? e 'sub1) :sub1)
         ((eq? e 'number?) :number?))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (b-prim eq?)
   (a-prim atom?)
   (a-prim null?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))
; ------------------------------

; ------------------------------
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
; ------------------------------

; ------------------------------
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
; ------------------------------

; ------------------------------
(define *letcc
  (lambda (e table)
    (let/cc skip
      (beglis (ccbody-of e)
              (extend
               (name-of e)
               (box (a-prim skip))
               table)))))
; ------------------------------

; ------------------------------
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))
; ------------------------------

; ------------------------------
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))
; ------------------------------

; ------------------------------
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'letcc) *letcc)
         ((eq? (car e) 'set!) *set)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))
; ------------------------------

; ------------------------------
; placeholders to allow evaluation
(define setbox 0)
(define abort 0)
; ------------------------------

; ------------------------------
(define text-of
  (lambda (x)
    (car (cdr x))))

(define formals-of
  (lambda (x)
    (car (cdr x))))

(define body-of
  (lambda (x)
    (cdr (cdr x))))

(define ccbody-of
  (lambda (x)
    (cdr (cdr x))))

(define name-of
  (lambda (x)
    (car (cdr x))))

(define right-side-of
  (lambda (x)
    (cond
      ((null? (cdr (cdr x))) 0)
      (else (car (cdr (cdr x)))))))

(define cond-lines-of
  (lambda (x)
    (cdr x)))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (car (cdr x))))

(define function-of
  (lambda (x)
    (car x)))

(define arguments-of
  (lambda (x)
    (cdr x)))
; ------------------------------