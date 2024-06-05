#lang racket

(define (atom e)
  (not (cons? e)))

(define (pairlis x y a)
  (cond [(null? x) a]
	[else (cons (cons (car x) (car y)) (pairlis (cdr x) (cdr y) a))]))

(define (evcon c a)
  (cond [(eval (caar c) a) (eval (cadar c) a)]
	[else (evcon (cdr c) a)]))

(define (evlis m a)
  (cond [(null? m) null]
	[else (cons (eval (car m) a) (evlis (cdr m) a))]))

(define (eval e a)
  (cond [(atom e) (cdr (assoc e a))]
	[(atom (car e)) (cond [(eq? (car e) 'QUOTE) (cadr e)]
			      [(eq? (car e) 'COND) (evcon (cdr e) a)]
			      [else (apply (car e) (evlis (cdr e) a) a)])]
	[else (apply (car e) (evlis (cdr e) a) a)]))

(define (apply fn x a)
  (cond [(atom fn) (cond [(eq? fn 'CAR) (caar x)]
			 [(eq? fn 'CDR) (cdar x)]
			 [(eq? fn 'CONS) (cons (car x) (cadr x))]
			 [(eq? fn 'ATOM) (atom (car x))]
			 [(eq? fn 'EQ) (eq? (car x) (cadr x))]
			 [else (apply (eval fn a) x a)])]
	[(eq? (car fn) 'LAMBDA) (eval (caddr fn) (pairlis (cadr fn) x a))]
	[(eq? (car fn) 'LABEL) (apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a))]))

(define (evalquote fn x) (apply fn x null))
