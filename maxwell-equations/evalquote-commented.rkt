#lang racket

;; Distinction between forms and functions.
;;
;; It is usual to use the word "function" imprecisely, and to apply it
;; to forms such as y*2+x.  Because we shall compute with expressions
;; that stand for functions, we need a notation that expresses the
;; distinction between functions and forms.  The notation that we
;; shall use is the lambda notation of Alonzo Church.
;;
;; Let f be an expression that stands for a function of two integer
;; variables.  It should make sense to write f[3;4] and to be able to
;; determine the value of this expression.  For example, sum[3;4]=7.
;; The expression y*2+x does not meet this requirement.  It is not at
;; all clear whether the value of y*2+x[3;4] is 10 or 11.  An
;; expression such as y*2+x will be called a form rather than a
;; function.  A form can be converted to a function by specifying the
;; correspondence between the variables in the form and the arguments
;; of the desired function.

;; cons returns #t if e is an atomic symbol, #f otherwise.
(define (atom e)
  (not (cons? e)))

;; pairlis gives the list of pairs of corresponding elements of the
;; lists x and y, and appends this to the list a.  The resultant list
;; of pairs, which is like a table with two columns, is called an
;; association list.
(define (pairlis x y a)
  (cond [(null? x) a]
	[else (cons (cons (car x) (car y)) (pairlis (cdr x) (cdr y) a))]))

;; evcon handles the COND form.  It evaluates the list of
;; propositional terms c in order, and chooses the form following the
;; first true predicate.  a is an association list of names and
;; definitions.
(define (evcon c a)
  ;; Given the conditional expression
  ;;   (COND ((ATOM (QUOTE A)) (QUOTE B)) ((QUOTE T) (QUOTE C)))
  ;; c is
  ;;   '(((ATOM (QUOTE A)) (QUOTE B)) ((QUOTE T) (QUOTE C)))
  ;; Thus,
  ;;   (caar c) returns '(ATOM (QUOTE A))
  ;;   (cadar c) returns '(QUOTE B)
  ;;   (cdr c) returns '(((QUOTE T) (QUOTE C)))
  (cond [(eval (caar c) a) (eval (cadar c) a)]
	[else (evcon (cdr c) a)]))

;; evlis evaluates the list of arguments m of a function.  a is an
;; association list of names and definitions.
(define (evlis m a)
  ;; Given the function call
  ;;   (CONS (QUOTE A) (QUOTE B))
  ;; m is
  ;;   '((QUOTE B) (QUOTE C))
  ;; Thus,
  ;;   (car m) returns '(QUOTE B)
  ;;   (cdr m) returns '((QUOTE C))
  (cond [(null? m) null]
	[else (cons (eval (car m) a) (evlis (cdr m) a))]))

;; eval handles a form.  a is an association list of names and
;; definitions.
(define (eval e a)
  (cond
   ;; If e is an atomic, then it must be a variable, and its value is
   ;; looked up on the association list.
   [(atom e) (cdr (assoc e a))]
   ;; If e is a list that begins with an atomic symbol, then it can be
   ;; a quote expressions, a conditional expression or a function
   ;; call.
   [(atom (car e))
    (cond
     ;; If car of the form is QUOTE, then it is a constant.
     ;; Given the expression
     ;;   (QUOTE A)
     ;; Then,
     ;;   (cadr e) returns 'A
     [(eq? (car e) 'QUOTE) (cadr e)]
     ;; If car of the form is COND, then it is a conditional
     ;; expression, and evcon handles it.
     ;; Given the expression
     ;;   (COND ((ATOM (QUOTE A)) (QUOTE B)) ((QUOTE T) (QUOTE C)))
     ;; Then,
     ;;   (cdr e) returns '(((ATOM (QUOTE A)) (QUOTE B)) ((QUOTE T) (QUOTE C)))
     [(eq? (car e) 'COND) (evcon (cdr e) a)]
     ;; Otherwise, the form must be a function followed by its
     ;; arguments.  The arguments are then evaluated, and the function
     ;; is given to apply.
     [else (apply (car e) (evlis (cdr e) a) a)])]
   [else (apply (car e) (evlis (cdr e) a) a)]))

;; apply handles a function and its arguments.
;;
;; fn is a function.  If it is an atomic symbol, there are two
;; possibilities:
;;
;; - fn is an elementary function: CAR, CDR, CONS, ATOM or EQ.
;; - fn is an atom.
;;
;; In each case, the appropriate function is applied to the arguments.
;;
;; x is a list of S-expressions used as arguments.  a is an
;; association list of names and definitions.
(define (apply fn x a)
  (cond
   ;; fn is an atomic symbol.
   [(atom fn)
    (cond
     ;; fn is the elementary function CAR.
     ;; Given the arguments
     ;;   '((A B C))
     ;; Then,
     ;;   (caar x) returns 'A
     [(eq? fn 'CAR) (caar x)]
     ;; fn is the elementary function CDR.
     ;; Given the arguments
     ;;   '((A B C))
     ;; Then,
     ;;   (cdar x) returns '(B C)
     [(eq? fn 'CDR) (cdar x)]
     ;; fn is the elementary function CONS.
     ;; Given the arguments
     ;;   '(A B)
     ;; Then,
     ;;   (car x) returns 'A
     ;;   (cadr x) returns 'B
     [(eq? fn 'CONS) (cons (car x) (cadr x))]
     ;; fn is the elementary function ATOM.
     ;; Given the arguments
     ;;   '((A . B))
     ;; Then,
     ;;   (car x) returns '(A . B)
     [(eq? fn 'ATOM) (atom (car x))]
     ;; fn is the elementary function EQ.
     ;; Given the arguments
     ;;   '(A B)
     ;; Then,
     ;;   (car x) returns 'A
     ;;   (cadr x) returns 'B
     [(eq? fn 'EQ) (eq? (car x) (cadr x))]
     ;; Otherwise, fn must be looked up in the association list.
     [else (apply (eval fn a) x a)])]

   ;; If fn begins with LAMBDA, then the arguments are paired with the
   ;; bound variables and the form is given to eval to evaluate.
   ;; Given the expression
   ;;   (LAMBDA (X) (CDR X))
   ;; And the arguments
   ;;   '((A B C))
   ;; Then,
   ;;   (caddr fn) returns '(CDR X)
   ;;   (cadr fn) returns '(X)
   [(eq? (car fn) 'LAMBDA) (eval (caddr fn) (pairlis (cadr fn) x a))]

   ;; If fn begins with LABEL, then the function name and definition are
   ;; added to the association list, and the inside function is evaluated
   ;; by apply.
   ;; Given the expresion
   ;;   (LABEL FF (LAMBDA (X) (FF (CONS (QUOTE A) X))))
   ;; And the arguments,
   ;;   '((B))
   ;; Then,
   ;;   (caddr fn) returns '(LAMBDA (X) (FF (CONS (QUOTE A) X)))
   ;;   (cadr fn) returns 'FF
   [(eq? (car fn) 'LABEL) (apply (caddr fn) x (cons (cons (cadr fn) (caddr fn)) a))]))

;; evalquote is a universal LISP function.  In other words, it can
;; compute the value of any given function applied to its arguments
;; when given a description of that function.
;;
;; fn is the function to be applied represented as a S-expression.  x
;; is a list of S-expressions used as arguments.
(define (evalquote fn x) (apply fn x null))
