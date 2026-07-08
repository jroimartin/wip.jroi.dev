;; Zzz allows to write a delayed goal like
;;   (lambda (s/c)
;;     (lambda ()
;;       (g s/c)))
;; as
;;   (Zzz g)
(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

;; conj+ allows to write nested calls to conj like
;;   (conj g0
;;         (conj g1
;;               g2))
;; as
;;   (conj+ g0 g1 g2)
(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

;; disj+ allows to write nested calls to disj like
;;   (disj g0
;;         (disj g1
;;               g2))
;; as
;;   (disj+ g0 g1 g2)
(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

;; conde allows to write the common pattern
;;   (disj+ (conj+ g0 g1 g2)
;;          (conj+ g3 g4 g5)
;;          (conj+ g6 g7 g8))
;; as
;;   (conde
;;     (g0 g1 g2)
;;     (g3 g4 g5)
;;     (g6 g7 g8))
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

;; fresh allows to write nested calls to call/fresh like
;;   (call/fresh
;;     (lambda (x)
;;       (call/fresh
;;         (lambda (y)
;;            (g x y)))))
;; as
;;   (fresh (x y) (g x y))
(define-syntax fresh
  (syntax-rules ()
    ((_ () g ...) (conj+ g ...))
    ((_ (x0 x ...) g ...) (call/fresh (lambda (x0) (fresh (x ...) g ...))))))

;; pull automatically invokes an immature stream to return results.
(define (pull $)
  (if (procedure? $) (pull ($)) $))

;; take-all pulls all the results from the stream.
(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

;; take pulls n results from the stream.
(define (take n $)
  (if (zero? n)
      '()
      (let (($ (pull $)))
	(if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

;; empty-state represents the empty state.
(define empty-state '(() . 0))

;; call/empty-state attempts a goal in the empty state.
(define (call/empty-state g) (g empty-state))

;; run executes the provided goals and calls take, reifying the
;; results afterwards.
(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (multivar-reify (x ...) (take n (call/empty-state
				      (fresh (x ...) g0 g ...)))))))

;; run* executes the provided goals and calls take-all, reifying the
;; results afterwards.
(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (multivar-reify (x ...) (take-all (call/empty-state
					(fresh (x ...) g0 g ...)))))))

;; multivar-reify reifies a list of states s/c* by refiying each
;; state's substitution with respect to each provided variable.
(define-syntax multivar-reify
  (syntax-rules ()
    ((_ (x ...) s/c*)
     (map (lambda (s/c)
	    (reify
	     (let loop ((vs '(x ...)) (n 0))
	       (if (null? vs)
		   '()
		   (cons (cons (car vs) (var n))
			 (loop (cdr vs) (+ n  1)))))
	     s/c))
	  s/c*))))

;; The reify function takes a state s/c and an arbitrary value v,
;; perhaps containing variables, and returns the reified value of v.
(define (reify v s/c)
  (let ((v (walk* v (car s/c))))
    (walk* v (reify-s v '()))))

;; reify-s takes a walk*ed term as its first argument; its second
;; argument starts out as an empty substitution.  The result of
;; invoking reify-s is a reified name substitution, associating logic
;; variables to distinct symbols of the form _.n.
(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (ext-s v (reify-name (length s)) s))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

;; reify-name generates a symbol of the form _.n.
(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

;; walk* deeply walks a term with respect to a substitution.
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
		      (walk* (cdr v) s)))
     (else v))))
