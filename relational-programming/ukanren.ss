;; Variables are represented as vectors that hold their variable
;; index.  Variable equality is determined by coincidence of indices
;; in vectors.
(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

;; The walk operator searches for a variable's value in the
;; substitution.  When a non-variable term is walked, the term itself
;; is returned.
(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

;; The ext-s operator extends the substitution with a new binding.
;; When extending the substitution, the first argument is always a
;; variable and the second is an arbitrary term.
(define (ext-s x v s) `((,x . ,v) . ,s))

;; The ≡ goal constructor takes two terms as arguments and returns a
;; goal that succeeds if those two terms unify in the received state.
;; If those two terms fail to unify in that state, the empty stream is
;; instead returned.
(define (≡ u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

;; unit lifts the state into a stream whose only element is that
;; state.
(define (unit s/c) (cons s/c mzero))

;; mzero is the empty stream.
(define mzero '())

;; Terms of the language are defined by the unify operator.  Here,
;; terms of the language consist of variables, objects deemed
;; identical under eqv?, and pairs of the foregoing.  To unify two
;; terms in a substitution, both are walked in that substitution.  If
;; the two terms walk to the same variable, the original substitution
;; is returned unchanged.  When one of the two terms walks to a
;; variable, the substitution is extended, bindings the variable to
;; which that term walks with the value of which the other term walks.
;; If both terms walk to pairs, the cars and then cdrs are unifed
;; recursively, succeeding if unification succeeds in the one and then
;; the other.  Finally, non-variable, non-pair terms unify if they are
;; identical under eqv?, and unification fails otherwise.
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
	(and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))

;; The call/fresh goal constructor takes a unary function f whose body
;; is a goal, and itself returns a goal.  This returned goal, when
;; provided a state s/c, binds the formal parameter of f to a new
;; logic variable, and passes a state, with the substitution it
;; originally received and a newly incremented fresh-variable counter,
;; c, to the goal that is the body of f.
(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

;; The disj goal constructor takes two goals as arguments and returns
;; a goal that succeeds if either of the two subgoals succeed.
(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))

;; The conj goal constructor similarly takes two goals as arguments
;; and returns a goal that succeeds if both goals succeed for that
;; state.
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

;; The search strategy of μKanren is encoded through the mplus and
;; bind operators.

;; The mplus operator is responsible for merging streams.
(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

;; bind invokes the goal g on each element of the stream $.  If the
;; stream is empty or becomes exhausted mzero, the empty stream, is
;; returned.
(define (bind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (bind ($) g)))
   (else (mplus (g (car $)) (bind (cdr $) g)))))
