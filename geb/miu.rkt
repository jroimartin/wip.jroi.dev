#lang racket/base

(provide xi->xiu
	 mx->mxx
	 iii->u
	 uu->null)

(define (last lst)
  (unless (list? lst)
    (raise-argument-error 'last "list?" lst))
  (let loop ([l lst])
    (cond [(null? l) #f]
	  [(pair? (cdr l)) (loop (cdr l))]
	  [else (car l)])))

(define (xi->xiu lst)
  (unless (list? lst)
    (raise-argument-error 'xi->xiu "list?" lst))
  (cond [(null? lst) #f]
	[(equal? (last lst) 'I) (append lst '(U))]
	[else #f]))

(define (mx->mxx lst [which 0])
  (unless (list? lst)
    (raise-argument-error 'mx->mxx "list?" lst))
  (unless (exact-nonnegative-integer? which)
    (raise-argument-error 'mx->mxx "exact-nonnegative-integer?" which))
  (define (m? l) (equal? (car l) 'M))
  (let loop ([l lst]
	     [w which]
	     [acc '()])
    (cond [(null? l) #f]
	  [(and (zero? w) (m? l)) (append acc l (cdr l))]
	  [else (loop (cdr l) (if (m? l) (sub1 w) w) (append acc (list (car l))))])))

(define (cut-prefix lst pref)
  (unless (list? lst)
    (raise-argument-error 'cut-prefix "list?" lst))
  (unless (list? pref)
    (raise-argument-error 'cut-prefix "list?" pref))
  (let loop ([l lst]
	     [p pref])
    (cond [(null? p) l]
	  [(null? l) #f]
	  [(equal? (car l) (car p)) (loop (cdr l) (cdr p))]
	  [else #f])))

(define (replace lst from to which)
  (unless (list? lst)
    (raise-argument-error 'replace "list?" lst))
  (unless (list? from)
    (raise-argument-error 'replace "list?" from))
  (unless (list? to)
    (raise-argument-error 'replace "list?" to))
  (unless (exact-nonnegative-integer? which)
    (raise-argument-error 'replace "exact-nonnegative-integer?" which))
  (let loop ([l lst]
	     [w which]
	     [acc '()])
    (cond [(null? l) #f]
	  [(and (zero? w) (cut-prefix l from)) => (lambda (after) (append acc to after))]
	  [else (loop (cdr l) (if (cut-prefix l from) (sub1 w) w) (append acc (list (car l))))])))

(define (iii->u lst [which 0])
  (unless (list? lst)
    (raise-argument-error 'iii->u "list?" lst))
  (unless (exact-nonnegative-integer? which)
    (raise-argument-error 'iii->u "exact-nonnegative-integer?" which))
  (replace lst '(I I I) '(U) which))

(define (uu->null lst [which 0])
  (unless (list? lst)
    (raise-argument-error 'uu->null "list?" lst))
  (unless (exact-nonnegative-integer? which)
    (raise-argument-error 'uu->null "exact-nonnegative-integer?" which))
  (replace lst '(U U) '() which))

(define (derive lst ops)
  (unless (list? lst)
    (raise-argument-error 'derive "list?" lst))
  (unless (list? ops)
    (raise-argument-error 'derive "list?" ops))
  (cond [(null? ops) lst]
	[(pair? (car ops)) (let ([l (apply (caar ops) (append (list lst) (cdar ops)))])
			     (and l (derive l (cdr ops))))]
	[else (let ([l (apply (car ops) (list lst))])
		(and l (derive l (cdr ops))))]))

(module+ test
  (require rackunit
	   rackunit/text-ui)

  (define-test-suite tests
    (test-suite "last"
		(test-case
		 "multiple elements"
		 (check-equal? (last '(a b c d)) 'd))
		(test-case
		 "one element"
		 (check-equal? (last '(a)) 'a))
		(test-case
		 "empty list"
		 (check-false (last '())))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (last '(a . b))))))

    (test-suite "xi->xiu"
		(test-case
		 "ends with I"
		 (check-equal? (xi->xiu '(M U I)) '(M U I U)))
		(test-case
		 "empty list"
		 (check-false (xi->xiu '())))
		(test-case
		 "does not end with I"
		 (check-false (xi->xiu '(M U))))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (xi->xiu '(a . b))))))

    (test-suite "mx->mxx"
		(test-case
		 "0th"
		 (check-equal? (mx->mxx '(I M U M U I) 0) '(I M U M U I U M U I)))
		(test-case
		 "n-th"
		 (check-equal? (mx->mxx '(M U M U M U I) 2) '(M U M U M U I U I)))
		(test-case
		 "default which"
		 (check-equal? (mx->mxx '(I M U M U I)) (mx->mxx '(I M U M U I) 0)))
		(test-case
		 "which is too big"
		 (check-false (mx->mxx '(M U M U M U I) 3)))
		(test-case
		 "empty list"
		 (check-false (mx->mxx '() 0)))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (mx->mxx '(a . b) 0))))
		(test-case
		 "negative which"
		 (check-exn exn:fail:contract? (lambda () (mx->mxx '(M U M U M U I) -1)))))

    (test-suite "cut-prefix"
		(test-case
		 "has prefix"
		 (check-equal? (cut-prefix '(a b c d) '(a b)) '(c d)))
		(test-case
		 "no prefix"
		 (check-false (cut-prefix '(a b c d) '(b c))))
		(test-case
		 "partial prefix"
		 (check-false (cut-prefix '(a) '(a b))))
		(test-case
		 "empty list"
		 (check-false (cut-prefix '() '(b c))))
		(test-case
		 "empty prefix"
		 (check-equal? (cut-prefix '(a b c d) '()) '(a b c d)))
		(test-case
		 "empty lists"
		 (check-equal? (cut-prefix '() '()) '()))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (cut-prefix '(a . b) '(a)))))
		(test-case
		 "invalid pref type"
		 (check-exn exn:fail:contract? (lambda () (cut-prefix '(a b c d) '(a . b))))))

    (test-suite "replace"
		(test-case
		 "0th"
		 (check-equal? (replace '(a b c d) '(b c) '(x y z) 0) '(a x y z d)))
		(test-case
		 "nth"
		 (check-equal? (replace '(a b c d b c e) '(b c) '(x y z) 1) '(a b c d x y z e)))
		(test-case
		 "overlap"
		 (check-equal? (replace '(a b b b c d) '(b b) '(x y z) 1) '(a b x y z c d)))
		(test-case
		 "empty list"
		 (check-false (replace '() '(b c) '(x y z) 0)))
		(test-case
		 "empty from"
		 (check-equal? (replace '(a b c d) '() '(x y z) 2) '(a b x y z c d)))
		(test-case
		 "empty to"
		 (check-equal? (replace '(a b c d) '(b c) '() 0) '(a d)))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda ()(replace '(a . b) '(b) '(x y z) 0))))
		(test-case
		 "invalid from type"
		 (check-exn exn:fail:contract? (lambda ()(replace '(a b c d) '(b . c) '(x y z) 0))))
		(test-case
		 "invalid to type"
		 (check-exn exn:fail:contract? (lambda ()(replace '(a b c d) '(b c) '(x . y) 0))))
		(test-case
		 "negative which"
		 (check-exn exn:fail:contract? (lambda ()(replace '(a b c d) '(b c) '(x y z) -1)))))

    (test-suite "iii->u"
		(test-case
		 "0th"
		 (check-equal? (iii->u '(M I I I U M I I I) 0) '(M U U M I I I)))
		(test-case
		 "n-th"
		 (check-equal? (iii->u '(M I I I U M I I I) 1) '(M I I I U M U)))
		(test-case
		 "default which"
		 (check-equal? (iii->u '(M I I I U M I I I)) (iii->u '(M I I I U M I I I) 0)))
		(test-case
		 "overlap"
		 (check-equal? (iii->u '(M I I I I M U) 1) '(M I U M U)))
		(test-case
		 "reset counter"
		 (check-equal? (iii->u '(M I I M I I I U) 0) '(M I I M U U)))
		(test-case
		 "empty list"
		 (check-false (iii->u '() 0)))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (iii->u '(a . b) 0))))
		(test-case
		 "negative which"
		 (check-exn exn:fail:contract? (lambda () (iii->u '(M I I I) -1)))))

    (test-suite "uu->null"
		(test-case
		 "0th"
		 (check-equal? (uu->null '(M U U M U U I) 0) '(M M U U I)))
		(test-case
		 "n-th"
		 (check-equal? (uu->null '(M U U M U U I) 1) '(M U U M I)))
		(test-case
		 "default which"
		 (check-equal? (uu->null '(M U U M U U I)) (uu->null '(M U U M U U I) 0)))
		(test-case
		 "overlap"
		 (check-equal? (uu->null '(M U U U U M I) 1) '(M U U M I)))
		(test-case
		 "reset counter"
		 (check-equal? (uu->null '(M U M U U U I) 1) '(M U M U I)))
		(test-case
		 "empty list"
		 (check-false (uu->null '() 0)))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (uu->null '(a . b) 0))))
		(test-case
		 "negative which"
		 (check-exn exn:fail:contract? (lambda () (uu->null '(M I I I) -1)))))

    (test-suite "derive"
		(test-case
		 "geb example"
		 (check-equal? (derive '(M I) `(,mx->mxx
						,mx->mxx
						,xi->xiu
						,iii->u
						,mx->mxx
						,uu->null))
			       '(M U I I U)))
		(test-case
		 "op arguments"
		 (check-equal? (derive '(M U U I M U U) `((,uu->null . (1)))) '(M U U I M)))
		(test-case
		 "empty list"
		 (check-false (derive '() `(,xi->xiu ,mx->mxx))))
		(test-case
		 "empty list and op arguments"
		 (check-false (derive '() `((,mx->mxx . (0))
					    ,xi->xiu))))
		(test-case
		 "empty ops"
		 (check-equal? (derive '(M U I) '()) '(M U I)))
		(test-case
		 "empty lists"
		 (check-equal? (derive '() '()) '()))
		(test-case
		 "invalid lst type"
		 (check-exn exn:fail:contract? (lambda () (derive '(a . b) `(,mx->mxx)))))
		(test-case
		 "invalid ops type"
		 (check-exn exn:fail:contract? (lambda () (derive '(M I) `(,mx->mxx . ,mx->mxx)))))))

  (void (run-tests tests 'verbose)))
