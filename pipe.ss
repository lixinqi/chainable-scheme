#!/usr/bin/env racket
#lang scheme

(define (identity x) x)

(define (compose f0 f1)
	(lambda (x)
		(f1 (f0 x))))

(define-syntax pipe
	(syntax-rules ()
		((_ e0) e0)
		((_ e0 e1 e2 ...) (compose e0 (pipe e1 e2 ...)))))

(define-syntax water
	(syntax-rules ()
		((_ e0) e0)
		((_ e0 e1 e2 ...) ((pipe e1 e2 ...) e0))))

(define-syntax hook
	(syntax-rules
		()
		((_ e0 e1 ...)
		 (let [(f (pipe e0 e1 ...))]
			 (lambda (x)
				 (f x)
				 x)))))

(define list->xl
	(lambda (ls)
		(lambda (cb)
			(for-each cb ls))))

(define proc->xl identity)

(define xl->list
	(lambda (xl)
		(let [(ls '())]
			(define (collect x)
				(set! ls (cons x ls)))
			(xl collect)
			(reverse ls))))

(define xl-identity
	(lambda (x)
		(lambda (cb)
			(cb x))))

(define xl-nothing
	(lambda (x)
		void))

(define (x-bind f)
	(lambda (xl)
		(lambda (cb)
			(xl
				(lambda (x)
					((f x) cb))))))

(define-syntax xl-bind
	(syntax-rules ()
		((_ e0 e1 ...) (x-bind (pipe e0 e1 ...)))))

(define (x-map f)
	(x-bind (compose f xl-identity)))

(define-syntax xl-map
	(syntax-rules ()
		((_ e0 e1 ...) (x-map (pipe e0 e1 ...)))))

(define (x-filter f)
	(x-bind
		(lambda (x)
			(if (f x)
				(xl-identity x)
				(xl-nothing x)))))

(define-syntax xl-filter
	(syntax-rules ()
		((_ e0 e1 ...) (x-filter (pipe e0 e1 ...)))))

(define (x-hook f)
	(x-map (hook f)))

(define-syntax xl-hook
	(syntax-rules ()
		((_ e0 e1 ...) (x-hook (pipe e0 e1 ...)))))

(define (x-until f)
	(lambda (xl)
		(lambda (cb)
			(call/cc
				(lambda (break)
					(xl
						(lambda (x)
							(if (f x)
								(break x)
								(cb x)))))))))

(define-syntax xl-until
	(syntax-rules ()
		((_ e0 e1 ...) (x-until (pipe e0 e1 ...)))))

(define xl->void
	(lambda (xl)
		(xl void)))

(define println
	(lambda (x)
		(display x)
		(newline)))

;; --------- test ---------

(define add1
	(lambda (x)
		(+ x 1)))

(define add2 (pipe add1 add1))

(define (square x) (* x x))

