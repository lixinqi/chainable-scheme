#!/usr/bin/env guile
!#
(define (compose f0 f1)
	(lambda (x)
		(f1 (f0 x))))

(define (do-nothing . x)
	'())

(define (println x)
	(display x)
	(newline)
	x)

(define (const x)
	(lambda (v) x))

(define (pipe . x)
	(if (null? x)
		do-nothing
		(if (null? (cdr x))
			(car x)
			(compose (car x) (apply pipe (cdr x))))))

(define (hook f)
	(lambda (x)
		(f x)
		x))

(define (hook0 f)
	(lambda (x)
		(f)
		x))

(define (x-input x . y)
	(let ([f (apply pipe y)])
		(f x)))

(define xl-nothing
	(lambda (x)
		(lambda (xl-append) '())))

(define xl-identity
	(lambda (x)
		(lambda (xl-append) (xl-append x))))

(define (xl-bind . y)
	(let ([get-sub-xl (apply pipe y)])
		(lambda (xl)
			(lambda (xl-append)
				(xl
					(lambda (item)
						((get-sub-xl item) xl-append)))))))

(define (xl-map . y)
	(xl-bind
		(compose (apply pipe y) xl-identity)))

(define (xl-reduce acc f)
	(lambda (xl)
		(lambda (xl-append)
			(xl
				(lambda (x)
					(set! acc (f acc x))))
			(xl-append acc))))

(define (xl-filter . y)
	(let ([f (apply pipe y)])
		(xl-bind
		(lambda (x)
			(if (f x)
				(xl-identity x)
				(xl-nothing x))))))

(define (xl-until . y)
	(let ([f (apply pipe y)])
		(lambda (xl)
			(lambda (xl-append)
				(call/cc
					(lambda (break)
						(xl
							(lambda (x)
								(if (f x)
									(break x)
									(xl-append x))))))))))

(define xl-N
	(lambda (xl-append)
		(define (loop acc)
			(xl-append acc)
			(loop (+ 1 acc)))
		(loop 0)))

(define (xl-hook . y)
	(let ([f (apply pipe y)])
		(xl-map (hook f))))

(define xl-exec
	(lambda (xl)
		(xl (lambda (x) '()))))

(define list->xl
	(lambda (ls)
		(lambda (xl-append)
		(map xl-append ls))))

(define xl->list
	(lambda (xl)
		(let ([acc '()])
			(xl
				(lambda (x)
					(set! acc (cons x acc))))
			(reverse acc))))

(define xl-force (pipe xl->list list->xl))

(define (square x)
	(* x x))

(define (lt n)
	(lambda (x) (< x n)))

(define (lte n)
	(lambda (x) (<= x n)))

(define (eq n)
	(lambda (x) (= x n)))

(define (ne n)
	(lambda (x) (!= x n)))

(define (gt n)
	(lambda (x) (> x n)))

(define (gte n)
	(lambda (x) (>= x n)))

(define (add n)
	(lambda (x) (+ x n)))

(define (sub n)
	(lambda (x) (- x n)))

(define (mul n)
	(lambda (x) (* x n)))

(define (div n)
	(lambda (x) (/ x n)))

(define (counter)
	(let ([c 0])
		(define (get-counter . step)
			(let ([ret c])
			(set! c (+ 1 c))
			ret))
		get-counter))

(define-syntax tee
	(syntax-rules ()
		((_ x)
		 (lambda (v) (set! x v) v))))

;;------------test---------------
(define n 0)

(define (square x) (* x x))

(x-input
	xl-N
	(xl-until (counter) (gte 20))
	(xl-map square)
	xl-force
	(xl-reduce 0 +)
	xl->list
	println)

(x-input
	0
	(add 1)
	(add 2)
	(mul 2)
	println)

(x-input
	xl-N
	(xl-until (counter) (gte 10))
	(xl-map (add 1) square)
	(xl-filter square (gte 8))
	xl->list
	(hook println)
	(pipe
		reverse
		(hook (pipe car println))
		println))

