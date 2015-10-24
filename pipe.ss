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

(define (with-0 . y)
	(let ([f (apply pipe y)])
		(f 0)))

(define (x-input x . y)
	(let ([f (apply pipe y)])
		(f x)))

(define x-nothing
	(lambda (x)
		(lambda (x-append) '())))

(define x-identity
	(lambda (x)
		(lambda (x-append) (x-append x))))

(define (x-bind . y)
	(let ([get-sub-xl (apply pipe y)])
		(lambda (xl)
			(lambda (x-append)
				(xl
					(lambda (item)
						((get-sub-xl item) x-append)))))))

(define (x-map . y)
	(x-bind
		(compose (apply pipe y) x-identity)))

(define (x-reduce acc f)
	(lambda (xl)
		(lambda (x-append)
			(xl
				(lambda (x)
					(set! acc (f acc x))))
			(x-append acc))))

(define (x-filter . y)
	(let ([f (apply pipe y)])
		(x-bind
		(lambda (x)
			(if (f x)
				(x-identity x)
				(x-nothing x))))))

(define (x-until . y)
	(let ([f (apply pipe y)])
		(lambda (xl)
			(lambda (x-append)
				(call/cc
					(lambda (break)
						(xl
							(lambda (x)
								(if (f x)
									(break x)
									(x-append x))))))))))

(define x-N
	(lambda (x-append)
		(define (loop acc)
			(x-append acc)
			(loop (+ 1 acc)))
		(loop 0)))

(define (x-hook . y)
	(let ([f (apply pipe y)])
		(x-map (hook f))))

(define x-exec
	(lambda (xl)
		(xl (lambda (x) '()))))

(define list->xl
	(lambda (ls)
		(lambda (x-append)
		(map x-append ls))))

(define xl->list
	(lambda (xl)
		(let ([acc '()])
			(xl
				(lambda (x)
					(set! acc (cons x acc))))
			(reverse acc))))

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

(define-macro
	(tee x)
	`(lambda (v)
		 (set! ,x v)
		 v))

;;------------test---------------
(define n 0)

(define (square x) (* x x))

(x-input
	x-N
	(x-until
		(counter)
		(gte 3))
	(x-bind
		(const x-N)
		(x-until (gte 3)))
	(x-filter
		(counter)
		(gte 4))
	(x-hook (tee n))
	(x-hook println)
	x-exec)

(x-input
	x-N
	(x-until
		(counter)
		(gte 10))
	(x-map square)
	(x-reduce 0 +)
	(x-hook println)
	x-exec)

(x-input
	0
	(add 1)
	(add 2)
	(mul 2)
	println)

(x-input
	'(0 1 2 3 4 5 6)
	list->xl
	(x-map (add 1))
	(x-map square)
	(x-filter square (gte 8))
	xl->list
	println)

