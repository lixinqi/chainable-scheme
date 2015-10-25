#!/usr/bin/env guile
!#
(define (compose f0 f1)
	(lambda (x)
		(f1 (f0 x))))

(define identity
	(lambda (x) x))

(define (do-nothing . x)
	'())

(define (println x)
	(display x)
	(newline)
	x)

(define (const x)
	(lambda (v) x))

(define-syntax pipe
	(syntax-rules ()
		((_) do-nothing)
		((_ x) x)
		((_ x y z ...) (compose x (pipe y z ...)))))

;; (define (pipe . x)
;; 	(if (null? x)
;; 		do-nothing
;; 		(if (null? (cdr x))
;; 			(car x)
;; 			(compose (car x) (apply pipe (cdr x))))))

(define-syntax hook
	(syntax-rules
		()
		((_ f) (lambda (x) (f x) x))
		((_ f0 f1 f2 ...) (lambda (x) ((pipe f0 f1 f2 ...) x) x))))

;; (define (hook f)
;; 	(lambda (x)
;; 		(f x)
;; 		x))

(define (hook0 f)
	(lambda (x)
		(f)
		x))

(define-syntax water
	(syntax-rules
		()
		((_ i) i)
		((_ i f) (f i))
		((_ i f0 f1 f2 ...) ((pipe f0 f1 f2 ...) i))))

;; (define (x-input x . y)
;; 	(let ([f (apply pipe y)])
;; 		(f x)))

(define xl-nothing
	(lambda (x)
		(lambda (xl-append) '())))

(define xl-identity
	(lambda (x)
		(lambda (xl-append) (xl-append x))))

(define (x-bind f)
	(lambda (xl)
			(lambda (xl-append)
				(xl
					(lambda (item)
						((f item) xl-append))))))
(define-syntax xl-bind
	(syntax-rules ()
		((_ e0) (x-bind e0))
		((_ e0 e1 e2 ...) (x-bind (pipe e0 e1 e2 ...)))))

;; (define (xl-bind . y)
;; 	(let ([get-sub-xl (apply pipe y)])
;; 		(lambda (xl)
;; 			(lambda (xl-append)
;; 				(xl
;; 					(lambda (item)
;; 						((get-sub-xl item) xl-append)))))))

(define (x-map f)
	(xl-bind (compose f xl-identity)))

(define-syntax xl-map
	(syntax-rules ()
		((_ e0) (x-map e0))
		((_ e0 e1 e2 ...) (x-map (pipe e0 e1 e2 ...)))))

;; (define (xl-map . y)
;; 	(xl-bind
;; 		(compose (apply pipe y) xl-identity)))

(define (xl-reduce acc f)
	(lambda (xl)
		(lambda (xl-append)
			(xl
				(lambda (x)
					(set! acc (f acc x))))
			(xl-append acc))))

(define (x-filter f)
	(xl-bind
		(lambda (x)
			(if (f x)
				(xl-identity x)
				(xl-nothing x)))))

(define-syntax xl-filter
	(syntax-rules ()
		((_ e0) (x-filter e0))
		((_ e0 e1 e2 ...) (x-filter (pipe e0 e1 e2 ...)))))

;; (define (xl-filter . y)
;; 	(let ([f (apply pipe y)])
;; 		(xl-bind
;; 		(lambda (x)
;; 			(if (f x)
;; 				(xl-identity x)
;; 				(xl-nothing x))))))

(define (x-until f)
	(lambda (xl)
			(lambda (xl-append)
				(call/cc
					(lambda (break)
						(xl
							(lambda (x)
								(if (f x)
									(break x)
									(xl-append x)))))))))

(define-syntax xl-until
	(syntax-rules ()
		((_ e0) (x-until e0))
		((_ e0 e1 e2 ...) (x-until (pipe e0 e1 e2 ...)))))

;; (define (xl-until . y)
;; 	(let ([f (apply pipe y)])
;; 		(lambda (xl)
;; 			(lambda (xl-append)
;; 				(call/cc
;; 					(lambda (break)
;; 						(xl
;; 							(lambda (x)
;; 								(if (f x)
;; 									(break x)
;; 									(xl-append x))))))))))

(define xl-N
	(lambda (xl-append)
		(define (loop acc)
			(xl-append acc)
			(loop (+ 1 acc)))
		(loop 0)))

(define (x-hook f)
	(xl-map (hook f)))

(define-syntax xl-hook
	(syntax-rules ()
		((_ e0) (x-hook e0))
		((_ e0 e1 e2 ...) (x-hook (pipe e0 e1 e2 ...)))))

; (define (xl-hook . y)
; 	(let ([f (apply pipe y)])
; 		(xl-map (hook f))))

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
	(lambda (x) (not (= x n))))

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

(define true?
	(lambda (x)
		(cond
			((equal? x 0) #f)
			((equal? x #f) #f)
			((equal? x "") #f)
			((equal? x '()) #f)
			((equal? x #()) #f)
			(else #t))))
(water
	(list 0 #f "" '() #())
	list->xl
	(xl-hook true? println)
	xl-exec)

(water
	"end"
	println)

(water
	0
	(pipe (add 4) (mul 3) (div 5))
	println)

(define n 0)

(define (square x) (* x x))

(water
	xl-N
	(xl-until (counter) (gte 20))
	(xl-map square)
	xl-force
	(xl-reduce 0 +)
	xl->list
	println)

(water
	0
	(add 1)
	(add 2)
	(mul 2)
	println)

(water
	xl-N
	(xl-until (counter) (gte 10))
	(xl-map (add 1) square)
	(xl-filter square (gte 8))
	xl->list
	(hook println)
	(hook reverse println)
	(hook car println)
	(hook cdr println))

