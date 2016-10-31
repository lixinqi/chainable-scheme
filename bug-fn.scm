
(include "echo.scm")
(include "define-method.scm")

(define-syntax fn
	(syntax-rules ()
		((_ e ...) (lambda (x) (echo x | e ...)))))

(define generate-counter
  (lambda ()
		(let
				([count 0])
			(lambda (step )
				(set! count (+ step count))
				count))))

(define get-counter (fn (generate-counter)))

(display (get-counter 1))
(newline)
;; => ?
(display (get-counter 1))
(newline)
;; => ?










