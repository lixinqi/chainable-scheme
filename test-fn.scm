

(include "fn.scm")
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
;; => 1
(display (get-counter 1))
(newline)
;; => 2
