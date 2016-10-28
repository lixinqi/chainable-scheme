
(include "echo.scm")
(include "define-method.scm")
(define-method (list-map f)
	(map f this))



(define-syntax fn
	(syntax-rules ()
		((_ x) x)
		((_ e ...) (lambda (x) (echo x | e ...)))))




(echo '(0 1 2 3 4)
			| list-map (fn + 5 | number->string | string-append "...")
      | display)
(newline)
;; => (5... 6... 7... 8... 9...)












