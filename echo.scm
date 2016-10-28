
;; echo 宏函数第二版
(define-syntax echo
	(syntax-rules (|)
		((_ x) x)
		((_ x | f | e ...)
             (echo (f x ) | e ...))
		((_ x | f y | e ...)
             (echo (f x y) | e ...))
		((_ x | f y y2 | e ...)
		         (echo (f x y y2) | e ...))
		((_ x | f y y2 y3 | e ...)
             (echo (f x y y2 y3) | e ...))
		((_ x | f y y2 y3 y4 | e ...)
		         (echo (f x y y2 y3 y4) | e ...))
		((_ x | f y y2 y3 y4 y5 | e ...)
             (echo (f x y y2 y3 y4 y5) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 | e ...)
             (echo (f x y y2 y3 y4 y5 y6 y7) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 y10  e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9 y10) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 | e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) | e ...))
		((_ x | f y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15| e ...)
		         (echo (f x y y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) | e ...))
		((_ x | f y ...) (echo (f x y ...)))))



;; 测试

;; (echo 0 | + 1 2 3 4 5 6 7 8 9  | - 0 | * 13 | / 14 | display)
;; (newline)
;; ;; => 585/14
;; 
;; (define prepend  (lambda (ls elem) (cons elem ls)))
;; (echo '()
;;       | prepend 0
;;       | prepend '(+ 52)
;;       | prepend '(- 0)
;;       | prepend '(* 13)
;;       | prepend '(/ 14)
;;       | reverse
;;       | display)
;; (newline)
;; ;; => (0 (+ 52) (- 0) (* 13) (/ 14))

