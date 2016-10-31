
;; 引用echo
(include "echo.scm")

;; 局部变量前缀，防止名字冲突
(define __tmp-var-name-prefix "__tmp-var-")

;; 局部变量编号，防止名字冲突
(define __tmp-var-name-counter 0)

;; 获取局部变量名
(define (__get-tmp-var-name)
	(set! __tmp-var-name-counter (+ 1 __tmp-var-name-counter))
	(string->symbol
		(string-append
		   __tmp-var-name-prefix
			(number->string __tmp-var-name-counter))))

;; 把表达式压平
(define (fn-expr-flatten expr)
	(define (__flatten-fn-expr expr)
		(if (null? expr)
			(list '() '())
			(let
				([ret (__flatten-fn-expr (cdr expr))])
				(if (list? (car expr))
					(let
						([varname (__get-tmp-var-name)])
						(list (cons (list varname (car expr)) (car ret)) (cons varname (cadr ret))))
					(list (car ret) (cons (car expr) (cadr ret)))))))
	(let ([ret (__flatten-fn-expr expr)]
				[var-name (__get-tmp-var-name)])
		`(let
			 ,(car ret)
			 (lambda (,var-name)
				 (echo ,var-name | ,@(cdadr ret))))))

;; 使用primitive-eval直接算出所需闭包
(define-syntax fn
	(syntax-rules ( | )
		[(_) (lambda (x) x)]
		[(_ x) x]
		[(_ e0 e ...)
			(primitive-eval (fn-expr-flatten (syntax->datum #`(fn e0 e ...))))]))
