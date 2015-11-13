#!/usr/bin/env racket
#lang scheme

;; 自函数
(define (identity x) x)

;; 复合函数
(define (compose f0 f1)
	(lambda (x)
		(f1 (f0 x))))

;; 管道算子
(define-syntax pipe
	(syntax-rules ()
		((_ e0) e0)
		((_ e0 e1 e2 ...) (compose e0 (pipe e1 e2 ...)))))

;; 第一个参数作初始值，后续参数作一系列处理流程
(define-syntax water
	(syntax-rules ()
		((_ e0) e0)
		((_ e0 e1 e2 ...) ((pipe e1 e2 ...) e0))))

;; 插入处理逻辑，不改变结果
(define-syntax hook
	(syntax-rules
		()
		((_ e0 e1 ...)
		 (let [(f (pipe e0 e1 ...))]
			 (lambda (x)
				 (f x)
				 x)))))

;; 将列表转化成序列
(define list->xl
	(lambda (ls)
		(lambda (cb)
			(for-each cb ls))))

;; 将函数转化成序列
(define proc->xl identity)

;; 降序列转成列表
(define xl->list
	(lambda (xl)
		(let [(ls '())]
			(define (collect x)
				(set! ls (cons x ls)))
			(xl collect)
			(reverse ls))))

;; 将单个值封装成序列，类似haskell monad 的return函数
(define xl-identity
	(lambda (x)
		(lambda (cb)
			(cb x))))

;; 空序列
(define xl-nothing
	(lambda (x)
		void))

;; 序列绑定操作，等同于hashekll monad的>>=操作
(define (x-bind f)
	(lambda (xl)
		(lambda (cb)
			(xl
				(lambda (x)
					((f x) cb))))))

(define-syntax xl-bind
	(syntax-rules ()
		((_ e0 e1 ...) (x-bind (pipe e0 e1 ...)))))

;; 序列映射操作，类似于普通的map操作
(define (x-map f)
	(x-bind (compose f xl-identity)))

(define-syntax xl-map
	(syntax-rules ()
		((_ e0 e1 ...) (x-map (pipe e0 e1 ...)))))

;; 序列过滤，类似于普通的filter操作
(define (x-filter f)
	(x-bind
		(lambda (x)
			(if (f x)
				(xl-identity x)
				(xl-nothing x)))))

(define-syntax xl-filter
	(syntax-rules ()
		((_ e0 e1 ...) (x-filter (pipe e0 e1 ...)))))

;; 序列遍历，类似于普通的for-each操作
(define (x-hook f)
	(x-map (hook f)))

(define-syntax xl-hook
	(syntax-rules ()
		((_ e0 e1 ...) (x-hook (pipe e0 e1 ...)))))

;; 序列截断，类比于haskell monad的takeWhile操作
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

;; 强制执行序列中的逻辑
(define xl->void
	(lambda (xl)
		(xl void)
		(void)))

;; 自然数序列
(define xl-N
	(lambda (cb)
		(define (loop n)
			(cb n)
			(loop (+ 1 n)))
		(loop 0)))

;; --------- test ---------
;;
(define println
	(lambda (x)
		(display x)
		(newline)))


(define add1
	(lambda (x)
		(+ x 1)))

(define add2 (pipe add1 add1))

(define (square x) (* x x))

(define (counter)
	(let [(c 0)]
		(define (count . x)
			(let [(ret c)]
				(set! c (+ c 1))
				ret))
		count)) 

(define (gt n)
	(lambda (x) (> x n)))

(define (gte n)
	(lambda (x) (>= x n)))

(define (lte n)
	(lambda (x) (<= x n)))

(define (lt n)
	(lambda (x) (< x n)))

(define (eq n)
	(lambda (x) (= x n)))

(define (ne n)
	(lambda (x) (not (= x n))))

(water
	xl-N ;;所有自然数
	(xl-until (counter) (gte 10)) ;; 直到 计数器大于等于10，即前10个
	(xl-map square) ;;每个数的平方
	(xl-filter (gte 10)) ;;过滤出大于10的数
	(xl-hook println) ;;打印数据
	xl->void)
