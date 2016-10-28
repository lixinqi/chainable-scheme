
;; 示例函数定义和链式使用在形式上的不一致
(include "echo.scm")
(include "define-method.scm")
(define-method (add y z) (+ this y z))
(define-method (println)
	(display this)
	(newline))

(echo 0 | add 1 2 | println)


