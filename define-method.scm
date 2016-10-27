(define-syntax define-method
  (lambda (x) 
    (syntax-case x ()
    [(define-method (f a ...) e ...)
      (datum->syntax (syntax k) (syntax->datum #`(define (f this a ...) e ...)))])))
