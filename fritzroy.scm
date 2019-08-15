(load "pmatch.scm")

(define eval-expr
  (lambda (expr env)
    (pmatch expr
	    [,n (guard (number? n))
		n]
	    [#t
	     #t]
	    [#f
	     #f]
	    [(zero? ,e)
	     (zero? (eval-expr e env))]
	    [(add1 ,e)
	     (add1 (eval-expr e env))]
	    [(sub1 ,e)
	     (sub1 (eval-expr e env))]

	    [(* ,e1 ,e2)
	     (* (eval-expr e1 env)
		(eval-expr e2 env))]
	    [(if ,t ,c ,a)
	     (if (eval-expr t env)
		 (eval-expr c env)
		 (eval-expr a env))]
	    [,x (guard (symbol? x))
		(env x)]
	    [(lambda (,x) ,body)
	     (lambda (arg)
	       (eval-expr body (lambda(y)
				 (if (eq? x y)
				     arg
				     (env y)))))]
	    [(,rator ,rand)
	     ((eval-expr rator env)
	      (eval-expr rand env))])))
