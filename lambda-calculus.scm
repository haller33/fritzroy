(load "pmatch.scm")

(define eval-lambda-expr
  (lambda (expr env)
    (pmatch expr
	    [,x (guard (symbol? x))
		      (env x)]
	    [(lambda (,x) ,body)
	     (lambda (arg)
	       (eval-lambda-expr body
                           (lambda(y)
				                     (if (eq? x y)
				                         arg
				                         (env y)))))]
	    [(,rator ,rand)
	     ((eval-lambda-expr rator env)
	      (eval-lambda-expr rand env))])))

;;'((lambda (!)
;;			   (lambda ()
;;				   ((! !))))
;;		   (lambda (!)
;;			   (lambda ()
;;				   ((! !)))))
;;

(define run
  (lambda ()
    (eval-lambda-expr
     '(lambda(x)x)
     (lambda(x)
	     (error 'x "envirompment")))))
