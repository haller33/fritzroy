(load "fritzroy.scm")

(let ((n (eval-expr
		  '(((lambda (!)
			   (lambda (n)
				 ((! !) n)))
		     (lambda (!)
			   (lambda (n)
				 (if (zero? n)
					 1
					 (* n ((! !) (sub1 n)))))))
		    5)
		  (lambda(x)
		    (error 'lookup "envirompment")))))
  (format #t "Factorial of ~a is : ~a~%" 5 n))
