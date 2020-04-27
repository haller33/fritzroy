#!/data/data/com.termux/files/usr/bin/guile -s
!#

;;;
;; this is my own definition of Meta Circular Evaluator.
;;

(define (inenv? exp env)
  (define (ienvaps exp env acc)
    (if (or (null? env) acc)
	acc
	(ienvaps exp (cdr env)
		 (or (eqv? (caar env) exp)
		     acc))))
  (ienvaps exp env #f))


(define (extraenv exp env)
  (if (eqv? (caar env) exp)
      (cadar env)
      (extraenv exp (cdr env))))

(define (insidenv add env)
  (if (null? add)
      env
      (insidenv (cdr add)
		(cons (car add)
		      env))))

(define (evale exp env)
  (cond ((eqv? exp 'nil)
	 '())
	((eqv? exp '#f)
	 #f)
	((eqv? exp '#t)
	 #t)
        ((null? exp)
	 'nil)
	((number? exp)
	 exp)
	((inenv? exp env)
	 (evale (extraenv exp env) env))
	((eqv? (car exp) 'set)
	 (evale (caddr exp)
		(cons (cons (cadr exp)
			    (evale (caddr exp) env))
		      env)))
	((eqv? (car exp) 'let)
	 (evale (caddr exp)
		(insidenv (cadr exp) env)))
	((eqv? (car exp) 'quot)
	 exp)
	((eqv? (car exp) '==)
	 (eqv? (evale (cadr exp) env)
	       (evale (caddr exp) env)))
	((eqv? (car exp) '>)
	 (> (evale (cadr exp) env)
	    (evale (caddr exp) env)))	
	((eqv? (car exp) '<)
	 (< (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'sub1)
	 (+ (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'mult1)
	 (* (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'if)
	 (if (evale (cadr exp) env)
	     (evale (caddr exp) env)))
	((eqv? (car exp) 'cons)
	 (cons (evale (cadr exp) env)
	       (evale (caddr exp) env)))
	(#t
	 (write 'error)
	 (write exp)
	 (write env))))


;; debug mode
;; (trace evale)


(let ((n
       
       (evale '(let ((x 7))
		 (let ((h n))
		   (sub1 h x)))
	      '((j 88)(n 5)))))
  
  (format #t "~A~%" n))



