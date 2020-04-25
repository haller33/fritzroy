#! /nix/store/n4a0i8qpi20cyrd2hfvpnwx93kphy7qg-user-environment/bin/scheme --script

;;;
;; this is my own definition of Meta Circular Evaluator.
;;

(define (inenv exp env)
  (define (ienvaps exp env acc)
    (if (or (null? env) acc)
	acc
	(ienvaps exp (cdr env)
		 (or (eqv? (caar env) exp)
		      acc))))
  (ienvaps exp env #f))


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
	 (extraenv exp env))
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
	       (evale (caddr exp) env)))))


;; debug mode
(trace evale)

(let ((n

       (evale 'n 
	      '((n nil)))))
  
  (format #t "~A~%" n))



