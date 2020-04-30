#! /nix/store/jw2522hjypr3dv8v2sjk8gmk4jywi43w-user-environment/bin/scheme --script
;;#!/data/data/com.termux/files/usr/bin/guile -s
;; !#
; 


;;;
;; this is my own definition of Meta Circular Evaluator
;;

(define (inenv? exp env)
  (define (ienvaps env acc)
    (if (or (null? env) acc)
	acc
	(ienvaps (cdr env)
		 (or (eqv? (caar env) exp)
		     acc))))
  (ienvaps env #f))


(define (extraenv exp env)
  (if (eqv? (caar env) exp)
      (cadar env)
      (extraenv exp (cdr env))))


(define (insidenv frame env)
  (if (or (null? frame) (number? frame))
      env
      (insidenv (cdr frame)
		(cons (car frame)
		      env))))

(define (outsidenv frame env)
  (if (or (null? env) (number? frame))
      env
      (insidenv (cdr frame)
		(cdr env))))

(define (gc env)
  (define (exist? item lst)
    (and (not (null? lst))
	 (or (eqv? item (caar lst))
	     (exist? item (cdr lst)))))
  (define (gc-aps env acc)
    (if (null? env)
	(reverse acc)
	(gc-aps (cdr env)
		(if (not (exist? (caar env) acc))
		    (cons (car env) acc)
		    acc))))
  (gc-aps env (list (car env))))

(define (dothis exp env) ;; TODO
  (if (null? (cdr exp))
      (caar exp)
      (evale 'nil env)))


(define (evale exp env)
  (cond ((eqv? exp 'nil)
	 '())
	((eqv? exp '#f)
	 #f)
	((eqv? exp '#t)
	 #t)
	((string? exp)
	 exp)
        ((null? exp)
	 'nil)
	((number? exp)
	 exp)
	((inenv? exp env)
	 (evale (extraenv exp env) env))
	((eqv? (car exp) '<PROCEDURE>)
	 exp)
	((eqv? (car exp) 'do) ;; TODO
	 (dothis (cdr exp) env))
	((eqv? (car exp) 'set)
	 (evale (caddr exp)
		(cons (list (cadr exp)
			    (evale (caddr exp) env))
		      env)))
	
	((eqv? (car exp) 'let)
	 (evale (evale (caddr exp)
		       (insidenv (cadr exp) env))
		(outsidenv (cadr exp) env)))
	
	((eqv? (car exp) 'lambda)
	 (cons '<PROCEDURE>
	       (list (caddr exp)
		     (cadr exp)
		     env)))
	((eqv? (car exp) 'define)
	 (evale (if (list? (cadr exp))
		    (list 'set
			  (caadr exp)
			  (list 'lambda (cdadr exp)
				(caddr exp)))
		    (cons 'set
			  (list (cadr exp)
				(caddr exp))))
		env))
	((eqv? (car exp) 'quot)
	 (cadr exp))
	((eqv? (car exp) 'eval)
	 (evale (cadadr exp) '()))
	((eqv? (car exp) 'evil)
	 (evale (cadadr exp)
		(evale (cadddr exp) env)))
	((eqv? (car exp) 'write)
	 (write	(evale (cadr exp) env))
	 'nil)
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
	 (- (evale (caddr exp) env)
	    (evale (cadr exp) env)))
	((eqv? (car exp) 'some1)
	 (+ (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'mult1)
	 (* (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'if)
	 (if (evale (cadr exp) env)
	     (evale (caddr exp) env)))
	((eqv? (car exp) 'list)
	 (list (evale (cadr exp) env)
	       (evale (caddr exp) env)))
	((eqv? (car exp) 'cons)
	 (cons (evale (cadr exp) env)
	       (caddr exp)))
	((eqv? (car exp) 'car)
	 (car (evale (cadr exp) env)))
	((eqv? (car exp) 'cdr)
	 (cdr (evale (cadr exp) env)))
	((eqv? (car exp) 'system-env)
      	 (evale (list 'quot
		      env) env))
	((eqv? (car exp) 'system-gc)
	 (evale 'nil (gc env)))
	
	((eqv? (caar exp) 'lambda)
	 (evale (list (evale (car exp) env)
		      (cdr exp)) env))
	((and (list? (car exp))
	      (null? (cddr exp)))
	 (applye (list (evale (car exp) env)
		       (cadr exp)) env))

	(#t
	 (write 'error)
	 (write exp)
	 (write env)
	 'nil)))


(define (mapargs variables args env) ;; TODO: Refactory
  (define (mapargs-aps variables args acc)
    (if (or (atom? args) (null? args) (null? variables))
	acc
	(mapargs-aps (cdr variables)
		     (cdr args)
		     (cons (list (car variables)
				 (evale (car args) env))
			   acc))))
  (if (atom? args)
      (list (list (car variables) args))
      (mapargs-aps variables args '())))


(define (applye exp env)
  (cond ((eqv? (caar exp) '<PROCEDURE>)
	 (evale (cons 'let
		      (list (mapargs (caddar exp) (cadr exp) (car (cdddar exp)))
			    (cadar exp))) (car (cdddar exp))))))

;; debug mode
(trace evale)
(trace inenv?)
(trace extraenv)
(trace insidenv)
(trace outsidenv)
(trace applye)
(trace mapargs)
(trace gc)


(let ((n
       (evale '(((lambda (x)
		  (lambda (s)
		    (sub1 x s))) 55) 2)
	      
	      '((j 88)(n 5)))))
  (format #t "~A~%" n))



