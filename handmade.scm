#!/data/data/com.termux/files/usr/bin/guile -s
!#
;; #! /nix/store/jw2522hjypr3dv8v2sjk8gmk4jywi43w-user-environment/bin/scheme --script


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

(define (dothis exp env)
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
		     (cadr exp))))
	((eqv? (car exp) 'define)
	 (evale (if (list? (cadr exp))
		    'nil
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
	
	((and (symbol? (car exp))
	      (symbol? (cadr exp))
	      (null? (cddr exp)))
	 (applye (list (evale (car exp) env)
		       (evale (cdr exp) env))))

	(#t
	 (write 'error)
	 (write exp)
	 (write env)
	 'nil)))


(define (applye exp env)
  (cond ((eqv? (caaar exp) '<PROCEDURE>)
	 (evale (cons 'let
		      (list (list (cadddr exp))
			    (caddr exp)) env)))))
		 


;; debug mode
;; (trace evale)
;; (trace inenv?)
;; (trace extraenv)
;; (trace insidenv)
;; (trace outsidenv)
;; (trace gc)


(let ((n
       (evale '(let ((n 88))
		 (define g 77))
	      
	      '((j 88)(n 5)))))
  (format #t "~A~%" n))



