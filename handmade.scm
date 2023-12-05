;; MIT License
;;
;; Copyright (c) 2023 haller33
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; this is the inicial release of the project handmade interpreter...

;;#! /nix/store/jw2522hjypr3dv8v2sjk8gmk4jywi43w-user-environment/bin/scheme --script

;; #!/data/data/com.termux/files/usr/bin/guile -s
;; !#
;;
;; !#
; 


;;;
;; this is my own definition of Meta Circular Evaluator
;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (inenv? exp env)
  (define (ienvaps env acc)
    (if (or (not (list exp))
	    (null? env) acc)
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
  (define (inside frame env)
    (if (or (null? frame) (number? frame))
	env
	(inside (cdr frame)
		  (cons (car frame)
			env))))
  (inside frame env))
  

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


(define (dothis exp env) ;; TODO: create cps on the fly
  (if (not (null? (cdr exp)))
      (evale (list 'system-cps-done
		   'system-list-cps-done
		   '(lambda(x)(write x)))
	     (cons
	      (list 'system-list-cps-done
		    exp)
	      (cons (list 'system-cps-done
			  '(lambda(lis cc)
			     (if (null? (cdr lis))
				 (cc lis)
				 (system-cps-done
				  (cdr lis)
				  (lambda(x)
				    (cc lis))))))
		    env)))))


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
	((inenv? (car exp) env)
	 (evale (cons (extraenv (car exp) env)
		      (cdr exp)) env))
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
	 exp)
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
	((eqv? (car exp) 'and)
	 (and (evale (cadr exp) env)
	      (evale (caddr exp) env)))
	((eqv? (car exp) 'not)
	 (not (evale (cadr exp) env)))
	((eqv? (car exp) 'or)
	 (or (evale (cadr exp) env)
	     (evale (caddr exp) env)))
	((eqv? (car exp) 'null?)
	 (null? (evale (cadr exp) env)))

	((eqv? (car exp) '>)
	 (> (evale (cadr exp) env)
	    (evale (caddr exp) env)))	
	((eqv? (car exp) '<)
	 (< (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'sub1)
	 (- (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'some1)
	 (+ (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'mult1)
	 (* (evale (cadr exp) env)
	    (evale (caddr exp) env)))
	((eqv? (car exp) 'if)
	 (evale (if (evale (cadr exp) env)
		    (caddr exp)
		    (cadddr exp)) env))
	((eqv? (car exp) 'list)
	 (list (evale (cadr exp) env)
	       (evale (caddr exp) env)))
	((eqv? (car exp) 'cons)
	 (cons (evale (cadr exp) env)
	       (evale (caddr exp) env)))
	((eqv? (car exp) 'car)
	 (car (evale (cadr exp) env)))
	((eqv? (car exp) 'cdr)
	 (cdr (evale (cadr exp) env)))
	
	((eqv? (car exp) 'system-env)
      	 (list 'quot env))
	((eqv? (car exp) 'system-gc)
	 (evale 'nil (gc env)))

	((and (not (null? (cdr exp)))
	      (eqv? (caar exp) 'lambda))
	 (evale (list (evale (car exp) env)
		      (cdr exp)) env))
	((list? exp)
	 (applye exp env))

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
			    (cadar exp))) (car (cdddar exp))))
	(#t
	 exp)))

;; debug mode
(trace evale)
;; (trace inenv?)
(trace extraenv)
;; (trace insidenv)
;; (trace outsidenv)
(trace applye)
;; (trace mapargs)
(trace dothis)




;(let ((n
;       (evale '(do
;		   (set n 99)
;		   (write "hello world")
;		 (lambda(x)x))
;	      '())))
;  (format #t "~A~%" n))


