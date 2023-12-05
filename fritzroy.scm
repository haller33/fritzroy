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

;; fritzroy inicial implementation..

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
