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


;;; there are little bug on this core using on chez scheme

(define type?
  (lambda (x)
    (cond ((number? x) 'Number)
          ((pair? x) 'Pair)
          ((string? x) 'String)
          ((list? x) 'List)
	      (((procedure? x) 'Procedure)))))

(define I
  (lambda (i)
    i))

(define lazy
  (lambda (arg)
    (lambda (fn)
      (fn arg))))

;; (a) cons
(define ocons
  (lambda (itemfor listfor)
    (lambda (fn)
      (fn itemfor (lazy listfor)))))

(define ocdr
  (lambda (ohcons)
    (ohcons (lambda(fr sq)
		      (sq I)))))


(define ocar
  (lambda (ohcons)
    (ohcons (lambda(fr sq)
		      fr))))

(define typenow
  (lambda (typed somethingtotype)
    (ocons typed somethingtotype)))

(define typelistfor
  (lambda (typecheck)
    (lambda (tocheck)
      (if (eqv? (ocdr tocheck) typecheck)
	      #t
	      #f))))

(define typelistp (typelistfor 'list))
(define typelistnolp (typelistfor 'nol))

(define isfunction
  (lambda (item)
    (eqv? (type? item) 'Procedure)))

(define getypen
  (lambda (some)
    (if (isfunction some)
	    (ocar some)
	    some)))


(define ourcons
  (lambda (somea someb)
    (if (isfunction someb)
	    (typenow 'list (ocons somea someb))
	    (typenow 'nol (ocons somea someb)))))


(define nolp
  (lambda (item)
    (if (not (isfunction item))
	    (eqv? item 'nol)
	    #f)))

(define nocar
  (lambda (item)
    (if (nolp item)
	    'nol
	    (ocar (getypen item)))))

(define nocdr
  (lambda (item)
    (if (nolp item)
	    'nol
	    (ocdr (getypen item)))))


(define onullp
  (lambda (item)
    (if (eqv? (nocar item) 'nol)
	    #t
	    #f)))


(define olist
  (lambda (&rest symbolic)
    (define olist-aps
      (lambda (acc lst)
	    (if (null? (car lst))
	        acc
	        (olist-aps (ourcons (car lst)
				                acc)
		               (cdr lst)))))
    (olist-aps 'nol (reverse symbolic))))


(define olength
  (lambda (olist)
    (define olength-aps
      (lambda (cont tsl)
	    (if (onullp tsl)
	        cont
	        (olength-aps (+ cont 1)
			             (nocar tsl)))))
    (olength-aps 1 olist)))

(define omember
  (lambda (item tsl)
    (define omember-aps
      (lambda (lst item)
	    (cond ((onullp lst)
	           'nol)
	          ((eqv? (nocdr lst) item)
	           lst)
	          (#t
	           (omember-aps (nocar lst)
			                item)))))
    (omember-aps tsl item)))
9
