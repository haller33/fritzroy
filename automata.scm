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

;; this is a test using automatas and single dimentional automatas

;; #! /nix/store/jw2522hjypr3dv8v2sjk8gmk4jywi43w-user-environment/bin/scheme --script

;; #!/data/data/com.termux/files/usr/bin/guile -s
;; !#


(define rulepath '(((O O O) O)
		   ((O O V) V)
		   ((O V O) V)
		   ((O V V) V)
		   ((V O O) O)
		   ((V O V) V)
		   ((V V O) V)
		   ((V V V) O)))


(define (generate rows data)
  (define (prints-aps rows data string)
    (if (or (null? data)
	    (= rows 0))
	string
	(prints-aps (- rows 1)
		    (cdr data)
		    (string-append
		     (if (eqv? (car data) 'O)
			 " "
			 "#")
		     string))))
  (prints-aps rows data ""))



(define (rulet atom rules)
  (define (gettrew num ls acc)
      (if (= num 0)
	  (reverse acc)
	  (gettrew (- num 1)
		   (cdr ls)
		   (cons (car ls) acc))))
  (define (getrew lis)
    (gettrew 3 lis '()))
  (define (check cell rulein)
    (and (eqv? (car cell) (car rulein))
	 (eqv? (cadr cell) (cadr rulein))
	 (eqv? (caddr cell) (caddr rulein))))
  (define (resut cell n-rules)
    (if (check cell (caar n-rules))
	(cadar n-rules)
	(resut cell (cdr n-rules))))
  (define (plot acc atom-s)
    (cons (resut (cons (caddr atom-s) (gettrew 2 atom '())) rules)
	  (reverse (cons (resut (cons (car atom)
				atom-s)
			  rules)
		   acc))))
  (define (reuse acc atom-n)
    (plot acc atom-n))
  (define (rulet-aps atom-n acc)
    (if (null? (cdddr atom-n))
	(reuse acc atom-n)
	(rulet-aps (cdr atom-n)
		   (cons (resut (getrew acc)
			 rules)
		   acc))))
  (rulet-aps (cddr atom) '()))

(define (row-n words symb symbtwo)
  (define (row-n-aps num acc)
    (if (= num 0)
	(reverse acc)
	(row-n-aps (- num 1)
		   (cons (if (= num (+ (/ words 2)
				       (mod words 2)))
			     symbtwo
			     symb)
			 acc))))
  (row-n-aps words '()))

(define (begin-automata first)
  (format #t "~A~%" (generate words first))
  ;; (sleep speeds)
  (system "sleep 0.1")
  ;; (sleep (make-time 'time-duration 1000 1))
  (begin-automata (rulet first rulepath)))

;; (define speeds 2)
(define words 166)


(begin-automata (row-n words 'O 'V))






