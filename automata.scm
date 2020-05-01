#!/data/data/com.termux/files/usr/bin/guile -s
!#


(define rulepath '(((O O O) O)
		   ((O O V) V)
		   ((O V O) O)
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
		     (symbol->string (car data))
		     string))))
  (prints-aps rows data ""))



(define (rulet atom rules)
  (define (checkrow lis blis)
    (if (< (length lis) 3)
	(checkrow (cons (car blis)
		      lis)
		(cdr blis))
	(getrew lis)))
  (define (getrew lis)
    (define (gettrew num ls acc)
      (if (= num 0)
	  (reverse acc)
	  (gettrew (- num 1)
		   (cdr ls)
		   (cons (car ls) acc))))
    (gettrew 3 lis '()))
  (define (check cell rulein)
    (and (eqv? (car cell) (car rulein))
	 (eqv? (cadr cell) (cadr rulein))
	 (eqv? (caddr cell) (caddr rulein))))
  (define (resut cell n-rules)
    (if (check cell (caar n-rules))
	(cadar n-rules)
	(resut cell (cdr n-rules))))
  (define (reuse acc)
    (cons (car atom) (reverse (cons (car atom) acc))))
  (define (rulet-aps atom-n acc)
    (if (null? atom-n)
	(reuse acc)
	(rulet-aps (cdr atom-n)
		   (cons (resut (checkrow atom-n atom)
			 rules)
		   acc))))
  (rulet-aps atom '()))

(define (row-n num symb)
  (define (row-n-aps num acc)
    (if (= num 0)
	acc
	(row-n-aps (- num 1)
		   (cons symb acc))))
  (row-n-aps num '()))

(define (begin-automata first)
  (format #t "~A~%" (generate words (rulet first rulepath)))
  (sleep speeds)
  (begin-automata (rulet first rulepath)))

(define speeds 1)
(define words 85)


(begin-automata (row-n words 'V))






