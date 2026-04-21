;; member
;; rember
;; filter
;; map

;;; test cases for member
;; (member 'x '()) => #f
;; (member 'x '(a x c d)) => #t
;; (member 'x '(x a b c)) => #t
;; (member 'x '(a b c x)) => #t
;; (member 'x '(a x c x)) => #t
;; (member 'x '(a b c d)) => #f
;; 
(define member
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((equal? (car l) x) #t)
     (else
      (member x (cdr l))))))

;;; test cases for rember/rember-all/deep-rember*
;; (rember 'x '()) => '()
;; (rember 'x '(a b)) => '(a b)
;; (rember 'x '(a x b)) => '(a b)
;; (rember 'x '(x a b)) => '(a b)
;; (rember 'x '(x a b x)) => '(a b)
;; (rember 'x '(a b x)) => '(a b)
;; 
(define rember-all
  (lambda (x l)
    (cond
     ((null? l) '())
     ((equal? (car l) x)
      (rember-all x (cdr l)))
     (else
      (cons (car l)
            (rember-all x (cdr l)))))))

;; (rember 'x '()) => '()
;; (rember 'x '(a b)) => '(a b)
;; (rember 'x '(a x b)) => '(a b)
;; (rember 'x '(x a b)) => '(a b)
;; (rember 'x '(x a b x)) => '(a b x)
;; (rember 'x '(a b x)) => '(a b)
(define rember
  (lambda (x l)
    (cond
     ((null? l) '())
     ((equal? (car l) x)
      (cdr l))
     (else
      (cons (car l)
            (rember x (cdr l)))))))

;;; test case for deep-rember*
;; (deep-rember* 'x '()) => '()
;; (deep-rember* 'x '((x) a b)) => '(() a b)
;; (deep-rember* 'x '(a (x c) d)) => '(a (c) d)
;; (deep-rember* 'x '(a d (a x c))) => '(a d (a c))
;; (deep-rember* 'x '(a d c)) => '(a d c)
;; (deep-rember* 'x '(a d c x)) => '(a d c)
;;
(define deep-rember*
  (lambda (x l)
    (cond
     ((null? l) '())
     ((equal? (car l) x)
      (deep-rember* x (cdr l)))
     ((pair? (car l))
      (cons (deep-rember* x (car l))
            (deep-rember* x (cdr l))))
     (else
      (cons (car l)
            (deep-rember* x (cdr l)))))))



;;; test cases for filter
;;
;; (filter (lambda (x) (equal? x 2)) '()) => '()
;; (filter (lambda (x) (equal? x 2)) '(2 3 2)) => '(2 2)
;; (filter (lambda (x) (equal? x 2)) '(3)) => '()
;;
(define filter
  (lambda (f l)
    (cond
     ((null? l)
      '())
     ((f (car l))
      (cons (car l) (filter f (cdr l))))
     (else
      (filter f (cdr l))))))

;; filter in
(define filter-in
  (lambda (f l)
    (cond
     ((null? l)
      '())
     ((f (car l))
      (cons (car l) (filter-in f (cdr l))))
     (else
      (filter-in f (cdr l))))))

;; filter out
(define filter-out
  (lambda (f l)
    (cond
     ((null? l)
      '())
     ((f (car l))
      (filter-out f (cdr l)))
     (else
      (cons (car l) (filter-out f (cdr l)))))))

;; filter out simple
(define filter-out-s
  (lambda (f l)
    (cond
     ((null? l)
      '())
     ((not (f (car l)))
      (cons (car l) (filter-out-s f (cdr l))))
     (else
      (filter-out-s f (cdr l))))))

;; filter-out-acc
;;
;; (filter-acc odd? '(1 2 3 4) '())
(define filter-acc
  (lambda (f l acc)
    (cond
     ((null? l)
      acc)
     ((not (f (car l)))
      (filter-acc f (cdr l) (cons (car l) (cdr l))))
     (else
      (filter-acc f (cdr l) (cdr l))))))


;; filter-out-cps
;;
;; (filter-cps odd? '(1 2 3 4 5 6 7 8 9) (lambda(x) x)) => '(2 4 6 8)
(define filter-cps
  (lambda (f l cc)
    (cond
     ((null? l)
      (reverse (cc l)))
     ((not (f (car l)))
      (filter-cps f (cdr l) (lambda (ll)
                              (cons (car l) (cc ll)))))
     (else
      (filter-cps f (cdr l) (lambda (ll)
                              (cc ll)))))))

;;; filter-out-cps-no-reverse
;;
;; (filter-cps-nr odd? '(1 2 3 4 5 6 7 8 9) (lambda(x) x)) => '(2 4 6 8)
(define filter-cps-nr
  (lambda (f l cc)
    (cond
     ((null? l)
      (cc l))
     ((not (f (car l)))
      (filter-cps-nr f (cdr l) (lambda (ll)
                                 (cc (cons (car l) ll)))))
     (else
      (filter-cps-nr f (cdr l) (lambda (ll)
                              (cc ll)))))))


;;; test cases for map
;;
;; (map (lambda (X) x) '()) => '()
;; (map (lambda (X) x) '(1 2 3)) => '(1 2 3)
;; (map (lambda (X) (* 2 x) '(1 2 3)) => '(2 4 6)
;; (map (lambda (x) (equal? x 2)) '(1 2 3 'a))  => '(#f #t #f #f)
;;
(define map
  (lambda (f l)
    (cond
     ((null? l)
      '())
     (else
      (cons (f (car l)) (map f (cdr l)))))))
