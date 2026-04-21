;; --- Vector Implementation via Tagged Lists ---

(define (get-current-procedure-name)
  (call-with-current-continuation
    (lambda (c)
      (with-exception-handler
        (lambda (e) 'unknown)
        (lambda () (string->symbol
               (cadr (string-split (format "~s" c) " "))))))))

(define (trace-pr name f)
  (lambda args
    (printf "procedure: ~s\n" name)
    (let ((res (apply f args)))
      (printf "ret ~a: ~a~%" name res)
      res)))
;;;;;;;;
;; Creates a new 'vector' (variable)
;; Representation: ('vector-tag' value)
;; (define (var c) 
; (list 'vector-tag c))
;
;;; Checks if x is a 'vector' (variable)
;(define (var? x)
;  (if (pair? x)
;      (eq? (car x) 'vector-tag)
;      #f))
;
;;; Checks if two 'vectors' are equal by comparing their internal counters
;(define (var=? x1 x2)
;  (eq? (cadr x1) (cadr x2)))
;;;;;;;;

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2)
  (= (vector-ref x1 0)
     (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))
(define (ext-s x v s) `((, x . , v) . , s))
(define (equalo u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(, s . , (cdr s/c))) mzero))))
(define (unit s/c) (cons s/c mzero))
(define mzero '())
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
;;    (printf "Stream : ~a~%" s)
    (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))
(define (call/fresh f)
  (lambda (s/c)
;;    (printf "Stream : ~a~%" s)
    (let ((c (cdr s/c)))
      ((f (var c)) `(, (car s/c) . , (+ c 1)))))) 
(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))
(define (mpluso $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus ($2) $1))) ;; invert $1 and $2 to have infinity paths
   (else (cons (car $1) (mplus (cdr $1) $2)))))
(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))
(define (bind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (bind ($) g)))
   (else (mplus (g (car $)) (bind (cdr $) g)))))

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))
(define-syntax conj+
  (syntax-rules ()
    (( _ g) (Zzz g))
    (( _ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))
(define-syntax disj+
  (syntax-rules ()
    (( _ g) (Zzz g))
    (( _ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))
(define-syntax conde
  (syntax-rules ()
    (( _ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))
(define-syntax fresh
  (syntax-rules ()
    (( _ () g0 g ...) (conj+ g0 g ...))
    (( _ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

(define (pull $) (if (procedure? $) (pull ($)) $))
(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) ' () (cons (car $) (take-all (cdr $))))))
(define (take n $)
  (if (zero? n) ' ()
      (let (($ (pull $)))
        (cond
          ((null? $) ' ())
          (else (cons (car $) (take (- n 1) (cdr $))))))))

(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))
(define (reify-state/1st-var s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v ' ()))))
(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v)
      (let ((n (reify-name (length s))))
        (cons `(, v . , n) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))
(define (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else v))))

(define empty-state ' (() . 0))
(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (mK-reify (take n (call/empty-state
                        (fresh (x ...) g0 g ...)))))))
(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state
                          (fresh (x ...) g0 g ...)))))))

(define (occurs x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (var=? v x))
     (else (and (pair? v) (or (occurs x (car v) s)
                              (occurs x (cdr v) s)))))))
(define (ext-s x v s)
  (if (occurs x v s) #f `((,x . , v) . , s)))

(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus $2 (cdr $1))))))





;(trace unit)
; (trace unify)
; (trace call/fresh)
; (trace walk)
; (trace equalo)
; (trace run)
; (trace run*)


(define empty-state ' (() . 0))
(define a-and-b
  (conj
   (call/fresh (lambda (a) (equalo a 7)))
   (call/fresh (lambda (b) (disj (equalo b 5) (equalo b 6))))))


;; ((call/fresh (lambda (q) (equalo q 5))) empty-state)
;; (a-and-b empty-state)
;; (run 1 (x y) (conj+ (equalo 'split x) (equalo 'pea y)))

;;(run* (r)
;;     (fresh (x y)
;;            (disj+
;;             (conj+ (equalo 'split x)
;;                    (equalo 'pea y))
;;             (conj+ (equalo 'red x)
;;                    (equalo 'bean y)))
;;            (equalo `(,x ,y soup) r)))



;; ------------------------------------------------------------
;; Helper list relations (conso, nullo)
;; ------------------------------------------------------------
(define (conso a d l)
  (equalo (cons a d) l))

(define (nullo l)
  (equalo l '()))

(define-syntax all
  (syntax-rules ()
    ((_ g ...) (conj+ g ...))))

;; ------------------------------------------------------------
;; Test 1: Basic unification
;; ------------------------------------------------------------
#|
(display "\n=== Test 1: Basic unification ===\n")
(run* (q)
  (fresh (x y)
    (equalo x 5)
    (equalo y 6)
(equalo (list x y) q)))
|#
;; Expected: ((5 6))

;; ------------------------------------------------------------
;; Test 2: Deduction rule (as in Python example)
;; ------------------------------------------------------------
(display "\n=== Test 2: Deduction rule ===\n")


;; rules for apply use of minikanren/microkanren
(define (deduction premise1 premise2 conclusion)
  (fresh (M P S)
    (equalo premise1 (list '-> M P))
    (equalo premise2 (list '-> S M))
    (equalo conclusion (list '-> S P))))

(define trace-on #f)
(define timely #f)

(define (check-and-trace)
  (cond 
   [trace-on
    (begin
      (trace bind)
      (trace mplus)
      (trace ext-s)
      (trace unify) ;; core
      (trace call/fresh) ;; core
      (trace walk) ;; core
      (trace equalo))] ;; core
   [else
    (begin
      (untrace bind)
      (untrace mplus)
      (untrace ext-s)
      (untrace unify)
      (untrace call/fresh)
      (untrace walk)
      (untrace equalo))]))


(define (first-result lst)
  (if (and (list? lst)
           (list? (car (lst))))
      lst
      (car lst)))

;; helper
(define (append-map f lst)
  (let loop ((lst lst) (result '()))
    (if (null? lst)
        (reverse result)
        (loop (cdr lst)
              (append (reverse (f (car lst))) result)))))

;; Helper: remove first occurrence of element from list
(define (remove x lst)
  (cond ((null? lst) '())
        ((equal? (car lst) x) (cdr lst))
        (else (cons (car lst) (remove x (cdr lst))))))

;; Helper: generate all combinations no repeat of size k from list lst
(define (combinations lst k)
  (cond
    ((= k 0) '(()))
    ((null? lst) '())
    (else
     (let ((first (car lst))
           (rest (cdr lst)))
       (append (map (lambda (c) (cons first c))
                    (combinations rest (- k 1)))
               (combinations rest k))))))
;; Generate all permutations of size k from list lst (order matters, no repetition)
(define (permutations lst k)
  (cond
    ((= k 0) '(()))
    ((null? lst) '())
    (else
     (append-map (lambda (first)
                   (map (lambda (rest) (cons first rest))
                        (permutations (remove first lst) (- k 1))))
                 lst))))

;; Step 2: backward deduction? Actually deduce-one expects two premises;
;; the original code uses (deduction Q step1 fact4) which is "backward":
;; find Q such that (Q -> step1) and (step1 -> fact4) give fact4.
;; But note: deduction is not symmetric; better to define a backward helper.
(define (backward-deduce conclusion premise2)
  ;; Find premise1 such that (deduction premise1 premise2 conclusion)
  (run 1 (p1) (deduction p1 premise2 conclusion)))


;; Helper: deduce one step
(define (deduce-one p1 p2)
  (let ((res (run 1 (q) (deduction p1 p2 q))))
    (if (null? res) #f (car res))))

(define run-step-only
  (lambda (app fact1 fact2)
    (check-and-trace)
    (let ((step-one (cond [timely
                           (time (app fact1 fact2))]
                          [else
                           (app fact1 fact2)])))
      step-one)))

(define run-step
  (lambda (app fact1 fact2)
    (check-and-trace)
    (let ((step-one (cond [timely
                           (time (app fact1 fact2))]
                          [else
                           (app fact1 fact2)])))
      (cond [(eq? app backward-deduce)
             (printf "To get ~s using ~s, need premise: ~s\n" step-one fact1 fact2)]
            [else
             (printf "From ~s and ~s deduce ~s\n" fact1 fact2 step-one)])
      step-one)))

(define (convolution lst)
  (define (member? x l)
    (cond ((null? l) #f)
          ((equal? x (car l)) #t)
          (else (member? x (cdr l)))))
  (define (check-pise p l)
    (if (and p (not (member? p l)))
        p '()))
  (define (append-nempty l acc)
    (if (null? l)
        acc
        (if (not (null? (car l)))
            (append-nempty (cdr l) (cons (car l) acc))
            (append-nempty (cdr l) acc))))
  (define (add-if-new i l)
    (if (check-pise i l) (cons i l) l))
  (define (convolution-acc l acc)
    (if (null? l)
        acc
        (let* ((p (car l))
              (r1 (run 1 (q) (deduction (car p) (cadr p) q))))  ;; deduction
              ; (r2 (run 1 (q) (deduction (car p) q (cadr p))))  ;; abdution
              ; (r3 (run 1 (q) (deduction q (car p) (cadr p))))) ;; induction
          (printf "-> ~a ~a : ~a~%" (car p) (cadr p) r1)
          (convolution-acc (cdr l)
                           (add-if-new r1 acc))))); (add-if-new r2 (add-if-new r3 acc)))))))
  (define (convolution-acc-old p l acc)
    (if (null? l)
        acc
        (let ((r1 (run 1 (q) (deduction (car p) (cadr p) q)))  ;; deduction
              (r2 (run 1 (q) (deduction (cadr p) q (car p))))  ;; abdution
              (r3 (run 1 (q) (deduction q (car p) (cadr p))))) ;; induction
          (printf "-> ~a ~a~%" (car p) (cadr p))
          (convolution-acc (car l)
                           (cdr l)
                           (cons (check-pise r1 acc) acc)
                                 (cons (check-pise r2 acc)
                                       (cons (check-pise r3 acc) acc))))))
  (check-and-trace)
  (append-nempty
   (convolution-acc lst '()) '()))


;; flags
(define trace-on #f)
(define timely #f)

;; Facts
; (define fact1 '(-> bird animal))
; (define fact2 '(-> robin bird))
; (define fact3 '(-> robin sleep))
; (define fact4 '(-> eat animal))

;; step deductions
; (define step1 (run-step deduce-one fact1 fact2))
; (define step2 (run-step backward-deduce fact3 step1))
; (define step3 (run-step deduce-one fact4 step2))

(define fact1 '(-> bird animal))
(define fact2 '(-> robin eat))
(define fact3 '(-> animal living))
(define fact4 '(-> living mortal))

(define facts (list fact1 fact2 fact3 fact4))
(define permut-facts (permutations facts 2))

; (printf "facts :: ~a~%" facts)
; (define trace-on #t)
; (define trace-on #t)
; (check-and-trace)

(define-syntax trp
  (syntax-rules ()
    ((_ pron)
     (set! pron (trace-pr 'pron pron)))))

;; (set! walk (trace-pr 'walk walk))
; (set! ext-s (trace-pr 'ext-s ext-s))
;;;(trp ext-s)
;;;(trp unify)
;;;(trp equalo)
;;;(trp walk)

; (define ret (run 1 (q) (deduction '(-> living mortal) '(-> animal living) q)))
; (printf "RES: ~a~%" ret)


;; #|
(define convolution-lst (convolution permut-facts))

; (printf "permutation simbols: ~a~%" permut)
(printf "convolution :: ~a~%" convolution-lst)

(define new-facsts (append (map (lambda(x)(car x)) convolution-lst)
                           facts))

(define permut-new-facts (permutations new-facsts 2))
(printf "new facts :: ~a~%" new-facsts)
; (printf "new facts-permut :: ~a~%" permut-new-facts)
;; |#

#|
(define convolution-lst-2 (convolution permut-new-facts))
(printf "convolution-2 :: ~a~%" convolution-lst-2)

(define new-facsts-2 (append
                    (map (lambda(x)(car x)) convolution-lst-2)
                    (map (lambda(x)(car x)) convolution-lst)
                    facts))

(printf "~%~%initial facts :: ~a~%" facts)
(printf "new facts so far :: ~a~%" new-facsts-2)
|#
; (write (chain-deduction facts))



; (let* ((fact1 (list '-> 'bird 'animal))
;       (fact2 (list '-> 'robin 'bird))
;       (fact4 (list '-> 'robin 'sleep))
;       (fact6 (list '-> 'eat 'animal))
;       (fact3 (run* (Q)
;                         (deduction fact1 fact2 Q))))
;       (printf "deduction: ~a\n" fact3)
;       (let ((fact5 (run* (Q)
;                          (deduction Q (car fact3) fact4)))
;             (fact7 (run* (Q)
;                          (deduction fact6 (car fact3) Q))))
;         (printf "~a :: ~a~%" (symbol->string 'fact3) (car fact3))
;         (printf "~a :: ~a~%" (symbol->string 'fact5) (car fact5))
;         (printf "~a :: ~a~%" (symbol->string 'fact6) fact6)
;         (printf "backwards deduction from ~a to fact5: ~a\n" fact4 fact5)
;         (printf "try deduction from ~a to fact5: ~a\n" fact6 fact7)))
;; Expected: ((-> robin animal))

;; ------------------------------------------------------------
;; Test 3: Differento for domain 1..4 (explicit unequal pairs)
;; ------------------------------------------------------------

#|
(display "\n=== Test 3: Differento (domain 1..4) ===\n")
(define (differento x y)
  (conde
    ((equalo x 1) (equalo y 2))
    ((equalo x 1) (equalo y 3))
    ((equalo x 1) (equalo y 4))
    ((equalo x 2) (equalo y 1))
    ((equalo x 2) (equalo y 3))
    ((equalo x 2) (equalo y 4))
    ((equalo x 3) (equalo y 1))
    ((equalo x 3) (equalo y 2))
    ((equalo x 3) (equalo y 4))
    ((equalo x 4) (equalo y 1))
    ((equalo x 4) (equalo y 2))
    ((equalo x 4) (equalo y 3))))

;; Test differento

(run* (q)
  (fresh (x y)
    (differento x y)
(equalo (list x y) q)))
|#
;; This will produce 12 pairs (all ordered unequal pairs). Not printed here.

;; ------------------------------------------------------------
;; Test 4: all-differento for a list (using recursion)
;; ------------------------------------------------------------
#|
(display "\n=== Test 4: all-differento for a list of 4 variables ===\n")

(define (all-differento-old lst)
  (conde
    ((nullo lst))
    ((fresh (a d)
       (conso a d lst)
       (all-differento d)
       (all-diff-heado a d)))))

(define (all-differento lst)
  (conde
    ((nullo lst))
    ((fresh (a d)
       (conso a d lst)
       (all-differento d)        ;; a must be different from every element in d
       (fresh (b)
         ;; (conso b _ lst)   ; dummy to get first element? We need a loop. original one
         (conso b d lst)   ; dummy to get first element? We need a loop. 
         ;; Instead, we define a helper that checks a against all elements of d
         (all-diff-heado a d))))))

;; Helper: a must be different from all elements in list d
(define (all-diff-heado a d)
  (conde
    ((nullo d))
    ((fresh (b d2)
       (conso b d2 d)
       (differento a b)
       (all-diff-heado a d2)))))
|#
;; Test all-differento with four variables
#|
(run* (q)
  (fresh (a b c d)
    (all-differento (list a b c d))
    (equalo (list a b c d) q)))
;; This will generate all permutations of 1,2,3,4 – 24 solutions. Too many to print.
;; We can limit to first 5:
(display "First 5 permutations of 1..4:\n")
(run 5 (q)
  (fresh (a b c d)
    (all-differento (list a b c d))
    (equalo (list a b c d) q)))
|#
;; ------------------------------------------------------------
;; Test 5: 2×2 Sudoku (domain 1..2)
;; ------------------------------------------------------------
#|
(display "\n=== Test 5: 2×2 Sudoku (domain 1..2) ===\n")
(define (domain-2 x)
  (conde ((equalo x 1)) ((equalo x 2))))

;; We need a differento for domain 1..2
(define (differento-2 x y)
  (conde
    ((equalo x 1) (equalo y 2))
    ((equalo x 2) (equalo y 1))))

(define (all-differento-2 lst)
  (conde
    ((nullo lst))
    ((fresh (a d)
       (conso a d lst)
       (all-differento-2 d)
       (all-diff-heado-2 a d)))))

(define (all-diff-heado-2 a d)
  (conde
    ((nullo d))
    ((fresh (b d2)
       (conso b d2 d)
       (differento-2 a b)
       (all-diff-heado-2 a d2)))))

(define (solve-2x2 a b c d)
  (all (domain-2 a) (domain-2 b) (domain-2 c) (domain-2 d)
       (all-differento-2 (list a b))
       (all-differento-2 (list c d))
       (all-differento-2 (list a c))
       (all-differento-2 (list b d))))

(printf "solved : ~a~%"
 (time
  (run* (q)
        (fresh (a b c d)
               (solve-2x2 a b c d)
               (equalo (list a b c d) q)))))
;; Expected: ((1 2 2 1) (2 1 1 2))
|#
;; ------------------------------------------------------------
;; Test 6: 4×4 Sudoku (rows and columns only, no squares – slow, so commented)
;; ------------------------------------------------------------
#|
(display "\n=== Test 6: 4×4 Sudoku (rows & columns) ===\n")
(define (domain-4 x)
  (conde ((equalo x 1)) ((equalo x 2)) ((equalo x 3)) ((equalo x 4))))

(define (solve-4x4 a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)
  (all (domain-4 a1) (domain-4 a2) (domain-4 a3) (domain-4 a4)
       (domain-4 b1) (domain-4 b2) (domain-4 b3) (domain-4 b4)
       (domain-4 c1) (domain-4 c2) (domain-4 c3) (domain-4 c4)
       (domain-4 d1) (domain-4 d2) (domain-4 d3) (domain-4 d4)
       (all-differento (list a1 a2 a3 a4))
       (all-differento (list b1 b2 b3 b4))
       (all-differento (list c1 c2 c3 c4))
       (all-differento (list d1 d2 d3 d4))
       (all-differento (list a1 b1 c1 d1))
       (all-differento (list a2 b2 c2 d2))
       (all-differento (list a3 b3 c3 d3))
       (all-differento (list a4 b4 c4 d4))))

;; Partially filled puzzle (first row: 1,_,3,_)
(write
 (time
  (run 1 (q)
       (fresh (a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)
              (all (equalo a1 1) (equalo a3 3)
                   (solve-4x4 a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 d1 d2 d3 d4)
                   (equalo q (list (list a1 a2 a3 a4)
                                   (list b1 b2 b3 b4)
                                   (list c1 c2 c3 c4)
                                   (list d1 d2 d3 d4))))))))

(display "\nAll tests completed.\n")
|#
