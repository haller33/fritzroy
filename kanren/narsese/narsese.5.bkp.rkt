#lang racket
;;;
;;; Narsese Interpreter for Non-Axiomatic Logic (NAL-1 to NAL-3)
;;; Based on "Non-Axiomatic Logic: A Model of Intelligent Reasoning" by Pei Wang
;;;

(require racket/port racket/string)

;; -------------------------------
;; Utility functions
;; -------------------------------

(define (string-contains str substr)
  (let ((len1 (string-length str))
        (len2 (string-length substr)))
    (if (> len2 len1) #f
        (let loop ((i 0))
          (if (> i (- len1 len2)) #f
              (if (string=? (substring str i (+ i len2)) substr)
                  i
                  (loop (+ i 1))))))))

;; -------------------------------
;; Data structures
;; -------------------------------

(define (make-truth f c) (cons f c))
(define truth-freq car)
(define truth-conf cdr)

(define default-truth (make-truth 1.0 0.9))

(define (make-var type index) (list 'var type index))
(define (make-inh subj pred) (list 'inh subj pred))
(define (make-sim subj pred) (list 'sim subj pred))
(define (make-inst subj pred) (list 'inst subj pred))
(define (make-prop subj pred) (list 'prop subj pred))
(define (make-instprop subj pred) (list 'instprop subj pred))
(define (make-conj terms) (cons 'conj terms))
(define (make-disj terms) (cons 'disj terms))
(define (make-neg term) (list 'neg term))
(define (make-prod terms) (cons 'prod terms))
(define (make-ext-set terms) (cons 'ext-set terms))
(define (make-int-set terms) (cons 'int-set terms))
(define (make-intersection terms) (cons 'intersection terms))
(define (make-union terms) (cons 'union terms))

(define (inh? x) (and (pair? x) (eq? (car x) 'inh)))
(define (sim? x) (and (pair? x) (eq? (car x) 'sim)))
(define (inst? x) (and (pair? x) (eq? (car x) 'inst)))
(define (prop? x) (and (pair? x) (eq? (car x) 'prop)))
(define (instprop? x) (and (pair? x) (eq? (car x) 'instprop)))
(define (conj? x) (and (pair? x) (eq? (car x) 'conj)))
(define (disj? x) (and (pair? x) (eq? (car x) 'disj)))
(define (neg? x) (and (pair? x) (eq? (car x) 'neg)))
(define (prod? x) (and (pair? x) (eq? (car x) 'prod)))
(define (ext-set? x) (and (pair? x) (eq? (car x) 'ext-set)))
(define (int-set? x) (and (pair? x) (eq? (car x) 'int-set)))
(define (intersection? x) (and (pair? x) (eq? (car x) 'intersection)))
(define (union? x) (and (pair? x) (eq? (car x) 'union)))
(define (var? x) (and (pair? x) (eq? (car x) 'var)))

;; -------------------------------
;; Parser
;; -------------------------------

(define k 1)

(define (parse-narsese str)
  (let* ((trimmed (string-trim str))
         (len (string-length trimmed))
         (last-char (string-ref trimmed (- len 1)))
         (type (cond ((char=? last-char #\.) 'belief)
                     ((char=? last-char #\?) 'question)
                     ((char=? last-char #\!) 'goal)
                     (else (error "Missing punctuation"))))
         (body (substring trimmed 0 (- len 1)))
         (truth (if (and (eq? type 'belief) (string-contains body "%"))
                    (parse-truth-value body)
                    #f))
         (clean-body (if truth
                         (let ((pos (string-contains body "%")))
                           (substring body 0 pos))
                         body)))
    (list type (parse-statement clean-body) truth)))

(define (parse-truth-value str)
  (let ((start (string-contains str "%")))
    (if start
        (let* ((rest (substring str (+ start 1)))
               (end (string-contains rest "%")))
          (if end
              (let ((inner (substring rest 0 end)))
                (let ((parts (string-split inner ";")))
                  (if (= (length parts) 2)
                      (make-truth (string->number (car parts))
                                  (string->number (cadr parts)))
                      (error "Invalid truth value"))))
              (error "Unmatched %")))
        #f)))

(define (parse-statement str)
  (let ((trimmed (string-trim str)))
    (cond ((string-prefix? trimmed "<") (parse-relation trimmed))
          ((string-prefix? trimmed "(") (parse-composite trimmed))
          ((string-prefix? trimmed "{") (parse-ext-set trimmed))
          ((string-prefix? trimmed "[") (parse-int-set trimmed))
          (else (string->symbol trimmed)))))

(define (parse-ext-set str)
  (let* ((inner (substring str 1 (- (string-length str) 1)))
         (parts (split-on-commas inner)))
    (make-ext-set (map parse-statement parts))))

(define (parse-int-set str)
  (let* ((inner (substring str 1 (- (string-length str) 1)))
         (parts (split-on-commas inner)))
    (make-int-set (map parse-statement parts))))

;; Improved parse-relation using a simple state machine
(define (parse-relation str)
  ;; str is like "<subj copula pred>"
  (define content (substring str 1 (- (string-length str) 1)))
  (define len (string-length content))
  (let loop ((i 0))
    (if (>= i len)
        (error "Invalid relation: no copula found")
        (let ((c (string-ref content i)))
          (cond
            ;; Inheritance: -->
            ((and (char=? c #\-) (< (+ i 2) len)
                  (char=? (string-ref content (+ i 1)) #\-)
                  (char=? (string-ref content (+ i 2)) #\>))
             (let ((subj-str (string-trim (substring content 0 i)))
                   (pred-str (string-trim (substring content (+ i 3)))))
               (make-inh (parse-statement subj-str) (parse-statement pred-str))))
            ;; Inheritance: -> (single hyphen, also allowed)
            ((and (char=? c #\-) (< (+ i 1) len)
                  (char=? (string-ref content (+ i 1)) #\>))
             (let ((subj-str (string-trim (substring content 0 i)))
                   (pred-str (string-trim (substring content (+ i 2)))))
               (make-inh (parse-statement subj-str) (parse-statement pred-str))))
            ;; Similarity: <->
            ((and (char=? c #\-) (< (+ i 2) len)
                  (char=? (string-ref content (+ i 1)) #\>)
                  (char=? (string-ref content i) #\-)) ; actually need to check previous? Better:
             ;; Actually similarity is <->, so the hyphen is at position i, next is >, but that's ambiguous.
             ;; We'll detect by looking for "<->" pattern where the character before hyphen is '<'? But we are inside content.
             ;; Simpler: search for "<->" as a substring.
             #| handled by separate detection |# (loop (+ i 1)))
            ;; Similarity: <-> (explicit detection)
            ((and (>= i 2) (char=? (string-ref content (- i 2)) #\<)
                  (char=? (string-ref content (- i 1)) #\-)
                  (char=? (string-ref content i) #\>))
             (let ((subj-str (string-trim (substring content 0 (- i 2))))
                   (pred-str (string-trim (substring content (+ i 1)))))
               (make-sim (parse-statement subj-str) (parse-statement pred-str))))
            ;; Instance: ◦→
            ((and (char=? c #\o) (< (+ i 1) len)
                  (char=? (string-ref content (+ i 1)) #\→))
             (let ((subj-str (string-trim (substring content 0 i)))
                   (pred-str (string-trim (substring content (+ i 2)))))
               (make-inst (parse-statement subj-str) (parse-statement pred-str))))
            ;; Property: →◦
            ((and (char=? c #\→) (< (+ i 1) len)
                  (char=? (string-ref content (+ i 1)) #\o))
             (let ((subj-str (string-trim (substring content 0 i)))
                   (pred-str (string-trim (substring content (+ i 2)))))
               (make-prop (parse-statement subj-str) (parse-statement pred-str))))
            ;; Instance-Property: ◦→◦
            ((and (char=? c #\o) (< (+ i 2) len)
                  (char=? (string-ref content (+ i 1)) #\→)
                  (char=? (string-ref content (+ i 2)) #\o))
             (let ((subj-str (string-trim (substring content 0 i)))
                   (pred-str (string-trim (substring content (+ i 3)))))
               (make-instprop (parse-statement subj-str) (parse-statement pred-str))))
            ;; Temporal implication =/> (simplified to inheritance)
            ((and (char=? c #\=) (< (+ i 2) len)
                  (char=? (string-ref content (+ i 1)) #\/)
                  (char=? (string-ref content (+ i 2)) #\>))
             (let ((subj-str (string-trim (substring content 0 i)))
                   (pred-str (string-trim (substring content (+ i 3)))))
               (make-inh (parse-statement subj-str) (parse-statement pred-str))))
            (else (loop (+ i 1))))))))

(define (parse-composite str)
  (let* ((content (substring str 1 (- (string-length str) 1)))
         (parts (split-on-commas content)))
    (let ((op (string->symbol (car parts)))
          (args (map parse-statement (cdr parts))))
      (cond ((eq? op '&&) (make-conj args))
            ((eq? op '||) (make-disj args))
            ((eq? op '--) (if (= (length args) 1) (make-neg (car args)) (error "-- needs one argument")))
            ((eq? op '&/) (make-conj args))
            ((eq? op '*) (make-prod args))
            ((eq? op '∩) (make-intersection args))
            ((eq? op '∪) (make-union args))
            (else (error "Unknown operator" op))))))

(define (split-on-commas str)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (depth 0) (result '()))
      (if (>= i len)
          (reverse (cons (string-trim (substring str start i)) result))
          (let ((c (string-ref str i)))
            (cond ((char=? c #\() (loop (+ i 1) start (+ depth 1) result))
                  ((char=? c #\)) (loop (+ i 1) start (- depth 1) result))
                  ((and (char=? c #\,) (= depth 0))
                   (loop (+ i 1) (+ i 1) depth
                         (cons (string-trim (substring str start i)) result)))
                  (else (loop (+ i 1) start depth result))))))))

;; -------------------------------
;; Knowledge base (mutable hash table)
;; -------------------------------

(define kb (make-hash))

;; Simple inference: deduction only
(define (apply-inference stmt1 truth1 stmt2 truth2)
  (define (deduce s1 s2)
    (cond ((and (inh? s1) (inh? s2))
           (let ((p (caddr s1))
                 (m (cadr s1))
                 (s (cadr s2))
                 (m2 (caddr s2)))
             (if (and (equal? m s) (not (equal? p m2)))
                 (list (make-inh s p) (deduction truth1 truth2))
                 #f)))
          (else #f)))
  (or (deduce stmt1 stmt2) (deduce stmt2 stmt1) '()))

(define (add-belief stmt truth)
  (hash-set! kb stmt truth)
  ;; forward inference: try to deduce new beliefs
  (for-each (lambda (other-stmt other-truth)
              (let ((result (apply-inference stmt truth other-stmt other-truth)))
                (when (pair? result)
                  (let ((new-stmt (car result))
                        (new-truth (cadr result)))
                    (unless (hash-has-key? kb new-stmt)
                      (hash-set! kb new-stmt new-truth)
                      (display "Derived: ") (display new-stmt) (display " ") (display new-truth) (newline))))))
            (hash-keys kb)
            (hash-values kb)))

(define (add-belief-old stmt truth)
  (hash-set! kb stmt truth))

(define (get-belief stmt)
  (hash-ref kb stmt #f))

(define (revise-belief stmt new-truth)
  (let ((old (get-belief stmt)))
    (if old
        (hash-set! kb stmt (revision old new-truth))
        (hash-set! kb stmt new-truth))))

;; -------------------------------
;; Inference functions
;; -------------------------------

(define (and-ext x y) (* x y))
(define (or-ext x y) (- 1 (* (- 1 x) (- 1 y))))

(define (revision t1 t2)
  (let ((f1 (truth-freq t1)) (c1 (truth-conf t1))
        (f2 (truth-freq t2)) (c2 (truth-conf t2)))
    (let ((denom (+ (* c1 (- 1 c2)) (* c2 (- 1 c1)))))
      (if (= denom 0)
          (make-truth (/ (+ f1 f2) 2) 0)
          (make-truth (/ (+ (* f1 c1 (- 1 c2)) (* f2 c2 (- 1 c1))) denom)
                      (/ denom (+ denom (* (- 1 c1) (- 1 c2)))))))))

(define (deduction t1 t2)
  (let ((f1 (truth-freq t1)) (c1 (truth-conf t1))
        (f2 (truth-freq t2)) (c2 (truth-conf t2)))
    (make-truth (and-ext f1 f2)
                (and-ext (and-ext f1 f2) (and-ext c1 c2)))))

(define (induction t1 t2)
  (let ((f1 (truth-freq t1)) (c1 (truth-conf t1))
        (f2 (truth-freq t2)) (c2 (truth-conf t2)))
    (let ((w+ (and-ext (and-ext f1 f2) (and-ext c1 c2))))
      (if (= w+ 0)
          (make-truth 0 0)
          (make-truth f1 (/ w+ (+ w+ k)))))))

(define (abduction t1 t2)
  (let ((f1 (truth-freq t1)) (c1 (truth-conf t1))
        (f2 (truth-freq t2)) (c2 (truth-conf t2)))
    (let ((w+ (and-ext (and-ext f1 f2) (and-ext c1 c2))))
      (if (= w+ 0)
          (make-truth 0 0)
          (make-truth f2 (/ w+ (+ w+ k)))))))

(define (comparison t1 t2)
  (let ((f1 (truth-freq t1)) (c1 (truth-conf t1))
        (f2 (truth-freq t2)) (c2 (truth-conf t2)))
    (let ((w+ (and-ext (and-ext f1 f2) (and-ext c1 c2)))
          (w (and-ext (or-ext f1 f2) (and-ext c1 c2))))
      (if (= w 0)
          (make-truth 0 0)
          (make-truth (/ w+ w) (/ w (+ w k)))))))

(define (conversion t)
  (let ((f (truth-freq t)) (c (truth-conf t)))
    (let ((w+ (and-ext f c)))
      (make-truth 1 (/ w+ (+ w+ k))))))

(define (negation t)
  (make-truth (- 1 (truth-freq t)) (truth-conf t)))

;; -------------------------------
;; Query with variables
;; -------------------------------

(define (unify pattern term bindings)
  (cond ((not bindings) #f)
        ((equal? pattern term) bindings)
        ((var? pattern)
         (let ((var pattern))
           (if (assoc var bindings)
               (unify (cdr (assoc var bindings)) term bindings)
               (cons (cons var term) bindings))))
        ((var? term) (unify term pattern bindings))
        ((and (pair? pattern) (pair? term) (eq? (car pattern) (car term)))
         (let loop ((ps (cdr pattern)) (ts (cdr term)) (b bindings))
           (if (and (null? ps) (null? ts))
               b
               (if (or (null? ps) (null? ts))
                   #f
                   (loop (cdr ps) (cdr ts) (unify (car ps) (car ts) b))))))
        (else #f)))

(define (match-query query)
  (let ((matches '()))
    (for-each (lambda (stmt truth)
                (let ((b (unify query stmt '())))
                  (when b (set! matches (cons (list b truth) matches)))))
              (hash-keys kb)
              (hash-values kb))
    matches))

;; -------------------------------
;; REPL and file loading
;; -------------------------------

(define (handle-input input)
  (with-handlers ([exn? (lambda (e)
                          (display "Error: ")
                          (display (exn-message e))
                          (newline))])
    (let ((parsed (parse-narsese input)))
      (case (car parsed)
        ((belief)
         (let ((stmt (cadr parsed)) (truth (or (caddr parsed) default-truth)))
           (add-belief stmt truth)
           (display "Belief stored.") (newline)))
        ((question)
         (let ((stmt (cadr parsed)))
           (let ((matches (match-query stmt)))
             (cond
               [(null? matches)
                (display "No answer.") (newline)]
               [else
                (display "Answers:") (newline)
                (for-each (lambda (m)
                            (display "  Bindings: ") (display (car m))
                            (display " Truth: ") (display (cadr m)) (newline))
                          matches)]))))
        ((goal)
         (display "Goal received (not yet implemented).") (newline))
        (else (error "Unknown sentence type"))))))

(define (load-file filename)
  (display (format "Loading ~a...\n" filename))
  (with-handlers ([exn? (lambda (e)
                          (display "File error: ")
                          (display (exn-message e))
                          (newline))])
    (let ((in (open-input-file filename)))
      (let loop ()
        (let ((line (read-line in)))
          (unless (eof-object? line)
            (when (not (string=? (string-trim line) ""))
              (display (format "> ~a\n" line))
              (handle-input line))
            (loop))))
      (close-input-port in))))

(define (repl)
  (display "Narsese Interpreter (NAL-1 to NAL-3)\n")
  (display "Enter statements ending with . ? or !\n")
  (let loop ()
    (display "> ")
    (flush-output)
    (let ((input (read-line)))
      (cond
        [(eof-object? input)
         (display "Goodbye.") (newline)]
        [else
         (when (not (string=? input ""))
           (handle-input input))
         (loop)]))))

;; -------------------------------
;; Main: check command-line arguments
;; -------------------------------

(define (main)
  (let ((args (current-command-line-arguments)))
    (if (= (vector-length args) 0)
        (repl)
        (begin
          (load-file (vector-ref args 0))
          (repl)))))

;; Run main
(main)
