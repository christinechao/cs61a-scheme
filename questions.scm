; Some utility functions that you may find useful.
(define (apply-to-all proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (apply-to-all proc (cdr items)))))

(define (keep-if predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (keep-if predicate (cdr sequence))))
        (else (keep-if predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Problem 18
;; Turns a list of pairs into a pair of lists
(define (zip pairs)
  (define firsts (apply-to-all (lambda (pair) (car pair)) pairs))
  (define seconds (apply-to-all (lambda (pair) (cadr pair)) pairs))
  (list firsts seconds)
)

(zip '())
; expect (() ())
(zip '((1 2)))
; expect ((1) (2))
(zip '((1 2) (3 4) (5 6)))
; expect ((1 3 5) (2 4 6))

; Problem 19

;; A list of all ways to partition TOTAL, where  each partition must
;; be at most MAX-VALUE and there are at most MAX-PIECES partitions.
(define (list-partitions total max-pieces max-value)
    (define (helper t mp mv sofar partitions)
      ;(display (list t mp mv sofar partitions))
      ;(newline)
      (if (and (= t 0) (not (null? sofar)))
        (define partitions (append partitions (list sofar))) nil
      )
      (if (or (<= mp 0) (<= mv 0) (<= t 0))
        partitions
        (begin
          (define (looper i upper old_parts partitions)
            ;(display (list looper i upper old_parts partitions))
            ;(newline)
            (if (>= i upper)
              partitions
              (begin
                (define new_parts (helper (- t i) (- mp 1) i (append sofar (list i)) old_parts))
                (define new_parts (keep-if (lambda (e) (not (null? e))) new_parts))
                (define partitions (append partitions new_parts))
                (looper (+ i 1) upper old_parts partitions)
              )
            )
          )
          (looper 1 (+ mv 1) partitions partitions)
        )
     )
   )
   (helper total max-pieces max-value (list) (list))
)
;; perform (op i) for each i in Python range(lower, upper)
;; then result some result at the end
(define (for-in-range op lower upper)
  (if (< lower upper)
    (begin
      (op lower)
      (for-in-range op (+ lower 1) upper)
    )
    nil
  )
)


(list-partitions 5 2 4)
; expects a permutation of ((4 1) (3 2))
(list-partitions 7 3 5)
; expects a permutation of ((5 2) (5 1 1) (4 3) (4 2 1) (3 3 1) (3 2 2))


; Problem 20
;; Returns a function that takes in an expression and checks if it is the special
;; form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

(define (analyze_body body)
  (if (null? body)
    nil
    (cons (analyze (car body)) (analyze_body (cdr body)))))
    
;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         ;(cdr expr)
         expr
         )
        ((or (lambda? expr) (define? expr))
         (define op (car expr))
         (define formals (cadr expr))
         (define body (apply-to-all analyze (cddr expr)))
         (append (list op formals) body)
        )
        ((let? expr)
         (let ((bindings (cadr expr))
               (body   (apply-to-all analyze (cddr expr))))

           (define zipped (zip bindings))
           (define parameters (car zipped))
           (define param_values (apply-to-all analyze (cadr zipped)))
           (define fn (append (list 'lambda parameters) body))
           (cons fn param_values)
           ))
        (else
          (apply-to-all analyze expr)
         )))

(analyze 1)
; expect 1
(analyze 'a)
; expect a
(analyze '(+ 1 2))
; expect (+ 1 2)

;; Quoted expressions remain the same
(analyze '(quote (let ((a 1) (b 2)) (+ a b))))
; expect (quote (let ((a 1) (b 2)) (+ a b)))

;; Lambda parameters not affected, but body affected
(analyze '(lambda (let a b) (+ let a b)))
; expect (lambda (let a b) (+ let a b))
(analyze '(lambda (x) a (let ((a x)) a)))
; expect (lambda (x) a ((lambda (a) a) x))

(analyze '(let ((a 1)
                (b 2))
            (+ a b)))
; expect ((lambda (a b) (+ a b)) 1 2)
(analyze '(let ((a (let ((a 2)) a))
                (b 2))
            (+ a b)))
; expect ((lambda (a b) (+ a b)) ((lambda (a) a) 2) 2)
(analyze '(let ((a 1))
            (let ((b a))
              b)))
; expect ((lambda (a) ((lambda (b) b) a)) 1)


;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  'YOUR-CODE-HERE
  nil)




