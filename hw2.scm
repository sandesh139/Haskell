;; Name: Timilsina, Sandesh
;; Net ID: stimilsina@unm.edu

;; problem1-4.4    p129
;; Points: 1/15
#| here we deep recursion until we get the last element and add it to the empty list and to the new list. |#
(define deepen-1
  (lambda (ls)
    (if (pair? ls)
	(if (null? (cdr ls))
	    (cons ls '())
	    (cons (cons (car ls) '()) (deepen-1 (cdr ls))))
	'())))



;; problem1-4.6    p136
;; Points: 1/15
#|here we add new element in left of old element. We check if the element in the ls is old
element or not. If it is old element we add the new element to it. If the element is not empty list we do deep recursion for list inside the list. |#
(define insert-left-all
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old) ( cons new (cons old (insert-left-all new old (cdr ls)))))
     ((pair? (car ls))
      (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
     (else (cons (car ls) (insert-left-all new old (cdr ls)))))))



;; problem1-4.10    p143
;; Points: 1/15
|# we again perform deep recursion in this problem. We return first  element of ls if it is
not a list. If the firs element is pair we perform the recursion, by providing first element as the input to the leftmost function.|#
(define (leftmost ls)
  (cond
   ((null? ls)
    '())
   ((pair? (car ls))
    (leftmost (car ls)))
   (else
    (car ls))))



;; problem1-4.11    p143
;; Points: 1/15
(define (rightmost ls)
  (cond
   ((null? ls)
    '())
   ((and (pair? (car ls)) (null? (cdr ls))) (rightmost (car ls)))
   ((null? (cdr ls)) (car ls))
   (else
    (rightmost (cdr ls)))))



;; problem1-4.18    p156
;; Points: 1/15
(define length-it
  (lambda (ls)
    (addup ls 0)))

(define addup
  (lambda (ls x)
    (if (null? ls )
        x
        (addup (cdr ls) (+ 1 x)))))


;; problem1-4.19    p156
;; Points: 1/15
(define mk-asc-list-of-ints
  (lambda (number)
    (get-int-ascending number '())))

(define get-int-ascending
  (lambda (number ls)
    (if(equal? number 0)
       ls
       (get-int-ascending (- number 1) (cons number ls)))))



;; Points: 1/15
(define mk-desc-list-of-ints
  (lambda (number)
    (get-int-descending 1 number '())))

(define get-int-descending
  (lambda (n number ls)
    (if (< number n)
        ls
	(get-int-descending (+ n 1) number (cons n ls)))))


;; problem1-4.20    p156
;; Points: 1/15
(define occurs
  (lambda (item ls)
    (cond
      ((null? ls) 0)
      ((equal? item (car ls))
       (+ 1 (occurs item (cdr ls))))
      (else
      (occurs item (cdr ls))))))



;; Points: 1/15
(define occurs-it
  (lambda (item ls)
    (occurs-helper item 0 ls)))

(define occurs-helper
  (lambda (item number ls)
    (cond
     ((null? ls) number)
    ((equal? item (car ls)) (occurs-helper item (+ number 1) (cdr ls)))
    (else (occurs-helper item number (cdr ls))))))



;; problem2-
;; Points: 1/15
(define calculator
  (lambda (ls)
    (if (pair? ls)
        (calc-number ls)
        ls)))

(define calc-number
  (lambda (ls)
    (if (equal? '+ (car (cdr ls)))
        (cond ((and (number? (car ls)) (number? (car (cdr (cdr ls)))))
	       (+ (car ls) (car (cdr (cdr ls)))))
              ((and (number? (car ls)) (not (number? (car (cdr (cdr ls))))))
               (+ (car ls) (calc-number (car (cdr (cdr ls))))))
              ((and (not (number? (car ls))) (number? (car (cdr (cdr ls)))))
               (+ (calc-number (car ls)) (car (cdr (cdr ls)))))
              ((and (not (number? (car ls))) (not (number? (car (cdr (cdr ls))))))
               (+ (calc-number (car ls)) (calc-number (car (cdr (cdr ls))))))
              )
        (if (equal? '- (car (cdr ls)))
            (cond ((and (number? (car ls)) (number? (car (cdr (cdr ls)))))
                   (- (car ls) (car (cdr (cdr ls)))))
                  ((and (number? (car ls)) (not (number? (car (cdr (cdr ls))))))
                   (- (car ls) (calc-number (car (cdr (cdr ls))))))
                  ((and (not (number? (car ls))) (number? (car (cdr (cdr ls)))))
                   (- (calc-number (car ls)) (car (cdr (cdr ls)))))
                  ((and (not (number? (car ls))) (not (number? (car (cdr (cdr ls))))))
                   (- (calc-number (car ls)) (calc-number (car (cdr (cdr ls))))))
		  )
	    (if (equal? '* (car (cdr ls)))
		(cond ((and (number? (car ls)) (number? (car (cdr (cdr ls)))))
                       (* (car ls) (car (cdr (cdr ls)))))
                      ((and (number? (car ls)) (not (number? (car (cdr (cdr ls))))))
                       (* (car ls) (calc-number (car (cdr (cdr ls))))))
                      ((and (not (number? (car ls))) (number? (car (cdr (cdr ls)))))
                       (* (calc-number (car ls)) (car (cdr (cdr ls)))))
                      ((and (not (number? (car ls))) (not (number? (car (cdr (cdr ls))))))
                       (* (calc-number (car ls)) (calc-number (car (cdr (cdr ls))))))
                      )
		(cond ((and (number? (car ls)) (number? (car (cdr (cdr ls)))))
		       (/ (car ls) (car (cdr (cdr ls)))))
		      ((and (number? (car ls)) (not (number? (car (cdr (cdr ls))))))
		       (/ (car ls) (calc-number (car (cdr (cdr ls))))))
		      ((and (not (number? (car ls))) (number? (car (cdr (cdr ls)))))
		       (/ (calc-number (car ls)) (car (cdr (cdr ls)))))
		      ((and (not (number? (car ls))) (not (number? (car (cdr (cdr ls))))))
		       (/ (calc-number (car ls)) (calc-number (car (cdr (cdr ls))))))
		      ))))))




;; problem3-
;; Points: 1/15
(define infix->prefix
  (lambda (ls)
    (if (pair? ls)
        (infix-helper ls)
        ls)))

(define infix-helper
  (lambda (ls)
    (cond ((and (number? (car ls)) (number? (car (cdr (cdr ls)))))
	   (cons (car (cdr ls)) (cons (car ls) (cons (car (cdr (cdr ls))) '()))))
	  ((and (number? (car ls)) (not (number? (car (cdr (cdr ls))))))
	   (cons (car (cdr ls)) (cons (car ls) (cons (infix-helper (car (cdr (cdr ls)))) '()))))
	  ((and (not (number? (car ls))) (number? (car (cdr (cdr ls)))))
	   (cons (car (cdr ls)) (cons (infix-helper (car ls)) (cons (car (cdr (cdr ls))) '()))))
	  ((and (not (number? (car ls))) (not (number? (car (cdr (cdr ls))))))
	   (cons (car (cdr ls)) (cons (infix-helper (car ls)) (cons (infix-helper (car (cdr (cdr ls)))) '())))))))


;; problem4-
    ;; NOTE: All helper functions should be tail-recursive and should be defined within the body of iota-iota using letrec.
;; Points: 1/15
(define iota-iota
  (lambda (n)
    (letrec
        ((helper
          (lambda (x y ls)
            (cond
              ((> x n)
               (reverse ls))
              ((<= y n)
               (helper x (+ 1 y) (cons (cons x y ) ls)))
              ((> y n)
               (helper (+ 1 x) 1  ls))))))
      (helper 1 1 '()))))

;; problem5-
    ;; NOTE: Any helper functions you need should be defined within the body of digits->number using letrec.
;; Points: 1/15
(define digits->number
  (lambda (ls)
    (letrec
        ((helper
          (lambda (len ls counter sum)
            (cond
	     ((<= counter len)
	      (helper len (cdr ls) (+ counter 1) (+ sum (* (car ls) (expt 10 (- len counter))))))
	     (else sum)))))
      (helper (- (length ls) 1) ls 0 0))))



;; problem6-
;; Points: 1/15
(define cond->if
  (lambda (ls)
    (helper-cond (cdr (reverse (cdr ls))) '() 0 (cadar (reverse ls)))))

(define helper-cond
  (lambda (ls new counter getelse)
    (if (pair? ls)
        (if (pair? (caar ls))
            (if (pair? (cdr ls))
                (if (eq? counter 0)
                    (helper-cond (cdr ls) (cons (cons 'if (cons (caar ls) (cons (cadar ls)
										(cons getelse new)))) '()) (+ counter 1) getelse)
		    (helper-cond (cdr ls) (cons (cons 'if (cons (caar ls) (cons (cadar ls) new))) '()) counter getelse))
                (helper-cond (cdr ls) (cons 'if (cons (caar ls) (cons (cadar ls) new))) counter getelse))
            new)
        new)))




;; problem7-
    ;; NOTE: Do not use or define fact or expt, any helper functions you need should be defined within the body of cos using letrec
;; Points: 1/15
(define cos
  (lambda (x)
    (letrec
        ((helper
          (lambda (sum denominator numerator counter1 counter2)
            (cond
	     ((>= counter1 100)
	      sum)
	     ((eq? (modulo counter1 2) 1)
	      (helper (- sum (/ numerator denominator)) (* denominator (* counter2 (+ 1 counter2)))
		      (* numerator (* x x)) (+ 1 counter1) (+ 2 counter2)))
	     ((eq? (modulo counter1 2) 0)
	      (helper (+ sum (/ numerator denominator)) (* denominator (* counter2 (+ 1 counter2)))
		      (* numerator (* x x)) (+ 1 counter1) (+ 2 counter2)))))))
      (helper 0 1 1 0 1))))
          


