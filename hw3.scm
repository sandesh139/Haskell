;; Name: Timilsina, Sandesh
;; Net ID: stimilsina@unm.edu

;; === Part 1, Points: 10, Weight: 1/3 ===
(define compose
    (lambda (f g)
        (lambda (x)
            (f (g x))
        )
    )
)


;; problem1-1-7.2    p234
;; Points: 1/10
;;((compose3 (lambda (x) (* 5 x)) add1 add1) 10)
(define compose3
  (lambda (f g h)
    (lambda (x)
      ((compose f g) (h x)))))


;; problem1-1-7.3    p234
;; Points: 1/10
(define compose-many
  (lambda args
    (lambda (result)
      (letrec ((helper
		(lambda (ls result)
		  (if(null? ls)
		     result
		     (helper (cdr ls) ((car ls) result))))))
        (helper (reverse args) result)))))



;; problem1-1-7.6    p235
;; Points: 1/10
(define map-first-two
  (lambda (func ls)
    (if (null? ls)
	'()
	(if (null? (cdr ls))
	    '()
	    (cons (func (car ls) (cadr ls)) (map-first-two func (cdr ls)))))))

;; problem1-1-7.7    p235
;; Points: 1/10
(define reduce
  (lambda (func ls)
    (if (null? (cddr ls))
	(func (car ls) (cadr ls))
	(func (car ls) (reduce func (cdr ls))))))


;; problem1-1-7.8    p236
;; Points: 1/10
(define andmap
  (lambda (pred ls)
    (if (or (null? (cdr ls)) (not (pred (car ls))))
	(pred (car ls))
	(andmap pred (cdr ls)))))


;; problem1-1-7.12    p243
;; Points: 1/10
(define curried*
  (lambda (m)
    (lambda (n)
      (* m n))))

(define times10
  (curried* 10))

;; problem1-1-7.18    p244
;; Points: 1/10
(define between?
  (lambda (x y z)
    (if (and (> y x) (> z y))
	#t
	#f)))
(define between?-c
  (lambda (x)
    (lambda (y)
      (lambda (z)
	(if (and (> y x) (> z y))
	    #t
	    #f)))))

;; problem1-1-7.22    p250
;; Points: 1/10

(define mult-by-scalar
  (lambda (x)
    (flat-recur '() (lambda (a b) (cons (* x a) b)))))

(define flat-recur
  (lambda (seed list-proc)
    (letrec
	( (helper
	   (lambda (ls)
	     (if (null? ls)
		 seed
		 (list-proc (car ls) (helper (cdr ls)))))))
      helper)))




;; problem1-1-7.30    
;; Points: 1/10
;;((reverse-all) '(1 2 3))
;;((reverse-all) '(1 (1 2) (5 6 7) 3))
(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
	(
	 (helper
	  (lambda (ls)
	    (if (null? ls)
		seed
		(let ((a (car ls)))
		  (if (or (pair? a) (null? a))
		      (list-proc (helper a) (helper (cdr ls)))
		      (item-proc a (helper (cdr ls)))
		      )
		  )
		)
	    )
	  )
	 )
      helper
      )
    )
  )


(define reverse-all
  (lambda ls
    (deep-recur '()  (lambda (a b) (append b (list a))) (lambda (a b) (append b (list a))))))



;; problem1-1-7.31    p
;; Points: 1/10

;;not possible to do it.
(define flat-recur
  
)


;; === Part 2, Points: 10, Weight: 1/3 ===

;; problem2-1-a
;; Points: 1/8
(define tail-recur
  (lambda (bpred xproc aproc acc0)
    (lambda (x)
      (letrec ((helper
		(lambda (x acc)
		  (if (bpred x)
		      acc
		      (helper (xproc x) (aproc x acc))))))
	(helper x acc0)))))

;; problem2-1-b
;; Points: 1/8
(define reverse
  (lambda (ls)
    ((tail-recur
      null?
      cdr
      (lambda (ls result)
	(cons (car ls) result))
      '()) ls)))

;; problem2-1-c
;; Points: 1/8
(define iota
  (lambda (x)
    ((tail-recur
      zero?
      sub1
      cons
      '()) x)))

;; problem2-2-
;; Points: 1/8
(define disjunction2
  (lambda (pred1 pred2)
    (lambda (x)
      (or (pred1 x) (pred2 x)))))

;; problem2-3-
;; Points: 1/8
(define disjunction
  (lambda args
    (lambda (x)
      (letrec ((helper
		(lambda (ls)
		  (if (or (null? (cdr ls)) ((car ls) x))
		      ((car ls) x)
		      (helper (cdr ls))))))
	(helper args)))))



;; problem2-4-
;; Points: 1/8
(define matrix-map
  (lambda (f A)
    (let ((row-operate
           (lambda (row)
             (map f row))))
      (map row-operate A))))

;; problem2-5
(define fold
  (lambda (seed proc)
    (letrec
	(
	 (pattern
	  (lambda (ls)
	    (if (null? ls)
		seed
		(proc (car ls) (pattern (cdr ls)))
		)
	    )
	  )
	 )
      pattern
      )
    )
  )


;; problem2-5-a
;; Points: 1/8
(define delete-duplicates
  (lambda (input)
    ((fold
      '()
      (lambda (element ls)
       (if (member element ls)
           ls
           (cons element ls)))) input)))

;; problem2-5-b
;; Points: 1/8
(define assoc
  (lambda (item ls)
    ((fold
      #f
      (lambda (element ls)
        (if (member item element)
            element
            ls))) ls)))


;; === Part 3, Points: 8, Weight: 1/3 ===

;; problem3-1-
;; Points: 1/8
(define length
  (lambda (ls)
    (apply + (map (lambda (x) 1) ls))))

;; problem3-2-
;; Points: 1/8
(define sum-of-squares
  (lambda (ls)
    (apply + (map (lambda (x) (* x x)) ls))))

;; problem3-3-
;; Points: 1/8
(define avg
  (lambda (ls)
    (apply + (map (lambda (x) (/ x (length ls))) ls))))

;; problem3-4-
;; Points: 1/8
(define avg-odd
  (lambda (ls)
    (average (filter odd? ls))))



;; problem3-5-
;; Points: 1/8
;;(shortest '((1 2) (2 3) (3 4 5))) => '(1 2)
;;(shortest '()) => '()
(define shortest
  (lambda (ls)
    (if (or (null? ls) (= 0 (apply min (map length ls))))
        '()
        (car (filter (lambda (x) ( = (length x) (apply min (map length ls)))) ls)))))



;; problem3-6-
;; Points: 1/8
(define avg-fact
  (lambda (ls)
    (avg (map (lambda (x) (apply * (iota x))) ls))))



;; problem3-7-
;; Points: 1/8
(define tally
  (lambda (predicate ls)
    (length (filter predicate ls))))

;; problem3-8-
;; Points: 1/8


(define iota
    (lambda (x)
        (letrec
            ((loop
                (lambda (x acc)
                    (if (= x 0)
                         acc
                        (loop (sub1 x) (cons x acc))))))
            (loop x '()))))
(define select
  (lambda (pred)
    (lambda (ls0 ls1)
      (map cdr
           (filter
            (lambda (x) (pred (car x)))
            (map cons ls0 ls1))))))


(define list-ref
  (lambda (ls n)
    ((select (lambda (x) (= x n)))
     (iota (length ls))
     ls)))



