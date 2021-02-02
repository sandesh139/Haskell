;;Sandesh Timilsina
;;Homework 1.
;;Exercise 1.2
;;a. 10500900
;;b. 2.5e-007
;;c. ¡¥big-number
;;d. ¡¥cat
;;e. ¡¥cheshire
;;f. 1050900
;;g. ¡¥big-number
;;h. ¡¥number1


;;Exercise: 1.3
;;a. (- 10 (- 8 (- 6 4 )))            =   4
;;b. (/ 40 (* 5 20))                  =   2/5
;;c. (/ 2 3)                                =   2/3 
;;d. (+ (* 0.1 20)  (/ 4 -3))      =   0.6666666666666667

;;Exercise: 1.4
;;a.
(- (* 4 7) (+ 13 5))
;;b.
(* 3 (+ 4 (- -5 -3)))
;;c.
(/ 2.5 (* 5 (/ 1 10)))
;;d.
(* 5 (+ (* 537 (+ 98.3 (- 375 (* 2.5 153)))) 255))

;;Exercise: 1.5
;;Let alpha be ¡¥a¡¦ and beta be ¡¥b¡¦ and gamma be ¡¥r¡¦
;;a. a + ((b +r ) -a))
;;b. (a*b) + (r*b)
;;c. (a ¡V b) / (a - r)
;;Exercise 1.6
;;a.  
(cons 'one (cons 'two (cons 'three (cons 'four '()))))

;;b.
(cons 'one (cons (cons 'two (cons 'three (cons 'four '()))) '()))

;;c.
(cons 'one (cons (cons 'two (cons 'three '())) (cons 'four '())))

;;d.
(cons (cons 'one (cons 'two '())) (cons (cons 'three (cons 'four '())) '()))

;;e.
(cons (cons (cons 'one '()) '()) '())


;;Exercise 1.10
;;a.    #f   Because cons returns a list, and list is not a symbol
;;b.    #t   Because cons returns a list
;;c.     #f  Because cons add alpha in the list which beta belongs to. Then, list is not empty.
;;d.     #t Because cdr removes the first element and return the empty list


;;Exercise 1.14
;;a. #t  Because car returns ¡¥cat
;;b. #f Because cdr always return list 
;;c. #f Because cdr always return list 
;;d. #t Because we have element cat and mouse inside the list
;;e. #f Because car  returns the symbol cheshire
;;f. #t Because we have the empty list inside the list.

;;Exercise 2.1
(define second
     (lambda (list)
        (car (cdr list))))

;;Exercise 2.3 
;;a.   ¡¥(1 2)
;;b.    ¡¥((a b) (e f))


;;Exercise 2.4
  (define juggle
      (lambda (list)
          (cons (car (cdr (cdr list))) (cons (car list) (cons (car (cdr list)) '())))))

;;Exercise 2.6
;; a.  #t
;; b.  #t
;; c.  #t
;; d. #f

;;Exercise 2.7
;; a.  #t
;; b.  #f
;; c.  #t
;; d.  #f

;;Exercise 2.10
;;Last-item procedure is given as below:
(define last-item
    (lambda (ls)
        (if (null? (cdr ls))
             (car ls)
              (last-item (cdr ls)))))

;;member? Procedure is given as below:
(define member?
       (lambda (item ls)
            (if (null? ls)
                 #f
                 (or (equal? (car ls) item) (member? item (cdr ls))))))

;;Remove-1st  procedure  is given as below:
(define remove-1st
      (lambda (item ls)
           (if (null? ls)
               '()
                (if(equal? (car ls) item)
                    (cdr ls)
                    (cons (car ls) (remove-1st item (cdr ls)))))))


;;Exercise 2.12:
;;(mystery ¡¥(1 2 3 4 5)) gives ¡¥(1 2 3 4)
;;  Here the first element of the list is removed and list is tested of it empty or not. If the list is after                    ;;  removing first element then, it just sets that list to empty. So, for example given 1 2 3 4 are in the list in ;;  the output and while call mystery with list just containing 5 , it is set to empty and just the list  with 1 2 ;;  3 4 is given in output.
;;  A good name for the procedure is remove-last.

;;Exercise 2.13:
;; FIRST PROCEDURE:
(define subst-1st
    (lambda (new old ls)
       (if (null? ls)
	   '()
	   (if (equal? (car ls) old)
	       (cons new (cdr ls))
	       (cons (car ls) (subst-1st new old (cdr ls)))))))

;; SECOND PROCEDURE:
(define substq-1st
    (lambda (new old ls)
       (if (null? ls)
	   '()
	   (if (eq? (car ls) old)
	       (cons new (cdr ls))
	       (cons (car ls) (subst-1st new old (cdr ls)))))))


;;  Third procedure:
(define substq-1st
    (lambda (new old ls)
       (if (null? ls)
	   '()
	   (if (eqv? (car ls) old)
	       (cons new (cdr ls))
	       (cons (car ls) (subst-1st new old (cdr ls)))))))


;;  Exercise 2.14:
(define insert-left-lst
(lambda (new old Is)
(cond
((null? Is) '())
((equal? (car Is) old)
(cons new (cons old (cdr Is)) ))
(else (cons (car Is)
(insert-left-lst new old (cdr Is)))))))


;;  Exercise 2.15:
(define list-of-first-items
    (lambda (ls)
      (if (null? ls)
          '()
	  (cons (car (car ls)) (list-of-first-items (cdr ls))))))

;;  Exercise 2.16:
(define replace
    (lambda (new-item ls)
    (if (null? ls)
	'()
	(cons new-item (replace new-item (cdr ls))))))

;;  Exercise 2.18
;;  Here we define remove-first, which removes the first matched item of the list and we define reverse-  ;;  first which reverse the list. So, algorithm is reversing first and removing the first matched item and           ;;   reversing again.
(define remove-1st
      (lambda (item ls)
           (if (null? ls)
               '()
                (if(equal? (car ls) item)
                    (cdr ls)
                    (cons (car ls) (remove-1st item (cdr ls)))))))
(define reverse-first
    (lambda (ls)
    (if (null? ls)
	'()
	(append (reverse-first (cdr ls)) (cons (car ls) '()))))) 
(define remove-last
   (lambda (item ls)
     (reverse (remove-1st item (reverse ls)))))

