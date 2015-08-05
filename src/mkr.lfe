(defmodule mkr
  (export (conj 2)
	  (disj 2)
	  (= 2)
	  (call/fresh 1))

  (export  ;; for mkr-user
   (walk 2)
   (var 1)
   (var? 1)
   (pair? 1)))

(include-lib "mkr/include/mkr-bool.lfe")

(define (call/fresh f)
  (lambda(s/c)
    (let [(c (cdr s/c))]
      (funcall (funcall f (var c)) `(,(car s/c) . ,(+ 1 c))))))

(define (= u v)
  (lambda(s/c)
    (let [(s (unify u v (car s/c)))]
      (iff s (unit `(,s . ,(cdr s/c))) (mzero)))))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (var c) (tuple c))
(define (var? c) (is_tuple c))
(define (var=? x1 x2) (andd (is_tuple x1) (is_tuple x2) 
			    (== (element 1 x1) (element 1 x2))))

(define (unit s/c) (cons s/c (mzero)))
(define (mzero) '())

(define (bind s/c g)
  (cond ((null? s/c) (mzero))
	((is_function s/c) (lambda() (bind (funcall s/c) g)))
	(else (mplus (funcall g (car s/c)) 
		     (bind (cdr s/c) g)))))

(define (mplus s1 s2)
  (cond ((null? s1) s2)
	((is_function s1) (lambda() (mplus s2 (funcall s1))))
	(else (cons (car s1) (mplus (cdr s1) s2)))))

(define (disj g1 g2)
  (lambda(s/c)
    (mplus (funcall g1 s/c) (funcall g2 s/c))))

(define (conj g1 g2)
  (lambda(s/c)
    (bind (funcall g1 s/c) g2)))


(define (unify u0 v0 s)
  (let [(u (walk u0 s)) (v (walk v0 s))]
    (cond
     ((andd (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((andd (pair? u) (pair? v))
      (let [(s (unify (car u) (car v) s))]
	(andd s (unify (cdr u) (cdr v) s))))
     (else (andd (=:= u v) s)))))

(define (walk u s)
  (let [(pr (andd (var? u) (assp (lambda(v) (var=? v u)) s)))]
    (iff pr (walk (cdr pr) s) u)))

;; Schemey compatibility juice

(define (assp p l)
  (cond
   ((null? l) 'false)
   ((pair? l) 
    (let [(a (car l))]
      (iff (pair? a)
	   (if (funcall p (car a)) a (assp p (cdr l)))
	   (assp p (cdr l)))))))

(define (pair? p) 
  (cond
   ((?= (cons a d) p) 'true)
   (else 'false)))
(define (null? x) (== '() x))
