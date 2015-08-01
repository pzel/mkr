(defmodule mkr-user
  (export 
   (call/empty-state 1)
   (empty-state 0)
   (mK-reify 1)
   (take 2)
   (take-all 1)
   ))

(include-lib "mkr/include/mkr-bool.lfe")
(include-lib "mkr/include/mkr-user.lfe")

(define (empty-state) '(() . 0))

(define (call/empty-state g)
  (funcall g (empty-state)))

(define (mK-reify s/c)
  (: lists map (lambda(s) (reify-state/1st-var s)) s/c))

(define (reify-state/1st-var s/c)
  (let ((v (walk* (: mkr var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (: mkr walk v s)))
    (cond
     ((: mkr var? v)
      (let ((n (reify-name (length s))))
        (cons `(,v . ,n) s)))
     ((: mkr pair? v)
      (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-name n)
  (list_to_atom (++ "_." (integer_to_list n))))

(define (walk* v s)
  (let ((v (: mkr walk v s)))
    (cond
     ((: mkr var? v) v)
     ((: mkr pair? v)
      (cons (walk* (car v) s)
            (walk* (cdr v) s)))
     (else v))))

(define (pull st)
  (iff (is_function st) (pull (funcall st)) st))

(define (take n st)
  (iff (== 0 n) '()
       (let [(st (pull st))]
	 (cond ((== '() st) '())
	       (else (cons (car st) (take (- n 1) (cdr st))))))))

(define (take-all st)
  (let [(next (pull st))]
    (if (== '() next) 
	'()
	(cons (car next) (take-all (cdr next))))))
 
