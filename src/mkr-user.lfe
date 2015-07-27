(defmodule mkr-user
  (export (take 2)
	  (take-all 1)
	  ))

(include-lib "mkr/include/mkr-bool.lfe")
(include-lib "mkr/include/mkr-user.lfe")

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
  
	  
