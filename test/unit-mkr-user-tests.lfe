(defmodule unit-mkr-user-tests
    (behaviour ltest-unit)
  (export all)
  (import
   (from mkr
	 (empty-state 0)
	 (call/fresh 1)
	 (conj 2)
	 (disj 2)
	 (equalo 2))
   (from mkr-user
	 (take-all 1)
	 (take 2))
   (from ltest
	 (check-failed-assert 2)
	 (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "mkr/include/mkr-user.lfe")

(define (fives x) (disj (equalo 5 x) (zzz (fives x))))
(deftest call/fresh-recursive-goal
  (let [(res (funcall (call/fresh (lambda(q) (fives q))) (empty-state)))]
    (is-equal '(((#(0) . 5)). 1) (car res))
    (is (is_function (cdr res)))))

(define (one-two-three q)
  (disj (equalo q 1)
	(disj (equalo q 2)
	      (equalo q 3))))

(deftest take-n
  (is-equal '((((#(0) . 1)) . 1)
	      (((#(0) . 2)) . 1)
	      (((#(0) . 3)) . 1))
	    (take 3
		  (funcall (call/fresh (lambda(q) (one-two-three q))) (empty-state)))))

(deftest take-n-with-shorter-stream
   (is-equal '((((#(0) . 1)) . 1))
 	    (take 3
 		  (funcall (call/fresh (lambda(q) (conj (one-two-three q) (equalo 1 q)))) (empty-state)))))

(deftest take-all
  (is-equal '((((#(0) . 1)) . 1)
	      (((#(0) . 2)) . 1)
	      (((#(0) . 3)) . 1))
	    (take-all
		  (funcall (call/fresh (lambda(q) (one-two-three q))) (empty-state)))))


(define (fives+ x) (disj+ (equalo 5 x) (fives+ x)))
(define (sixes+ x) (disj+ (equalo 6 x) (sixes+ x)))
(define (fives-and-sixes+ x)
  (disj+ (conj+ (fives+ x) (equalo x 5))
	 (sixes+ x)))

(deftest disj+-snoozes-cdr
  (let ((res (funcall (funcall 
		       (funcall (call/fresh (lambda(q) (fives+ q))) (empty-state))))))
    (is-equal '(((#(0) . 5)). 1) (car res))
    (is (is_function (cdr res)))))

(deftest conj+-snoozes-and-interleaves
    (is-equal '((((#(0) . 6)) . 1)
		(((#(0) . 6)) . 1)
		(((#(0) . 5)) . 1))
  	    (take 3
  		  (funcall (call/fresh (lambda(q) (fives-and-sixes+ q))) (empty-state)))))

;; (deftest conj+-snoozes-cdr
;;   (let ((res (funcall (funcall (funcall (funcall (funcall 
;; 						  (funcall (call/fresh (lambda(q) (fives-and-sixes+ q))) (empty-state)))))))))
;;     (is-equal '(((#(0) . 5)). 1) (car res))
;;     (is (is_function (cdr res)))))
