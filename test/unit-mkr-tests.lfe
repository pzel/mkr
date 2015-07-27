(defmodule unit-mkr-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from mkr
	  (empty-state 0)
	  (call/fresh 1)
	  (conj 2)
	  (disj 2)
	  (equalo 2))
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest empty-state-is-empty
  (is-equal '(() . 0) (empty-state)))

(deftest call/fresh-simple-goal
  (is-equal '((((#(0) . 5)). 1))
	    (funcall (call/fresh (lambda(q) (equalo q 5))) (empty-state))))

(deftest call/fresh-complex-goal
  (flet ((a-and-b ()
	  (conj (call/fresh (lambda(a) (equalo a 7)))
		(call/fresh (lambda(b) (disj (equalo b 5)
					     (equalo b 6)))))))
	(is-equal '((((#(1) . 5) (#(0) . 7)) . 2)
		    (((#(1) . 6) (#(0) . 7)) . 2))
		  (funcall (a-and-b) (empty-state)))))
