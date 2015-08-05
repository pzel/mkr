(defmodule unit-mkr-tests
  (behaviour ltest-unit)
  (export all)
  (import
   (from mkr
         (call/fresh 1)
         (conj 2)
         (disj 2)
         (= 2))
   (from ltest
         (check-failed-assert 2)
         (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "mkr/include/mkr-bool.lfe")

(define (empty-state) (: mkr-user empty-state)) ;; DRY

(deftest call/fresh-simple-goal
  (is-equal '((((#(0) . 5)). 1))
            (funcall (call/fresh (lambda(q) (= q 5))) 
                     (empty-state))))

(deftest call/fresh-complex-goal
  (flet ((a-and-b ()
                  (conj (call/fresh (lambda(a) (= a 7)))
                        (call/fresh (lambda(b) (disj (= b 5)
                                                     (= b 6)))))))
        (is-equal '((((#(1) . 5) (#(0) . 7)) . 2)
                    (((#(1) . 6) (#(0) . 7)) . 2))
                  (funcall (a-and-b) (empty-state)))))

(deftest conj-fails-unless-all-goals-succeed
  (is-equal '()
            (funcall (call/fresh 
                      (lambda(q) (conj (= 2 q) (= 1 q)))) 
                     (empty-state))))

(deftest conj-succeeds-if-all-goals-succeed
  (is-equal '((((#(0) . 1)). 1))
            (funcall (call/fresh 
                      (lambda(q) (conj (= 1 q) (= 1 q)))) 
                     (empty-state))))

(deftest disj-fails-if-all-goals-fail
  (is-equal '()
            (funcall (call/fresh 
                      (lambda(q) (disj (= 1 1) (= 'a 'a)))) 
                     (empty-state))))

(deftest disj-succeeds-if-any-goals-succeed
  (is-equal '((((#(0) . a)). 1))
            (funcall (call/fresh 
                      (lambda(q) (disj (= 1 1) (= 'a q)))) 
                     (empty-state))))

(deftest andd-returns-last-non-falsey-value
  (is-equal 'a (andd 'true 'b 'a)))

(deftest andd-treats-the-empty-list-as-false
  (is-equal 'false (andd '() 'b 'a)))

(deftest orr-returns-first-non-falsey-value
  (is-equal 'a (orr '() '() 'false 'a 'b)))

(deftest orr-returns-false-on-all-falsey-values
  (is-equal 'false (orr '() '() 'false)))
