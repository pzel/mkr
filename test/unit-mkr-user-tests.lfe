(defmodule unit-mkr-user-tests
  (behaviour ltest-unit)
  (export all)
  (import
   (from mkr
         (call/fresh 1)
         (conj 2)
         (disj 2)
         (= 2))
   (from mkr-user
         (empty-state 0)
         (take-all 1)
         (take 2))
   (from ltest
         (check-failed-assert 2)
         (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "mkr/include/mkr-user.lfe")

(define (fives x) (disj (= 5 x) (zzz (fives x))))
(deftest call/fresh-recursive-goal
  (let [(res (funcall (call/fresh (lambda(q) (fives q)))
                      (empty-state)))]
    (is-equal '(((#(0) . 5)). 1) (car res))
    (is (is_function (cdr res)))))

(define (one-two-three q)
  (disj (= q 1)
        (disj (= q 2)
              (= q 3))))

(deftest take-n
  (is-equal '((((#(0) . 1)) . 1)
              (((#(0) . 2)) . 1)
              (((#(0) . 3)) . 1))
            (take 3
                  (funcall (call/fresh (lambda(q) (one-two-three q)))
                           (empty-state)))))

(deftest take-n-with-shorter-stream
  (is-equal '((((#(0) . 1)) . 1))
            (take 3
                  (funcall (call/fresh
                            (lambda(q) (conj (one-two-three q)
                                             (= 1 q))))
                           (empty-state)))))

(deftest take-all
  (is-equal '((((#(0) . 1)) . 1)
              (((#(0) . 2)) . 1)
              (((#(0) . 3)) . 1))
            (take-all
             (funcall (call/fresh (lambda(q) (one-two-three q)))
                      (empty-state)))))

(define (fives+ x) (disj+ (= 5 x) (fives+ x)))
(define (sixes+ x) (disj+ (= 6 x) (sixes+ x)))
(define (fives-and-sixes+ x)
  (disj+ (conj+ (fives+ x) (= x 5))
         (sixes+ x)))

(deftest disj+-snoozes-cdr
  (let ((res (funcall (funcall
                       (funcall (call/fresh (lambda(q) (fives+ q)))
                                (empty-state))))))
    (is-equal '(((#(0) . 5)). 1) (car res))
    (is (is_function (cdr res)))))

(deftest conj+-snoozes-and-interleaves
  (is-equal '((((#(0) . 6)) . 1)
              (((#(0) . 6)) . 1)
              (((#(0) . 5)) . 1))
            (take 3
                  (funcall (call/fresh
                            (lambda(q) (fives-and-sixes+ q)))
                           (empty-state)))))

(define (oneo n)
  (= 1 n))

(deftest conde-works
  (is-equal
   '(1 other)
   (run* (q)
         (conde
          ((oneo q) (= 1 q))
          ((= 2 q) (= 'two q)) ;; will fail
          ((= 'other q))))))

(deftest conde-supports-else-clause
  (is-equal
   '(else-worked)
   (run* (q)
         (conde
          ((oneo q)
           (= 'one q))
          ((= 2 q)
           (= 'two q))
          (else
           (= 'else-worked q))))))

(define (nothing q)
  (fresh () (= 'nothing q)))

(define (one-thing q)
  (fresh (x)
         (= x q)
         (= x 'one)))

(define (trees q)
  (fresh (a b c)
         (= a 'aspen)
         (= b 'birch)
         (= c 'cypress)))

(deftest nullary-fresh-works
  (is-equal '((((#(0) . nothing)) . 1))
            (take 1 (funcall (call/fresh (lambda(q) (nothing q)))
                             (empty-state)))))

(deftest unary-fresh-works
  (is-equal '((((#(0) . one) (#(1) . #(0))) . 2))
            (take 1 (funcall (call/fresh (lambda(q) (one-thing q)))
                             (empty-state)))))

(deftest trinary-fresh-works
  (is-equal '((((#(3) . cypress) (#(2) . birch) (#(1) . aspen) ) . 4))
            (take 1 (funcall (call/fresh (lambda(q) (trees q)))
                             (empty-state)))))


(deftest run-one-works
  (is-equal '(one)
            (run 1 (q) (= q 'one))))

(deftest run*-works
  (is-equal '(one two)
            (run* (q) (disj+
                       (= q 'one)
                       (= q 'two)
                       ))))

(deftest run-with-multiple-args
  (is-equal '(eh bee _.0 _.0)
            (run* (q a b)
                  (disj+
                   (conj+ (= a 'eh) (= q a))
                   (conj+ (= b 'bee) (= q b))
                   (conj+ (= b a) (= q a))
                   (conj+ (= q a))))))
