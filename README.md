# mkr

An implementation of µKanren in LFE.

### What is µKanren?

It is a tiny relational programming language, as defined in the <strike>super
fun tutorial</strike> serious scientific publication [µKanren: A Minimal
Functional Core for Relational
Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf) by
Jason Hemann and Daniel P. Friedman.


### Can I do the exercises from *The Reasoned Schemer* in `mkr`?

Yes! You may have to watch out, as `conde` actually interleaves streams,
like `condi` from *The Reasoned Schemer*. TODO: name things properly.


### Can I use the engine in my LFE project?

Yes! Also, if you don't like pain, be sure to include the user-level macros:
``` (include-lib "mkr/include/mkr-user.lfe") ```

This gives you access to:

  * `=` :  unification
  * `fresh` : a lambda-like form
  * `conde` : a cond-like form *(specifically: a disjunction of conjunctions)*
  * `run N` : top-level query with bounded result length
  * `run*`  : top-level query with unbounded result length

as well as these somewhat more low-level forms:

  * `conj+ ` : a variadic **conjunction** operator for goals
  * `disj+ ` : a variadic **disjunction** operator for goals
  * `zzz`  : a form to delay evaluations of recursive goals


### What can I do with it?

Play around a bit and then roll your own, most likely.
Alternately, try implementing
[something harder](http://webyrd.net/quines/quines.pdf) in it.


### Can I see some code, though?

Yes!

```
$ make repl
LFE Shell V7.0 (abort with ^G)
> (include-lib "mkr/include/mkr-user.lfe")
()

> (run* (q) (= '(a c o r n) q))
((a c o r n))

> (define (conso a d l)
    (= (cons a d) l))
conso

> (define (caro l a)
    (fresh (d) (= (cons a d) l)))
caro

> (run* (q) (caro '(a c o r n) q))
(a)

> (define (cdro l d)
    (fresh (a) (= (cons a d) l)))
cdro

> (run* (q) (cdro '(a c o r n) q))
((c o r n))

> (define (nullo l)
    (= '() l))
nullo

> (run* (q) (nullo q))
(())

> (run* (q) (nullo '()) (= 'true q))
(true)

> (define (eq-caro l x) 
    (fresh (a) (conde ((caro l a) (= x a)))))
eq-caro

> (run* (q) (eq-caro '(a b) 'b) (= 'true q))
()

> (run* (q) (eq-caro '(a b) q))
(a)

> (define (memo x l out)
    (conde ((eq-caro l x) (= l out))
           (else (fresh (d) 
                    (cdro l d) (memo x d out)))))

> (run 12 (out u) (memo 'tofu `(a b tofu d tofu e . ,out) u))
(_.0
 _.0
 (tofu . _.0)
 (_.0 tofu . _.1)
 (_.0 _.1 tofu . _.2)
 (_.0 _.1 _.2 tofu . _.3)
 (_.0 _.1 _.2 _.3 tofu . _.4)
 (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
 (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
 (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9))


```

### Mad props!

Thank you: Mr. Friedman, Mr. Byrd, Mr. Kiselyov, and Mr. Hemann!
See lots more at: [minikanren.org](http://minikanren.org)
