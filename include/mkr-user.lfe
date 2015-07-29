(defmacro zzz (g) `(lambda(s/c) (lambda() (funcall ,g s/c))))

(defmacro disj+
  ((cons g '()) `(zzz ,g))
  ((cons g gs) `(: mkr disj (zzz ,g) (disj+ ,@gs))))

(defmacro conj+
  ((cons g '()) `(zzz ,g))
  ((cons g gs) `(: mkr conj (zzz ,g) (conj+ ,@gs))))

(defmacro fresh 
  (e (cond
      ((== '() (car e))
       `(conj+ ,@(cdr e)))
      (else
       `(: mkr call/fresh (lambda (,(car (car e))) 
			    (fresh ,(cdr (car e)) ,@(cdr e))))))))

(defmacro run
  ((cons n goals)
   `(: mkr-user mK-reify
               (: mkr-user take ,n
                  (: mkr-user call/empty-state 
                     (fresh ,@goals))))))

(defmacro run*
  (goals
   `(: mkr-user mK-reify
               (: mkr-user take-all
                  (: mkr-user call/empty-state 
                     (fresh ,@goals))))))
