(defmacro zzz (g) `(lambda(s/c) (lambda() (funcall ,g s/c))))

(defmacro disj+
  ((cons g '()) `(zzz ,g))
  ((cons g gs) `(disj (zzz ,g) (disj+ ,@gs))))

(defmacro conj+
  ((cons g '()) `(zzz ,g))
  ((cons g gs) `(conj (zzz ,g) (conj+ ,@gs))))

