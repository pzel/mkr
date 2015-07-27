(defmacro iff (a b c) `(if (or (== ,a '()) (== ,a 'false)) ,c ,b))
(defmacro orr 
  ((cons e es) `(iff ,e ,e (orr ,@es)))
  (() `'false))
(defmacro andd
  ((cons e '()) `(iff ,e ,e 'false))
  ((cons e es) `(iff ,e (andd ,@es) 'false))
  (() `'true))


