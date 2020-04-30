(defmacro iff (q p n)
	(list 'cond
		(list q p)
		(list t n)
	)
)

(print (iff (> 3 2) 1 2))
(print (iff (< 3 2) 1 2))


(defmacro fif (test neg zero pos)
	(list 'cond
		(list (list '< test 0) neg)
		(list (list '> test 0) pos)
		(list t zero)
	)
)

(print (fif (- 0 10) -1 0 1))
(print (fif (+ 0 10) -1 0 1))
(print (fif 0 -1 0 1))


