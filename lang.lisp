(defmacro foreach (lst do func)
	`(let
		((,'fwrap nil))
		(progn
			(setq ,'fwrap
				(lambda (l)
					(cond
						((null l) nil)
						(t
							(progn
								(,func (car l))
								(funcall ,'fwrap (cdr l))
							)
						)
					)
				)
			)
			(funcall ,'fwrap ,lst)
		)
	)
)

(defmacro join (lst with sep)
	`(cdr (mapcan (lambda (x) (list ,sep x)) ,lst))
)

(defmacro myprint (&rest args)
	`(foreach (nconc (join ',args with ", ") (list #\linefeed)) do princ)
)

(defmacro rangegen (&key (from 0) to (step 1))
	`(let
		((,'cur (- ,from ,step)))
		(lambda ()
			(cond
				((>= ,'cur ,to) nil)
				(t
					(setq ,'cur (+ ,'cur ,step))
				)
			)
		)
	)
)
