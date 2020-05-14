;;; #2
;;; Определите макрос (POP стек), который читает из стека верхний
;;; элемент и меняет значение переменной стека

(defmacro popp (stack)
    `(let
        ((,'temp 0))
        (cond
            ((psetq ,'temp (car ,stack) ,stack (cdr ,stack)))
            (t temp)
        )
    )
)

;;; Test #2
(princ "  Test for task #2")
(setq stack1 (list 1 2 3 4))
;(print (macroexpand '(popp stack1)))
(print stack1)
(print (popp stack1))
(print stack1)
(print (popp stack1))
(print stack1)
(write-line "")

;;; #3
;;; Определите лисповскую форму (IF условие p q) в виде макроса

(defmacro iff (q p n)
    `(cond
        (,q ,p)
        (,t ,n)
    )
)

;;; Test #3
(princ "  Test for task #3")
(print (iff (> 3 2) 1 2))
(print (iff (< 3 2) 1 2))
(write-line "")

;;; #4
;;; Определите ввиде макроса форму (FIF тест отр нуль полож).

(defmacro fif (test neg zero pos)
    `(cond
        ((< ,test 0) ,neg)
        ((> ,test 0) ,pos)
        (t ,zero)
    )
)

;;; Test #4
(princ "  Test for task #4")
(print (fif (- 0 10) 1 2 3))
(print (fif (+ 0 10) 1 2 3))
(print (fif (* 0 10) 1 2 3))
(write-line "")

;;; Task #5
;;; Определите ввиде макроса форму (REPEAT e UNTIL p) паскалевского типа
(defmacro repeatt (f untill c)
    `(let
        ((,'fwrap 0))
        (progn
            (setq ,'fwrap
                (lambda
                    nil
                    (progn
                        (funcall ,f)
                        (cond
                            (,c (funcall ,'fwrap))
                            (t nil)
                        )
                    )
                )
            )
            (funcall ,'fwrap)
        )
    )
)
(setq c1 5)
(setq f1 (lambda ()
    (progn
        (print c1)
        (setq c1 (- c1 1))
    )
))

;;; Test #5
(princ "  Test for task #5")
;(print (macroexpand '(repeatt f1 untill (> c1 0))))
(repeatt f1 untill (> c1 0))
(write-line "")
