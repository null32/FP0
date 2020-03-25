;;; #1
;;; Определить funcall через apply
;;; (funcall '+ 1 2 3) == (funcall-2 '+ 1 2 3)

(defun funcall2 (&rest args)
        (apply (car args) (cdr args))
)

;;; Test #1
(write-line "Test 1")
(princ " >> ('+ 1 2 3)")
(print (funcall2 '+ 1 2 3))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> ('max 1 2 3)")
(print (funcall2 'max 1 2 3))
(write-line "")



(write-line "")
;;; #3
;;; Определить функцию (apl-apply f x), которая
;;; применяет каждую функцию из списка f к
;;; каждому соответствующему элементу списка x
;;; (apl-apply '(min max) '((1 2 3) (1 2 3))) => (1 3)
(defun apl-apply (funcs args)
        (cond
                ((and (null funcs) (null args)) nil)
                ((or (null funcs) (null args)) (throw 'invalid-argument "length of funcs and args does not match"))
                (t
                        (cons
                                (apply (car funcs) (car args))
                                (apl-apply (cdr funcs) (cdr args))
                        )
                )
        )
)

;;; Test #1
(write-line "Test 1")
(princ " >> (apl-apply '(min max) '((1 2 3) (1 2 3)))")
(print (apl-apply '(min max) '((1 2 3) (1 2 3))))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> (apl-apply '(min max) '((1 2 3) (1 2 3) nil))")
(print
        (catch 'invalid-argument
                (apl-apply '(min max) '((1 2 3) (1 2 3) nil))
        )
)
(write-line "")



(write-line "")
;;; #5
;;; Определить функциональный предикат (any pred list),
;;; который истенен, если pred истенен для хотя бы
;;; одного элемента списка list
;;; (any (lambda (x) (> x 3)) '(1 2 3)) => nil
;;; (any (lambda (x) (> x 3)) '(1 5 3)) => T

(defun any (pred list)
        (cond
                ((null list)
                        nil
                )
                ((funcall pred (car list))
                        t
                )
                (t
                        (any pred (cdr list))
                )
        )
)

;;; Test #1
(write-line "Test 1")
(princ " >> (any (lambda (x) (> x 3)) '(1 2 3))")
(print (any (lambda (x) (> x 3)) '(1 2 3)))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> (any (lambda (x) (> x 3)) '(1 5 3))")
(print (any (lambda (x) (> x 3)) '(1 5 3)))
(write-line "")



(write-line "")
;;; #7
;;; Определить фильтр (where pred list),
;;; удаляющий из списка list все елементы,
;;; которые не подходят под pred

(defun where (pred list)
        (cond
                ((null list) nil)
                ((funcall pred (car list))
                        (cons (car list) (where pred (cdr list)))
                )
                (t
                        (where pred (cdr list))
                )
        )
)

;;; Test #1
(write-line "Test 1")
(princ " >> (where (lambda (x) (> x 3)) '(1 3 5 7))")
(print (where (lambda (x) (> x 3)) '(1 3 5 7)))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> (where (lambda (x) (= (mod x 2) 0)) '(1 2 3 4))")
(print (where (lambda (x) (= (mod x 2) 0)) '(1 2 3 4)))
(write-line "")



(write-line "")
;;; #9
;;; Напишите генератор чисел Фибоначчи
;;; Что есть генератор?



(write-line "")
;;; #11
;;; Определить (multifun f x), который применяет
;;; каждую функцию списка f к списку x, и возвращает
;;; список, сформированный из результатов
;;; (multifun '(+ -) '(3 2 1)) => (6 0)

(defun multifun (f x)
        (cond
                ((null f) nil)
                (t
                        (cons
                                (apply (car f) x)
                                (multifun (cdr f) x)
                        )
                )
        )
)

;;; Test #1
(write-line "Test 1")
(princ " >> '(+ - *) '(4 3 2 1)")
(print (multifun '(+ - *) '(4 3 2 1)))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> '(max min) '(4 3 2 1)")
(print (multifun '(max min) '(4 3 2 1)))
(write-line "")



(write-line "")
;;; #13
;;; Определите функцию, которая возвращает своё определение
;;; ???



