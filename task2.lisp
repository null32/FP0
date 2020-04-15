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
    (mapcar (lambda (f a) (apply f a)) funcs args)
)

;;; Test #1
(write-line "Test 1")
(princ " >> (apl-apply '(min max) '((1 2 3) (1 2 3)))")
(print (apl-apply '(min max) '((1 2 3) (1 2 3))))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " >> (apl-apply '(min max) '((1 2 3) (1 2 3) nil))")
(print (apl-apply '(min max) '((1 2 3) (1 2 3) nil)))
(write-line "")

;;; Test #3
(write-line "Test 3")
(princ " >> (apl-apply '(+ - *) '((1 2 3) (3 2 1) (2 4 6)))")
(print (apl-apply '(+ - *) '((1 2 3) (3 2 1) (2 4 6))))
(write-line "")



(write-line "")
;;; #5
;;; Определить функциональный предикат (any pred list),
;;; который истенен, если pred истенен для хотя бы
;;; одного элемента списка list
;;; (any (lambda (x) (> x 3)) '(1 2 3)) => nil
;;; (any (lambda (x) (> x 3)) '(1 5 3)) => T

(defun any (pred lst)
    (reduce (lambda (p x) (or p (funcall pred x))) lst :initial-value nil)
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
    (mapcan (lambda (x) (cond ((funcall pred x) (list x)) (t nil))) list)
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
(defun fib-gen ()
    (let
        ((a 1) (b 0) (temp 0))
        (lambda ()
            (progn
                (setq temp b)
                (setq b (+ a b))
                (setq a temp)
                b
            )
        )
    )
)

;;; Test #1
(write-line "Test 1")
(princ " first 5 elements of fib sequence")
(setq f1 (fib-gen))
(print (funcall f1))
(print (funcall f1))
(print (funcall f1))
(print (funcall f1))
(print (funcall f1))
(write-line "")



(write-line "")
;;; #11
;;; Определить (multifun f x), который применяет
;;; каждую функцию списка f к списку x, и возвращает
;;; список, сформированный из результатов
;;; (multifun '(+ -) '(3 2 1)) => (6 0)

(defun multifun (funcs lst)
    (mapcar (lambda (f) (apply f lst)) funcs)
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
(defun getme ()
    (symbol-function 'getme)
)

;;; Test #1
(write-line "Test 1")
(princ " call function normal way")
(print (getme))
(write-line "")

;;; Test #2
(write-line "Test 2")
(princ " recursively call function 3 times")
(print (funcall (funcall (getme))))
(write-line "")
